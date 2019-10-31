#Upcales reponse by unit and river style to a network



# Dependencies ------------------------------------------------------------

require(tidyverse)

source(file.path(repo.dir, "scripts/plot.colors.R"))
source(file.path(repo.dir, "scripts/functions.R"))


# set up data refs and output file structure  ------------------------------------------------------------------------
print("setting up data refs and output file structure...")

in.species = species
in.lifestage = lifestage
in.model = model

# set directories and paths ------------------------------------------------------------------------

# set response input directory
# note: str_to_sentence will convert first letter of species and lifestage to upper case
response.dir = file.path(proj.dir, "Outputs/Response", str_to_sentence(in.species), str_to_sentence(in.lifestage), "Unit", gu.type, response.pool)

# set gut assemblage directory
assemblage.dir = file.path(proj.dir, "Outputs/Assemblage", gu.type, "Unit", response.pool)

# create upscale output directory
upscale.dir = file.path(proj.dir, "Outputs/Upscale", str_to_sentence(in.species), str_to_sentence(in.lifestage), "Unit", gu.type, response.pool)
if(!file.exists(upscale.dir)){dir.create(upscale.dir, recursive = TRUE)}

# delete any existing files in output directory from previous runs
unlink(file.path(upscale.dir, "*"), recursive = TRUE)


# Read in and restructure data----------------------------------------------------

# Read in response variable by response.pool variable

# read in response stats csv
response = read_csv(file.path(response.dir, "stats.csv"))

# get density (fish per m2) and density se from response stats
response.density = response %>%
  filter(variable == "density.m2", ROI == "hydro") %>%
  select(RS, Condition, unit.type, avg, se) %>%
  rename(density.m2 = avg, density.m2.se = se)

# read in assemblage statistics
gu.stats = read_csv(file.path(assemblage.dir, "stats.csv"))

# get bankfull area ratio value and se for each unit type
# note: area ratio for unit of type (i) = sum(areas for unit type (i)) / sum(areas for all units)
gu.assemblage.se = gu.stats %>% 
  filter(variable == "area.ratio", ROI == "bankfull") %>%
  select(RS, Condition, unit.type, se) %>%
  rename(area.ratio.se = se)

# read in assemblage area ratios
assemblage = read_csv(file.path(assemblage.dir, "assemblage.csv")) %>%
  select(-SUM)

# convert assemblage area ratios to long format

gu.levels = gu.stats %>% distinct(unit.type) %>% nrow()

gu.assemblage = assemblage %>%
  gather(key = "unit.type", value = "area.ratio", (ncol(assemblage) - gu.levels + 1):ncol(assemblage)) %>%
  select(RS, Condition, unit.type, area.ratio) %>%
  mutate(area.ratio = as.numeric(area.ratio)) %>%
  left_join(gu.assemblage.se, by = c('RS', 'Condition', 'unit.type'))


# assembling upscale data----------------------------------------------------

# set join columns based on response.pool
if(response.pool == "byRSCond"){join.by = c("RS", "Condition", "unit.type")}
if(response.pool == "byRS"){join.by = c("RS", "unit.type")}
if(response.pool == "byAll"){join.by = c("unit.type")}

# Combining response and assemblage data

upscale.response = gu.assemblage %>%
  left_join(response.density, by = join.by) %>%
  select(RS, Condition, unit.type, area.ratio, area.ratio.se, density.m2, density.m2.se) %>%
  mutate(ROI = "bankfull")


# Upscales response on the network for different scenarios ------------------------------

print("upscaling response on the network for different condition senarios")

upscale.fn = function(cond.col){
  
  if(any(is.na(area.col), str_length(area.col) == 0)){
    # Estimate area based on condition and braid.index if not supplied by the user
    braid.c = braid.index %>% select(RS, Condition, C)
    upscale.network = network %>% 
      select(!!seg.id.col, RS, !!cond.col, !!length.col, !!width.col)  
    names(upscale.network) = c(seg.id.col, "RS", "Condition", "reach.length", "reach.width")
    upscale.network = upscale.network %>% 
      left_join(braid.c, by = c("RS", "Condition")) %>%
      mutate(reach.area = reach.length * reach.width * C, 
             area.method = "estimated")
  }else{
    upscale.network = network %>% 
      select(!!seg.id.col, RS, !!cond.col, !!length.col, !!width.col, !!area.col) %>%
      mutate(area.method = "given")
    names(upscale.network) = c(seg.id.col, "RS", "Condition", "reach.length", "reach.width", "reach.area", "area.method")
  }
  
  # calculate reach.braid metric 
  upscale.network = upscale.network %>% 
    mutate(reach.braid = reach.area / reach.length / reach.width)

  # combine upscale network segments with response (tied to RS and condition specified on network)
  upscale.network.response = upscale.network %>%
    inner_join(upscale.response, by = c("RS", "Condition"))
  
  # upscale density to predicted count value and propagate standard error
  upscale.network.response = upscale.network.response %>%
    mutate(n = area.ratio * reach.area * density.m2,
           n.se = n * sqrt((area.ratio.se / area.ratio)**2) + (density.m2.se / density.m2))
  
  # calculate total predicted fish, propagated standard error and density for each network segment
  # add column for scenario
  upscale.network.response.final = upscale.network.response %>%
    group_by_at(vars(seg.id.col, 'RS', 'Condition')) %>% 
    summarize(pred.n = sum(n, na.rm = TRUE), pred.n.se = sqrt(sum(n.se**2, na.rm = TRUE))) %>%
    ungroup() %>%
    left_join(upscale.network %>% select(-RS, -Condition), by = seg.id.col) %>%
    mutate(Scenario = cond.col,
           pred.m2 = pred.n / reach.area) %>%
    select(!!seg.id.col, RS, Condition, reach.length, reach.width, reach.area, area.method, reach.braid, everything())
 
  return(upscale.network.response.final)
}

# run reach upscale function for each condition scenario
reach.upscale = map_dfr(cond.cols, upscale.fn)

# add additional fields
reach.upscale = reach.upscale %>%
  mutate(species = species,
         model = model,
         lifestage = lifestage,
         response.pool = response.pool,
         gu.type = gu.type)


# results summar function
reach.summary = function(grouped.data){
  grouped.data %>% summarize(value = sum(value, na.rm = TRUE), 
                          value.se = sqrt(sum(value.se**2, na.rm = TRUE)),
                          sum.area = sum(reach.area, na.rm = TRUE),
                          sum.length = sum(reach.length, na.rm = TRUE),
                          mean.width = mean(reach.width, na.rm = TRUE),
                          sd.width = sd(reach.width, na.rm = TRUE),
                          mean.braid = mean(reach.braid, na.rm = TRUE),
                          sd.braid = sd(reach.braid, na.rm = TRUE))
}

# result summary by Scenario and RS and Condition
basin.upscale.RSCond = reach.upscale %>% 
  rename(value = pred.n, value.se = pred.n.se) %>%
  mutate(variable = "pred.fish") %>%
  group_by(Scenario, RS, Condition, area.method, model, species, lifestage, variable, response.pool, gu.type) %>% 
  reach.summary()

# result summary by Scenario and RS and Condition
basin.upscale.RS = reach.upscale %>% 
  rename(value = pred.n, value.se = pred.n.se) %>%
  mutate(variable = "pred.fish") %>%
  group_by(Scenario, RS, area.method, model, species, lifestage, variable, response.pool, gu.type) %>%
  reach.summary()

# result summary by Scenario 
basin.upscale = reach.upscale %>%
  rename(value = pred.n, value.se = pred.n.se) %>%
  mutate(variable = "pred.fish") %>%
  group_by(Scenario, area.method, model, species, lifestage, variable, response.pool, gu.type) %>%
  reach.summary()

#write output to file
write_csv(reach.upscale, file.path(upscale.dir, "byReach.csv"), col_names = TRUE)
write_csv(basin.upscale, file.path(upscale.dir, "byBasin.csv"), col_names = TRUE)
write_csv(basin.upscale.RS, file.path(upscale.dir, "byBasinRS.csv"), col_names = TRUE)
write_csv(basin.upscale.RSCond, file.path(upscale.dir, "byBasinRSCond.csv"), col_names = TRUE)

