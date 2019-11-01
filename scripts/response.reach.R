#' Fish response by reach
#' 
#' @description Calculates summary of fish habitat models (fuzzy and NREI) by reach for each species and lifestage in the training dataset.  
#' Summary includes: 
#'    - model predicted fish count (for juveniles) or redd count (for spawners) written to 'pred.fish' folder
#'    - predicted fish or redd density (per meter) written to 'pred.fish.m' folder
#'    - predicted fish or redd density (per square meter) written to 'pred.fish.m2' folder 
#' Summary statistics include average, standard deviation, standard error, median, minimum, maximum, and sum total fish or redds
#' Data are summarized using 3 different groupings: All' (all data, un-grouped), RS' (by Reach Type), 'RSCond' (by Reach Type and Condition)
#' 
#' @note Outputs are written to 'Outputs/Reponse/Species/Lifestage/Reach' (e.g., 'Outputs/Reponse/Chinook/Juvenile/Reach')
#' 

# set directories and paths ------------------------------------------------------------------------

# set path to repo gut metric tables
metrics.dir = file.path(repo.dir, "TrainingData/Metrics")

# read in site metric fish data
site.fish.metrics = read_csv(file.path(metrics.dir, "Site_Fish_Metrics.csv"))

# check visit id column name and change if necessary to match selections 
if('visit.id' %in% names(site.fish.metrics)){site.fish.metrics = site.fish.metrics %>% rename(VisitID = visit.id)}

# get pairwise combinations of model (layer), species, and lifestage ---------------------------------------
model.df = site.fish.metrics %>%
  distinct(layer, species, lifestage) %>%
  filter(!is.na(species)) %>%
  as.data.frame()


#' calc.response.reach
#'
#' @description For pairwise combinations of each species x lifestage x model in reach-level fish metrics trainings data 
#' (i.e., Site_Fish_Metrics.csv), calculates fish or redds per meter (based on main thalewg length) and fish or redds
#' per square meter (for bankfull ROI uses bankfull area, for hydro ROI uses Delft3D modeled area).  Passes data to
#' make.outputs function to calculate summary statistics and create boxplots.
#'
#' @note Calls make.outputs function from functions.R script
#' 
#' @param model.df Tibble of pairwise combinations of each species x lifestage x model
#'
#' @export stats.csv Summary statistics of fish counts and densities csv file
#' @export Boxplots.* Boxplots of reach-level fish counts and densities in both pdf and png format
#' @export Response.csv Training data reach characteristics table with columns added for:
#'                      - model type (fuzzy, nrei)
#'                      - species (chinook, steelhead)
#'                      - lifestage (juvenile, spawner)
#'                      - ROI (hydro, bankfull)
#'                      - length.m (thalweg length)
#'                      - area.m2 (see above description)
#'                      - pred.n (redds or fish count)
#'                      - pred.m (redds or fish per meter)
#'                      - pred.m2 (redds or fish per square meter)
#'
calc.response.reach = function(model.df){
  
  in.species = model.df$species
  in.lifestage = model.df$lifestage
  in.model = model.df$layer
  
  print(str_c("Making site level fish response summaries and plots for", in.species, in.lifestage, sep = " "))
  
  # set response output directory
  # note: str_to_sentence will convert first letter of species and lifestage to upper case
  response.dir = file.path(proj.dir, "Outputs/Response", str_to_sentence(in.species), str_to_sentence(in.lifestage), "Reach")
  if(!file.exists(response.dir)){dir.create(response.dir, recursive = TRUE)}
  
  # create respons subdirectories for each prediction type
  if(!file.exists(file.path(response.dir, "pred.fish"))){dir.create(file.path(response.dir, "pred.fish"), recursive = TRUE)}
  if(!file.exists(file.path(response.dir, "pred.fish.m"))){dir.create(file.path(response.dir, "pred.fish.m"), recursive = TRUE)}
  if(!file.exists(file.path(response.dir, "pred.fish.m2"))){dir.create(file.path(response.dir, "pred.fish.m2"), recursive = TRUE)}
  
  # filter site level resonse to model, species and lifestage specified
  site.fish = site.fish.metrics %>% 
    filter(layer == in.model & species == in.species & lifestage == in.lifestage & var == "pred.fish")
  
  # make site summaries for different response and ROI -----------------------
  
  # predicted fish total in reach 
  pred.count.hydro = site.fish %>% 
    filter(category == "reach") %>% 
    mutate(ROI = "hydro") %>% 
    rename(pred.n = value) %>% 
    select(VisitID, pred.n, ROI)
  
  # reach main thalweg length (note - clipped to modeled extent)
  hydro.thalweg = site.fish.metrics %>%
    filter(layer == "thalweg", category == "main") %>%
    select(VisitID, value) %>%
    rename(length.m = value)
  
  # reach area (note - clipped to modeled extent)
  hydro.area = site.fish.metrics %>%
    filter(var == "area", category == "reach", layer == in.model, species == in.species, lifestage == in.lifestage) %>%
    select(VisitID, value) %>%
    rename(area.m2 = value)
  
  # join fish count, thalweg length, reach area and calculate fish densities
  pred.hydro = pred.count.hydro %>%
    left_join(hydro.thalweg, by = "VisitID") %>%
    left_join(hydro.area, by = "VisitID") %>%
    mutate(pred.m = pred.n / length.m) %>%
    mutate(pred.m2 = pred.n / area.m2) %>%
    select(VisitID, ROI, length.m, area.m2, everything())
  
  # computes density spread over entire bankfull area rather than hydro extent
  # note: this method is needed for for upscaling based on geomorphic units which area delineated and summarized within bankfull extent
  pred.bf = read_csv(file.path(metrics.dir, "Site_GUTMetrics_Tier2_InChannel.csv")) 
  
  # check visit id column name and change if necessary to match selections data
  if('visit.id' %in% names(pred.bf)){pred.bf = pred.bf %>% rename(VisitID = visit.id)}
  
  pred.bf = pred.bf %>%
    rename(area.m2 = bf.area.m2, length.m = main.thalweg.length.m) %>%
    select(VisitID, area.m2, length.m) %>%
    full_join(pred.count.hydro, by = "VisitID") %>%
    mutate(pred.m = pred.n / length.m, pred.m2 = pred.n / area.m2, ROI = "bankfull") 
  
  # combine bankfull and hydro (i.e., modeled wetted) predictions into single df
  # remove rows with pred.n of NA (denotes visits where model was not run)
  pred = rbind(pred.hydro, pred.bf)
  
  # join predictions to selections
  site.response = selections %>% 
    select(RS, Condition, VisitID) %>%
    inner_join(pred, by = 'VisitID') %>%
    mutate(unit.type = "NA") %>%
    filter(!is.na(pred.n))
  
  # convert to long format for use in make.outputs code
  site.response.long = site.response %>%
    gather(key = "variable", value = "value", pred.n, pred.m, pred.m2)
  
  # function to summarize data pooling in different ways and write to files
  make.all.outputs = function(){
    make.outputs(my.data, pool.by = "All", out.dir, my.facet = "ROI", RSlevels)
    make.outputs(my.data, pool.by = "RS", out.dir, my.facet = "ROI", RSlevels)
    make.outputs(my.data, pool.by = "RSCond", out.dir, my.facet = "ROI", RSlevels)
  }
  
  # make output for predicted fish count -----------------------
  
  # set output directory
  out.dir = file.path(response.dir, "pred.fish")
  # delete any existing files in Output from previous runs
  unlink(file.path(out.dir,"*"), recursive = TRUE)
  # set data
  my.data = site.response.long %>% filter(variable == "pred.n")
  # make summaries
  make.all.outputs()
  
  
  # make output for predicted fish density per length (m) -----------------------
  
  # set output directory
  out.dir = file.path(response.dir, "pred.fish.m")
  # delete any existing files in Output from previous runs
  unlink(file.path(out.dir,"*"), recursive = TRUE)
  # set data
  my.data = site.response.long %>% filter(variable == "pred.m")
  # make summaries
  make.all.outputs()
  
  
  # make output for predicted fish density per area (m2) -----------------------
  
  # set output directory
  out.dir = file.path(response.dir, "pred.fish.m2")
  # delete any existing files in Output from previous runs
  unlink(file.path(out.dir,"*"), recursive = TRUE)
  # set data
  my.data = site.response.long %>% filter(variable == "pred.m2")
  # make summaries
  make.all.outputs()
  

  # join selections and site response
  selections.site.response = selections %>%
    mutate(model = in.model, species = in.species, lifestage = in.lifestage) %>%
    inner_join(pred, by = "VisitID") 
  
  # write to file
  write_csv(selections.site.response, file.path(response.dir, "Response.csv"), col_names = TRUE)
  
}
  
# run reach response function for each species x lifestage x model
model.df %>% by_row(calc.response.reach)
