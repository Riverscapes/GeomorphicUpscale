
#This script takes the OUtput from GUT and summarizes responses by Unit


# To Do -------------------------------------------------------------------

# Nk todo
#eliminate species and model from directory structure and instead include as field in output table?  Maybe append to this table
#when run for different species or just run for all species and model types on the fly-- not make a user variable...?

# Dependencies ------------------------------------------------------------

require(tidyverse)
library(purrrlyr)

source(file.path(repo.dir, "scripts/plot.colors.R"))
source(file.path(repo.dir, "scripts/functions.R"))


# set directories and paths ------------------------------------------------------------------------


# set path to repo gut metric tables
metrics.dir = file.path(repo.dir, "Database/Metrics")

# read in unit metric fish data (for predicted fish per unit)
# specify gut output layer and corresponding metric table to draw data from based on gu.type parameter
if(gu.type == "GU"){
  unit.fish.metrics = read_csv(file.path(metrics.dir, "Unit_Fish_Metrics_Tier3_InChannel_GU_All.csv"))}
if(gu.type == "UnitForm" | gu.type == "UnitShape"){
  unit.fish.metrics = read_csv(file.path(metrics.dir, "Unit_Fish_Metrics_Tier2_InChannel_All.csv"))}


# check visit id column name and change if necessary to match selections 
if('visit.id' %in% names(unit.fish.metrics)){unit.fish.metrics = unit.fish.metrics %>% rename(VisitID = visit.id)}

# get pairwise combinations of model (layer), species, and lifestage ------------------------------------------------------------------------

model.df = unit.fish.metrics %>%
  distinct(model, species, lifestage) %>%
  filter(!is.na(species)) %>%
  as.data.frame()


# caclulate unit response function ------------------------------------------------------------------------

calc.response.unit = function(model.df){
  
  in.species = model.df$species
  in.lifestage = model.df$lifestage
  in.model = model.df$model
  
  # set response output directory
  # note: str_to_sentence will convert first letter of species and lifestage to upper case
  response.dir = file.path(proj.dir, "Outputs/Response", str_to_sentence(in.species), str_to_sentence(in.lifestage), "Unit", gu.type)
  if(!file.exists(response.dir)){dir.create(response.dir, recursive = TRUE)}
  
  # delete any existing files in output directory from previous runs
  unlink(file.path(response.dir, "*"), recursive = TRUE)
  
  # filter unit level resonse to model, species and lifestage specified
  unit.fish = unit.fish.metrics %>% 
    filter(model == in.model & species == in.species & lifestage == in.lifestage)
  
  # remove units where pred.fish is NA
  # these are units where there was no overlap between fish model extent and the unit
  unit.fish = unit.fish %>% filter(!is.na(pred.fish))
  
  # add fields for fish density (m2) and ratio of suitable habitat area to hydro model area
  # subset columns for plotting
  unit.fish = unit.fish %>%
    mutate(density.m2 = round(pred.fish / area.unit, 3), 
           suitable.area.ratio = round(hab.area.suitable / area.delft, 3)) %>%
    rename(unit.type = gu.type) %>%
    select(-area.sum, -hab.area.suitable)
  
  # convert to long form tibble 
  unit.fish.long = unit.fish %>%
    gather(key = "variable", value = "value", -VisitID, -UnitID, -unit.type, -model, -species, -lifestage)
  
  # join to selections
  # remove rows where 'value' is na (these are visits where model was not run)
  unit.response = selections %>% 
    select(RS, Condition, VisitID) %>%
    inner_join(unit.fish.long, by = 'VisitID') %>%
    filter(!is.na(value)) %>%
    mutate(ROI = "hydro",
           variable = factor(variable, levels = c('pred.fish', 'density.m2', 'n.units',
                                                  'area.delft', 'area.unit', 'suitable.area.ratio', 
                                                  'med', 'mean', 'sd', 
                                                  'med.suitable', 'mean.suitable', 'sd.suitable'))) 
  
  
  # summarizes data for use in upscale ----------------------------------------------------
  
  print("Making unit level fish response summaries and plots...")
  make.outputs.unit(in.data = unit.response, pool.by = "RSCond", gu.type = gu.type, out.dir = response.dir, RSlevels = RSlevels)
  make.outputs.unit(in.data = unit.response, pool.by = "RS", gu.type = gu.type, out.dir = response.dir, RSlevels = RSlevels)
  make.outputs.unit(in.data = unit.response, pool.by = "All", gu.type = gu.type, out.dir = response.dir, RSlevels = RSlevels)
  
}


# run reach response function for each species x lifestage x model
model.df %>% by_row(calc.response.unit)