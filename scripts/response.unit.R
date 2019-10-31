#' Fish response by unit
#' 
#' @description Calculates summary of fish habitat models (fuzzy and NREI) by Tier 2 unit form and Tier 3 geomorphic unit for each
#' species and lifestage in the training dataset.  Summary includes model predicted fish count (for juveniles) or 
#' redd count (for spawners), predicted fish or redd density (per square meter) and summary statistics of fish habitat model values
#' for the reach and within the suitable area portion of the reach.  Data are summarized using 3 different groupings: 
#' All' (all data, un-grouped), RS' (by Reach Type), 'RSCond' (by Reach Type and Condition)
#' 
#' @note Outputs are written to 'Outputs/Reponse/Species/Lifestage/Unit/UnitType' (e.g., 'Outputs/Reponse/Chinook/Juvenile/Unit/GU')


#' response.unit.fn
#'
#' @description Reads in unit fish metrics training data (e.g., Unit_Fish_Metrics_Tier3_InChannel_GU_All.csv).
#' Gets pairwise combinations of each species x lifestage x model and passes each row to calc.response.unit function.
#'
#' @note Calls calc.response.unit function
#'
#' @param gu.type Geomorphic unit type passed as character.  Function expects either "UnitForm" or "GU".
#' 
response.unit.fn = function(gu.type){

  print(str_c('Calculating fish response by geomorphic unit for', gu.type, sep = " "))
  
  # set directories and paths ------------------------------------------------------------------------
  
  # set path to repo gut metric tables
  metrics.dir = file.path(repo.dir, "TrainingData/Metrics")
  
  # read in unit metric fish data (for predicted fish per unit)
  # specify gut output layer and corresponding metric table to draw data from based on gu.type parameter
  if(gu.type == "GU"){
    unit.fish.metrics = read_csv(file.path(metrics.dir, "Unit_Fish_Metrics_Tier3_InChannel_GU_All.csv")) %>%
      filter(!is.na(GU))}
  if(gu.type == "UnitForm" | gu.type == "UnitShape"){
    unit.fish.metrics = read_csv(file.path(metrics.dir, "Unit_Fish_Metrics_Tier2_InChannel_All.csv")) %>%
      filter(!is.na(UnitForm))}
  
  # check visit id column name and change if necessary to match selections 
  if('visit.id' %in% names(unit.fish.metrics)){unit.fish.metrics = unit.fish.metrics %>% rename(VisitID = visit.id)}
  
  # get pairwise combinations of model (layer), species, and lifestage -----------------------------------
  model.df = unit.fish.metrics %>%
    distinct(model, species, lifestage) %>%
    filter(!is.na(species)) %>%
    as.data.frame()

  # run reach calculate response function for each pariwise combination of species x lifestage x model
  model.df %>% by_row(calc.response.unit, unit.fish.metrics = unit.fish.metrics, gu.type = gu.type)
  
}


#' calc.response.unit
#'
#' @description Filters unit fish metrics training data to specific species x lifestage x model.  Calculates
#' fish or redd density and passes subsetted tibble to make.outputs.unit function.
#' 
#' @note Parameters passed from response.unit.fn function.  Calls make.outputs.unit from functions.R script.
#'
#' @param model.df Individual row from tibble of each pariwise combination of species x lifestage x model
#' @param unit.fish.metrics Unit fish metrics training data (e.g., Unit_Fish_Metrics_Tier3_InChannel_GU_All.csv).
#' @param gu.type Geomorphic unit type passed as character.  Function expects either "UnitForm" or "GU".
#'
#' @export stats.csv Summary statistics csv file
#' @export Boxplots.* Boxplots of unit-level fish metrics in both pdf and png format
#' 
calc.response.unit = function(model.df, unit.fish.metrics, gu.type){
  
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
  
  # add fields for 
  #   - fish density (m2)
  #   - ratio of suitable habitat area to hydro model area
  #   - number of units (needed for unit count in grouping summary)
  unit.fish = unit.fish %>%
    mutate(density.m2 = round(pred.fish / area.unit, 3), 
           suitable.area.ratio = round(hab.area.suitable / area.delft, 3),
           n.units = 1) %>%
    rename(unit.type = gu.type) 
  
  # convert to long form tibble 
  unit.fish.long = unit.fish %>%
    gather(key = "variable", value = "value", -VisitID, -UnitID, -unit.type, -model, -species, -lifestage)
  
  # join to selections
  # remove rows where 'value' is na (these are visits where model was not run)
  unit.response = selections %>% 
    select(RS, Condition, VisitID) %>%
    inner_join(unit.fish.long, by = 'VisitID') %>%
    mutate(ROI = "hydro",
           variable = factor(variable, levels = c('pred.fish', 'density.m2', 'n.units',
                                                  'area.delft', 'area.unit', 'hab.area.suitable', 'suitable.area.ratio', 
                                                  'med', 'mean', 'sd', 
                                                  'med.suitable', 'mean.suitable', 'sd.suitable'))) 
  
  
  # summarizes data for use in upscale ----------------------------------------------------
  
  make.outputs.unit(in.data = unit.response, pool.by = "RSCond", gu.type = gu.type, out.dir = response.dir, RSlevels = RSlevels)
  make.outputs.unit(in.data = unit.response, pool.by = "RS", gu.type = gu.type, out.dir = response.dir, RSlevels = RSlevels)
  make.outputs.unit(in.data = unit.response, pool.by = "All", gu.type = gu.type, out.dir = response.dir, RSlevels = RSlevels)
  
}

response.unit.fn(gu.type = "UnitForm")
response.unit.fn(gu.type = "GU")
