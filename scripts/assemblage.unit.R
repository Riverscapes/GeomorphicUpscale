#' Geomorphic Unit Assemblage
#'
#' @description Produces summary statistics and boxplots of unit-level geomorphic metrics for Tier 2 form units
#' and Tier 3 geomorphic units.  Data are summarized using 3 different groupings: All' (all data, un-grouped), 
#' 'RS' (by Reach Type), 'RSCond' (by Reach Type and Condition).  Also calculates assemblages of geomorphic units as 
#' ratio of unit area to bankfull area for each unit type. 
#' 
#' @note Calls make.outputs.unit function and assemblage.proportions function from functions.R script.
#' Outputs are written to 'Outputs/Assemblage/UnitForm/Unit' for Tier 2 form units and to 'Outputs/Assemblage/GU/Unit' 
#' for Tier 3 geomorphic units.
#'  
#' @param gu.type Geomorphic unit type passed as character.  Function expects either "UnitForm" or "GU".
#'
#' @export stats.csv Summary statistics csv file
#' @export Boxplots.* Boxplots of unit-level geomorphic units in both pdf and png format
#' @export assemblage.csv Area ratios for each unit type
#' @export assemblage.* Barplots of are ratios for each unit type in both pdf and png format 
#'
#' @examples
#' unit.assemblage.fn(gu.type = "UnitForm")
#' unit.assemblage.fn(gu.type = "GU")
unit.assemblage.fn = function(gu.type){
  
  print(str_c('Calculating geomorphic unit assemblage for', gu.type, sep = " "))
  
  # set directories and paths ------------------------------------------------------------------------
  
  # set assemblage output directory
  assemblage.dir = file.path(proj.dir, "Outputs/Assemblage", gu.type, "Unit")
  if(!file.exists(assemblage.dir)){dir.create(assemblage.dir, recursive = TRUE)}
  
  # set path to repo gut metric tables
  metrics.dir = file.path(repo.dir, "TrainingData/Metrics")
  
  # specify gut output layer and corresponding metric table to draw data from based on gu.type parameter
  if(gu.type == "GU"){
    layer = "Tier3_InChannel_GU" 
    gut.metrics = "Unit_GUTMetrics_Tier3_InChannel_GU.csv"}
  if(gu.type == "UnitForm" | gu.type == "UnitShape"){
    layer = "Tier2_InChannel"
    gut.metrics = "Unit_GUTMetrics_Tier2_InChannel.csv"}
  
  
  # read in and prep unit data ----------------------------------------------------------
  
  # read in unit data
  unit.metrics = read_csv(file.path(metrics.dir, gut.metrics))
  
  # change visit.id to VisitID for joining
  if('visit.id' %in% names(unit.metrics)){unit.metrics = unit.metrics %>% rename(VisitID = visit.id)}
  
  # convert unit data to long format
  unit.metrics = unit.metrics %>%
      select(-gut.layer)%>%
      gather(value = "value", key = "variable", -VisitID, -unit.type) %>%
      mutate(ROI = "bankfull")
    
  # combine Unit data with selections
  unit.data = selections %>% 
    select(RS, Condition, VisitID) %>%
    inner_join(unit.metrics, by = 'VisitID') %>%
    filter(!is.na(unit.type))
    
  # calculate assemblage statistics ----------------------------------------------
  
  # summarize variables in GUT summary
  stats.RSCond = make.outputs.unit(unit.data, pool.by = "RSCond", gu.type = gu.type, out.dir = assemblage.dir, RSlevels = RSlevels)
  stats.RS = make.outputs.unit(unit.data, pool.by = "RS", gu.type = gu.type, out.dir = assemblage.dir, RSlevels = RSlevels)
  stats = make.outputs.unit(unit.data, pool.by = "All", gu.type = gu.type, out.dir = assemblage.dir, RSlevels = RSlevels)
  
  # writes assemblages and creates plots based on specified pool.by
  assemblage.proportions(stats.RSCond, pool.by = "RSCond", out.dir = assemblage.dir, gu.type = gu.type)
  assemblage.proportions(stats.RS, pool.by = "RS", out.dir = assemblage.dir, gu.type = gu.type)
  assemblage.proportions(stats, pool.by = "All", out.dir = assemblage.dir, gu.type = gu.type)

}

unit.assemblage.fn(gu.type = "UnitForm")
unit.assemblage.fn(gu.type = "GU")

  
  
