#' Reach Geomorphic Metrics
#'
#' @description Produces summary statistics and boxplots of reach-level geomorphic metrics for Tier 2 form units
#' and Tier 3 geomorphic units.  Data are summarized using 3 different groupings: All' (all data, un-grouped), 
#' 'RS' (by Reach Type), 'RSCond' (by Reach Type and Condition).  
#' 
#' @note Calls make.outputs function from functions.R script.
#' Outputs are written to 'Outputs/Assemblage/UnitForm/Reach' for Tier 2 form units and to 'Outputs/Assemblage/GU/Reach' 
#' for Tier 3 geomorphic units.
#'         
#' @param gu.type Geomorphic unit type passed as character.  Function expects either "UnitForm" or "GU".
#'
#' @export stats.csv Summary statistics csv file
#' @export Boxplots.* Boxplots of reach-level geomorphic units in both pdf and png format
#'
#' @examples
#' reach.assemblage.fn(gu.type = "UnitForm")
#' reach.assemblage.fn(gu.type = "GU")
reach.assemblage.fn = function(gu.type){
  
  # set directories  ------------------------------------------------------------------------
  
  # set assemblage output directory
  assemblage.dir = file.path(proj.dir, "Outputs/Assemblage", gu.type, "Reach")
  if(!file.exists(assemblage.dir)){dir.create(assemblage.dir, recursive = TRUE)}
  
  # delete any existing files in Output from previous runs
  unlink(file.path(assemblage.dir, "*"), recursive = TRUE)
  
  # set path to repo gut metric tables
  metrics.dir = file.path(repo.dir, "TrainingData/Metrics")
  
  # specify gut output layer and corresponding metric table to draw data from based on gu.type parameter
  if(gu.type == "GU"){
    layer = "Tier3_InChannel_GU" 
    gut.metrics = "Site_GUTMetrics_Tier3_InChannel_GU.csv"}
  if(gu.type == "UnitForm" | gu.type == "UnitShape"){
    layer = "Tier2_InChannel"
    gut.metrics = "Site_GUTMetrics_Tier2_InChannel.csv"}
  
  # Read in data and summarize ----------------------------------------------
  
  # read in site data
  site.metrics = read_csv(file.path(metrics.dir, gut.metrics)) 
  
  # change visit.id to VisitID for joining
  if('visit.id' %in% names(site.metrics)){site.metrics = site.metrics %>% rename(VisitID = visit.id)}
  
  # convert site metrics to long data format
  # add wet to bankfull area ratio
  site.metrics = site.metrics %>%
    select(-gut.layer) %>%
    mutate(wet.bf.area.ratio = round(wet.area.m2 / bf.area.m2, 3)) %>%
    gather(value = "value", key = "variable", -VisitID)
  
  # join site metrics and selections
  # add unit.type (here all units) and ROI 
  site.data = selections %>% 
    select(RS, Condition, VisitID) %>%
    inner_join(site.metrics, by = 'VisitID') %>%
    mutate(unit.type = "all", ROI = "bankfull")
  
  # re-set ROI to wetted for rows where variable is 'wet.area.m2'
  site.data = site.data %>%
    mutate(ROI = ifelse(variable == 'wet.area.m2', 'wetted', ROI))
  
  # summarize data pooling in different ways and write to files
  make.outputs(site.data, pool.by = "RSCond", out.dir = assemblage.dir, RSlevels = RSlevels)
  make.outputs(site.data, pool.by = "RS", out.dir = assemblage.dir, RSlevels = RSlevels)
  make.outputs(site.data, pool.by = "All", out.dir = assemblage.dir, RSlevels = RSlevels)
}

reach.assemblage.fn(gu.type = "UnitForm")
reach.assemblage.fn(gu.type = "GU")
