#' Geoindicator summary
#' 
#' @description Calculates geonindicator summary for RS selections.  
#' Geoindicators are filtered by user-defined vector in UpscaleWrapper.R script.
#' Summary statistics include average, standard deviation, standard error, median, minimum, maximum, and sum for each numeric geoindicator
#' Data are summarized using 3 different groupings: All' (all data, un-grouped), RS' (by Reach Type), 'RSCond' (by Reach Type and Condition)
#' 
#' @note Outputs are written to 'Outputs/Selections/Geoindicators'.  
#'       Summaries and plots can only be created for numeric data.  Non-numeric data is filtered out prior to generating summary stats and boxplots.
#'       
#' @export stats.csv Summary statistics csv file
#' @export Boxplots.* Boxplots of geoindicators in both pdf and png format
#' 


# set up data refs and output file structure  ------------------------------------------------------------------------

# set and create geoindicator output directory
geo.dir = file.path(proj.dir, "Outputs/Selections/Geoindicators")
if(!file.exists(geo.dir)){dir.create(geo.dir, recursive = TRUE)}

# delete any existing files in Output from previous runs
unlink(file.path(geo.dir, "*"), recursive = TRUE)

# specify location of input metric tables
metrics.dir = file.path(repo.dir,"TrainingData", "Metrics")

# get just numerical columns for boxplots (in addition to RS, Condition, VisitID)
key.cols = selections %>% select(VisitID, RS, Condition)
numeric.cols = selections %>% select_if(~is.numeric(.)) %>% select(-VisitID)

# create long format for selections of quatitative data
geoindicators.df = bind_cols(key.cols, numeric.cols) %>%
  gather(value = "value", key = "variable", -RS, -Condition, -VisitID) %>%
  mutate(unit.type = NA, ROI = "NA")

# filter to user supplied geoindicators (if specified)
if(any(!is.na(geoindicators))){
  geoindicators.df = geoindicators.df %>%
    filter(variable %in% geoindicators) %>%
    mutate(value = as.numeric(value))
}


# calculate geoindicator summaries and make boxplots  -------------------------------------

print("Calculating geoindicator summary statistics and generating boxplots...")

# #summarized by All
make.outputs(geoindicators.df, pool.by = "All", out.dir = geo.dir, RSlevels = RSlevels)
# 
# #summarized by River Style (RS)
make.outputs(geoindicators.df, pool.by = "RS", out.dir = geo.dir, RSlevels = RSlevels)

#summarized by RS and Condition
make.outputs(geoindicators.df, pool.by = "RSCond", out.dir = geo.dir, RSlevels = RSlevels)
