#' Selection maps
#'
#' @description Copies GUT output maps from 'TrainingData/Maps' to RS selection folders to help
#' verify if selection criteria is a good representation of desired River Style and Condition
#' 
#' @note Called by RSelection.R script. Outputs are written to 'Outputs/Selections/Maps'  
#' 


#' copy.maps
#' 
#' @description Copies GUT output maps to RS x Condition folder
#' 
#' @note Fuction parameters passed from selections.map function
#' 
#' @param df Input row from selections tibble
#' @param pool.by 'RS' or 'RSCond'
#' @param id.name Unique ID (VisitID) to match selection tibble row with corresponding map
#' 
copy.maps = function(df, pool.by, id.name){
  
  # get id.name (i.e., VisitID)
  id = df %>% select(!!as.name(id.name)) %>% pull()
  
  # get figure subdirectory
  if(pool.by == 'RS'){sub.dir = df %>% select(RS) %>% pull()}
  if(pool.by == 'RSCond'){sub.dir = df %>% select(RS, Condition) %>% mutate(sub.folder = str_c(RS, Condition, sep = "/")) %>% pull(sub.folder)}
  
  # set copy from and copy to filepaths
  from.fig.path = unlist(list.files(path = repo.maps.dir, pattern = paste('Visit_', as.character(id), '_pyGUT_Map.png', sep = ''), full.names = TRUE, recursive = FALSE, include.dirs = FALSE))
  to.fig.path = file.path(selection.maps.dir, sub.dir, basename(from.fig.path))
  
  # call file.copy function to copy figure
  file.copy(from.fig.path, to.fig.path)

}
  

#' selection.maps
#'
#' @description Creates subfolders for each RS x Condition to copy maps into. Calls copy.maps function to copy maps for each row in selections tibble.
#'
#' @param id.name Unique ID (VisitID) to match selection tibble row with corresponding map
#' @param pool.by 'RS' or 'RSCond'
#' 
selection.maps = function(id.name, pool.by){ 
  
  # get unique pool.by factor levels
  if(pool.by == 'RS'){rs.levels = selections %>% select(RS) %>% unique() %>% pull()}
  if(pool.by == 'RSCond'){rs.levels = selections %>% select(RS, Condition) %>% unique() %>% mutate(sub.folder = str_c(RS, Condition, sep = "/")) %>% pull(sub.folder)}
  
  # create map subdirectory for unique pool.by factor levels
  sapply(rs.levels, function(x) if(!dir.exists(file.path(selection.maps.dir, x))) dir.create(file.path(selection.maps.dir, x)))
  
  # run copy.maps function for each row in the selections tibble
  by_row(selections, copy.maps, pool.by = pool.by, id.name = id.name)
   
}