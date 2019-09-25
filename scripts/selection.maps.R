#Copy maps to new directories to quickly verify subsetted data as good representations for desired 
#river style and condition-------------------------------------------------------------


copy.maps = function(df, cat.field, id.name){
  
  # get id.name (i.e., VisitID)
  id = df %>% select(!!as.name(id.name)) %>% pull()
  
  # get id.name (i.e., VisitID)
  cat.name = df %>% select(!!as.name(cat.field)) %>% pull()
  
  from.fig.path = unlist(list.files(path = repo.maps.dir, pattern = paste('Visit_', as.character(id), '_pyGUT_Map.png', sep = ''), full.names = TRUE, recursive = FALSE, include.dirs = FALSE))
  to.fig.path = file.path(selection.maps.dir, cat.name, basename(from.fig.path))
  
  file.copy(from.fig.path, to.fig.path)

}
  

selection.maps = function(id.name, pool.by){ 
  
  # get unique pool.by factor levels
  rs.levels = selections %>% select(!!as.name(pool.by)) %>% unique() %>% pull()
  
  # create map subdirectory for unique pool.by factor levels
  sapply(rs.levels, function(x) if(!dir.exists(file.path(selection.maps.dir, x))) dir.create(file.path(selection.maps.dir, x)))
  
  by_row(selections, copy.maps, cat.field = pool.by, id.name = id.name)
   
}