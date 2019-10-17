#Copy maps to new directories to quickly verify subsetted data as good representations for desired 
#river style and condition-------------------------------------------------------------


copy.maps = function(df, pool.by, id.name){
  
  # get id.name (i.e., VisitID)
  id = df %>% select(!!as.name(id.name)) %>% pull()
  
  # get figure subdirectory
  if(pool.by == 'RS'){sub.dir = df %>% select(RS) %>% pull()}
  if(pool.by == 'RSCond'){sub.dir = df %>% select(RS, Condition) %>% mutate(sub.folder = str_c(RS, Condition, sep = "/")) %>% pull(sub.folder)}
  
  from.fig.path = unlist(list.files(path = repo.maps.dir, pattern = paste('Visit_', as.character(id), '_pyGUT_Map.png', sep = ''), full.names = TRUE, recursive = FALSE, include.dirs = FALSE))
  to.fig.path = file.path(selection.maps.dir, sub.dir, basename(from.fig.path))
  
  file.copy(from.fig.path, to.fig.path)

}
  

selection.maps = function(id.name, pool.by){ 
  
  # get unique pool.by factor levels
  if(pool.by == 'RS'){rs.levels = selections %>% select(RS) %>% unique() %>% pull()}
  if(pool.by == 'RSCond'){rs.levels = selections %>% select(RS, Condition) %>% unique() %>% mutate(sub.folder = str_c(RS, Condition, sep = "/")) %>% pull(sub.folder)}
  
  # create map subdirectory for unique pool.by factor levels
  sapply(rs.levels, function(x) if(!dir.exists(file.path(selection.maps.dir, x))) dir.create(file.path(selection.maps.dir, x)))
  
  by_row(selections, copy.maps, pool.by = pool.by, id.name = id.name)
   
}