#' Supporting functions
#' 
#' @description Set of supporting functions called by other scripts


#' summarize.f
#' 
#' @description Generic summary statistics function
#'
#' @param data Input tibble. Tibble should be grouped prior to calling function if want summary by group 
#' @param value Value column to summarize
#'
#' @return Tibble with value count, mean, standard deviation, standard error, median, minumum, maximum and sum total
#'
summarize.f = function(data, value){
  
  data.summary = data %>%
    summarize(n = length(na.omit(value)),
              avg = mean(value, na.rm = TRUE), 
              sd = sd(value, na.rm = TRUE),
              se = sd / sqrt(n),
              med = median(value, na.rm = TRUE), 
              min = min(value, na.rm = TRUE),
              max = max(value, na.rm = TRUE),
              sum = sum(value, na.rm = TRUE))
  
  return(data.summary)
}


#' make.summary
#' 
#' @description Groups input data according to 'pool.by' parameter and passes grouped dataframe to summarize.f function.
#'
#' @param in.data Input tibble.  Must be in long format with value, variable, RS, Condition, unit.type and ROI fields
#' @param pool.by How data should be grouped.  Options: 'RS', 'RSCond', 'All'
#' @param out.dir Path to output directory where summary statistics csv will be saved
#'
#' @return Tibble with summary statistics
#' @export stats.csv Summary statistics csv file written to output directory
#'
make.summary = function(in.data, pool.by, out.dir){
  
  #pool results by group specified
  if(pool.by == "RS"){
    group.data = in.data %>% 
      group_by(RS, unit.type, variable, ROI) %>% 
      select(-Condition) %>% 
      distinct()
  }
  
  if(pool.by == "RSCond"){
    group.data = in.data %>% 
      group_by(RS, Condition, unit.type, variable, ROI)
  }

  if(pool.by == "All"){
    group.data = in.data %>% group_by(unit.type, variable, ROI) %>% 
      select(-RS,-Condition) %>% 
      distinct()
  }
  
  # summarize variable with count, mean, max, median and sd.
  out.data = summarize.f(group.data, value)
  write_csv(out.data, file.path(out.dir ,"stats.csv"))
  return(out.data)
}


#' make.outputs
#'
#' @description Generic function that calls make.summary and summarize.f functions to calculate summary statistcs and create bar and boxplots
#'
#' @param in.data Input tibble.  Must be in long format with value, variable, RS, Condition, unit.type and ROI fields
#' @param pool.by How data should be grouped.  Options: 'RS', 'RSCond', 'All'
#' @param out.dir Output directory path where csv and plots are written to
#' @param RSlevels RS factor levels to set plotting order.  Inherited from UpscaleWrapper.R
#' @param my.facet Column used for plot facets.  Default set to "variable"
#'
#' @return Tibble with summary statistics
#' @export Boxplots.* Boxplots in both pdf and png format saved to output directory
#' 
make.outputs = function(in.data, pool.by, out.dir, RSlevels, my.facet = "variable"){
  
  if(!is.na(pool.by)){
    
    # if RSlevels isn't NA, set RS column to factor with same levels as RSlevels
    if(!all(is.na(RSlevels))){in.data$RS = factor(in.data$RS, levels = RSlevels)}
    
    # set output subdirectory name based on 'pool.by' argument
    if(pool.by == "RS"){
      sub.out.dir = file.path(out.dir, "byRS")
    }else if(pool.by == "RSCond"){
      sub.out.dir = file.path(out.dir, "byRSCond")
    }else{
      sub.out.dir = file.path(out.dir, "byAll")  
    }
    
    # create output subdirectory
    if(!file.exists(sub.out.dir)){dir.create(sub.out.dir, recursive = TRUE)}
    
    if(pool.by == "RS"){
      data.sub = in.data %>% select(-Condition) %>% distinct() %>% filter(!is.na(value))
      p1 = ggplot(data.sub, aes(x = factor(RS), y = value, color = RS)) +
        geom_boxplot() + 
        facet_wrap(reformulate(my.facet, "."), scales = "free_y")
    }
    
    if(pool.by == "RSCond"){
      # set condition plotting order
      in.data = in.data %>% mutate(Condition = factor(Condition, levels = condition.levels))
      data.sub = in.data %>% filter(!is.na(value))
      p1 = ggplot(data.sub, aes(x = factor(RS), y = value, fill = Condition)) +
        geom_boxplot() +
        scale_fill_manual(values = condition.fill) +
        facet_wrap(reformulate(my.facet, "."), scales = "free_y")
    }
    
    if(pool.by == "All"){
      data.sub = in.data %>% select(-RS,-Condition) %>% distinct() %>% filter(!is.na(value))
      p1 = ggplot(data.sub, aes(x = factor(variable), y = value)) +
        geom_boxplot() + 
        facet_wrap(reformulate(my.facet, "."), scales = "free")
    }
    
    out.name = file.path(sub.out.dir ,"Boxplots")
    ggsave(paste(out.name, ".pdf", sep = ""), plot = p1, width = 10, height = 7)
    ggsave(paste(out.name, ".png", sep = ""), plot = p1, width = 10, height = 7)
    
    out.data = make.summary(in.data, pool.by, sub.out.dir)
    
    return(out.data)
    
  }else{print('Pool by variable not in input dataframe')}
 
}


#' make.outputs.unit
#' 
#' @description Generic function that calls make.summary and summarize.f functions to calculate summary statistcs and create bar and boxplots
#' Similar to make.outputs function but is stuctured to include unit type (e.g., GU - geomorphic unit) in summary and plots
#'
#' @param in.data Input tibble.
#' @param pool.by How data should be grouped.  Options: 'RS', 'RSCond', 'All'
#' @param gu.type Geomorphic unit type passed as character.  Function expects either "UnitForm" or "GU".
#' @param out.dir Output directory path where csv and plots are written to 
#' @param my.facet Column used for plot facets.  Default set to "variable"
#' @param RSlevels RS factor levels to set plotting order.  Inherited from UpscaleWrapper.R
#'
#' @return Tibble with summary statistics
#' @export Boxplots.* Boxplots in both pdf and png format saved to output directory
#'
make.outputs.unit = function(in.data, pool.by, gu.type, out.dir, my.facet = "variable", RSlevels){
  
  # set output subdirectory name based on 'pool.by' argument
  if(pool.by == "RS"){
    sub.out.dir = file.path(out.dir, "byRS")
  }else if(pool.by == "RSCond"){
    sub.out.dir = file.path(out.dir, "byRSCond")
  }else{
    sub.out.dir = file.path(out.dir, "byAll")  
  }
  
  # set plotting colors
  if(gu.type == "UnitForm"){
    unit.colors = form.fill
    in.data = in.data %>%
      mutate(unit.type = factor(unit.type, levels = form.levels))
  }else{
    unit.colors = gu.fill
    in.data = in.data %>%
      mutate(unit.type = factor(unit.type, levels = gu.levels))
  }

  # create output subdirectory
  if(!file.exists(sub.out.dir)){dir.create(sub.out.dir, recursive = TRUE)}
  
  # if RSlevels isn't NA, set RS column to factor with same levels as RSlevels
  if(!all(is.na(RSlevels))){in.data$RS = factor(in.data$RS, levels = RSlevels)}
  
  if(pool.by == "All"){
    data.sub = in.data %>% select(-RS,-Condition) %>% distinct()
    if('n.units' %in% data.sub$variable){data.sub = data.sub %>% filter(variable != 'n.units')}
    p1 = ggplot(data.sub %>% filter(!is.na(value)), aes(x = factor(unit.type), y = value, fill = unit.type)) + 
      geom_boxplot() +
      scale_fill_manual(values = unit.colors) +
      facet_wrap(reformulate(".", my.facet), scales = "free_y") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  }
  
  if(pool.by == "RS"){
    data.sub = in.data %>% select(-Condition) %>% distinct()
    if('n.units' %in% data.sub$variable){data.sub = data.sub %>% filter(variable != 'n.units')}
    p1 = ggplot(data.sub %>% filter(!is.na(value)), aes(x = factor(unit.type), y = value, fill = unit.type)) + 
      geom_boxplot() +
      scale_fill_manual(values = unit.colors) +
      facet_grid(reformulate("RS", my.facet), scales = "free") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  }

 if(pool.by == "RSCond"){
   in.data = in.data %>% mutate(Condition = factor(Condition, levels = condition.levels))
   data.sub = in.data
   if('n.units' %in% data.sub$variable){data.sub = data.sub %>% filter(variable != 'n.units')}
    p1 = ggplot(data.sub %>% filter(!is.na(value)), aes(x = factor(unit.type), y = value, fill = Condition)) + 
      geom_boxplot() +
      scale_fill_manual(values = condition.fill) +
      facet_grid(reformulate("RS", my.facet), scales = "free", space = "fixed") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  }
  
  out.name = file.path(sub.out.dir, "Boxplots")

  ggsave(paste(out.name, ".pdf", sep=""), plot = p1, width = 15, height = 15 )
  ggsave(paste(out.name, ".png", sep=""), plot = p1, width = 15, height = 15)
  
  out.data = make.summary(in.data, pool.by, sub.out.dir)
  
  return(out.data)
}


#' assemblage.proportions
#' 
#' @description Calculates geomorphic unit assemblage based on 'area.ratio' from GUT Unit metrics table
#' where area.ratio = sum(areas for unit type i) / sum(areas for all unit types).  Called by assemblage.unit script.
#'
#' @param my.stats Input tibble
#' @param pool.by How data should be grouped.  Options: 'RS', 'RSCond', 'All'
#' @param out.dir Output directory path where csv and plots are written to
#' @param my.ROI Region of interest.  Default set to "bankfull"
#' @param gu.type Geomorphic unit type passed as character.  Function expects either "UnitForm" or "GU". 
#'
#' @export assemblage.csv  CSV file with proportions for each unit type written to output directory
#'
assemblage.proportions = function(my.stats, pool.by, out.dir, my.ROI = "bankfull", gu.type = gu.type){
  
  # set output subdirectory name based on 'pool.by' argument
  if(pool.by == "RS"){
    sub.out.dir = file.path(out.dir, "byRS")
  }else if(pool.by == "RSCond"){
    sub.out.dir = file.path(out.dir, "byRSCond")
  }else{
    sub.out.dir = file.path(out.dir, "byAll")  
  }
  
  #filter data for just proportions of reach occupied by each unit
  assemblage = my.stats %>% 
    filter(variable == "area.ratio", ROI == my.ROI) %>%
    select(-sd, -se, -med, -min, -max, -n, -sum)
  
  # spreads table grouped by pool.by so that unit types are converted to columns
  if(pool.by == "RSCond"){
    assemblage.group = assemblage %>%
      group_by(RS, Condition) %>%
      spread(key = "unit.type", value = avg) %>%
      ungroup()
  }
  
  if(pool.by=="RS"){
    assemblage.group = assemblage %>%
      group_by(RS) %>%
      spread(key = "unit.type", value = avg) %>%
      ungroup
  }
  
  if(pool.by == "All"){
    assemblage.group = assemblage %>%
      spread(key = "unit.type", value = avg)
  }
  
  # makes list of unit types and find starting and ending column for renormalization
  unit.list = levels(as.factor(my.stats$unit.type))
  start.col = length(names(assemblage.group)) - length(unit.list) + 1
  end.col = length(names(assemblage.group))
  
  # renormalize proportions to sum to 100
  assemblage.norm = assemblage.group %>% ungroup() %>%
    mutate(SUM1 = select(., start.col:end.col) %>% apply(1, sum, na.rm = TRUE)) %>% #make sum across proportions
    mutate_at(start.col:end.col, funs(./SUM1)) %>% #renormalize columns
    mutate(SUM = select(.,start.col:end.col) %>% apply(1, sum, na.rm = TRUE)) %>% #check math
    select(-SUM1)
  
  # save output
  write_csv(assemblage.norm, file.path(sub.out.dir, "assemblage.csv"), col_names = TRUE)
  
  # call function to make plots
  assemblage.plot(assemblage.norm, pool.by, start.col, end.col, out.dir = sub.out.dir, gu.type = gu.type)
  
}


#' assemblage.plot
#' 
#' @description Function to creat unit type (e.g., GU - geomorphic unit) bar plots. Called by assemblage.proportions function.
#'
#' @param assemblage.data Tibble of assemblage area ratios.  Passed from assemblage.proportions function 
#' @param pool.by How data should be grouped.  Options: 'RS', 'RSCond', 'All'
#' @param start.col Start of unit type columns. Passed from assemblage.proportions function. Used to convert assemblage tibble from wide format to long format.
#' @param end.col End of unit type columns. Passed from assemblage.proportions function. Used to convert assemblage tibble from wide format to long format.
#' @param out.dir Output directory path where csv and plots are written to
#' @param gu.type Geomorphic unit type passed as character.  Function expects either "UnitForm" or "GU".
#'
#' @export assemblage.* Assemblage barplots (using average area ratio) in both pdf and png format saved to output directory
#' 
assemblage.plot = function(assemblage.data, pool.by, start.col, end.col, out.dir, gu.type){
  
  #Read in and manipulate data for plotting
  my.data = assemblage.data %>%
    gather(key = "Unit", value = "value", start.col:end.col) %>%
    select(-SUM)
  
  # sets colors and factor orders for plotting
  
  # condition levels for plots hard coded as "poor", "mod", "good", "intact" to show up in correct order
  if(pool.by == "RSCond"){
    my.data$Condition = factor(my.data$Condition, levels = condition.levels)}
  
  # set RSlevel order (if not set to NA)
  if(pool.by != "All"){
    if(!all(is.na(RSlevels))){my.data$RS = factor(my.data$RS, levels = RSlevels)}}
  
  # set plotting colors
  if(gu.type == "UnitForm"){
    unit.colors = form.fill
    my.data = my.data %>%
      mutate(Unit = factor(Unit, levels = form.levels))
  }else{
    unit.colors = gu.fill
    my.data = my.data %>%
      mutate(Unit = factor(Unit, levels = gu.levels))
  }
  
  #makes assemblage plots
  if(pool.by == "All"){
    p1 = ggplot(my.data %>% filter(!is.na(value)), aes(x = ROI, y = value, fill = Unit)) +
      geom_bar(stat = "identity", position = 'stack') +
      scale_fill_manual(values = unit.colors)
  }
  
  if(pool.by == "RS"){
    p1 = ggplot(my.data %>% filter(!is.na(value)), aes(x = RS, y = value, fill = Unit)) +
      geom_bar(stat = "identity", position = 'stack') +
      scale_fill_manual(values = unit.colors)
  }
  
  if(pool.by == "RSCond"){
    p1 = ggplot(my.data %>% filter(!is.na(value)), aes(x = Condition, y = value, fill = Unit)) +
      geom_bar(stat = "identity", position = 'stack') +
      scale_fill_manual(values = unit.colors) +
      facet_wrap(~ RS, scales = 'free')
  }
  
  # save assemblage plots 
  ggsave(file.path(out.dir, "assemblage.pdf"), plot = p1, width = 10, height = 7 )
  ggsave(file.path(out.dir, "assemblage.png"), plot = p1, width = 10, height = 7)
  
}


