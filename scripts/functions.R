# summarizes data by value field
# note: tibble should be grouped before hand if you want the summary by group
summarize.f = function(data, value){
  
  data.summary = data %>%
    summarize(avg = mean(value, na.rm = TRUE), 
              sd = sd(value, na.rm = TRUE),
              med = median(value, na.rm = TRUE), 
              min = min(value, na.rm = TRUE),
              max = max(value, na.rm = TRUE),
              tot = sum(value, na.rm = TRUE),
              n = length(na.omit(value)))
  
  return(data.summary)
}


# function for grouped summaries
# in.data must be in long format with value, variable, RS, Condition, unit.type and ROI fields
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

# function that includes make.summary and summarize.f. writes summary and plots
# To do----------------------------------------------
# clean up xlab so it doesn't appear
# get rid fo repetivenesss in code.  figure out ggplot better.
# omit na prior to plotting so we don't get so many warning messages

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
      p1 = ggplot(in.data, aes(x = factor(RS), y = value, fill = Condition)) +
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


#very similar to make.outputs, just boxplots filled by Unit types rather than condition dependent upon set.levels.colors()
#to do----------------------------------------------------------
#make xlabels vertical rather than horizontal so I can read them
#2 levels of facet wrapping for conditin and RS- separate x axis somehow so it is easier to read and understand.
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
    p1 = ggplot(data.sub, aes(x = factor(unit.type), y = value, fill = unit.type)) + 
      geom_boxplot() +
      scale_fill_manual(values = unit.colors) +
      facet_wrap(reformulate(".", my.facet), scales = "free_y") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  }
  
  if(pool.by == "RS"){
    data.sub = in.data %>% select(-Condition) %>% distinct()
    p1 = ggplot(data.sub, aes(x = factor(unit.type), y = value, fill = unit.type)) + 
      geom_boxplot() +
      scale_fill_manual(values = unit.colors) +
      facet_grid(reformulate("RS", my.facet), scales = "free") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  }
  
#does it even make sense to plot condition for different units since the condition is really about the reach...  

 if(pool.by == "RSCond"){
   in.data = in.data %>% mutate(Condition = factor(Condition, levels = condition.levels))
    p1 = ggplot(in.data, aes(x = factor(unit.type), y = value, fill = Condition)) + 
      geom_boxplot() +
      scale_fill_manual(values = condition.fill) +
      facet_grid(reformulate("RS", my.facet), scales = "free") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

  }
  
  out.name = file.path(sub.out.dir ,"Boxplots")

  ggsave(paste(out.name, ".pdf", sep=""), plot = p1, width =15, height = 15 )
  ggsave(paste(out.name, ".png", sep=""), plot = p1, width = 15, height = 15)
  
  out.data = make.summary(in.data, pool.by, sub.out.dir)
  return(out.data)
}

