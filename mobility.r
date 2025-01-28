
getIndicators <- function(df, pop_df, category, aggregate_time = FALSE, aggregate_space = FALSE, per_class = FALSE){
  group_vars <- function(status = TRUE, pcode = FALSE){
    vars <- if (status) c('status') else if (pcode) c('pcode') else c()
    vars <- if (aggregate_time) vars else c(vars, 'start', 'end')
    vars <- if (aggregate_space) vars else c(vars, 'code')
    if (per_class) c(vars, 'class') else vars
  }
  
  distinct_vars <- function(status = TRUE){
    vars <- if(status) c('pcode', 'status') else c('pcode')
    vars <- if (aggregate_time) vars else c(vars, 'start', 'end')
    vars <- if (aggregate_space) vars else c(vars, 'code')
    if (per_class) c(vars, 'class') else vars
  }
  
  by_vars <- function(status = TRUE, end = TRUE){
    vars <- if (status) c('status') else c()
    vars <- if (aggregate_time) vars else c(vars, 'start')
    vars <- if (!aggregate_time & end) c(vars, 'end') else vars
    vars <- if (aggregate_space) vars else c(vars, 'code')
    if (per_class) c(vars, 'class') else vars
  }
  
  # return the variables to select on the modes and activity indicators
  select_vars <- function(status = TRUE, end = TRUE){
    vars <- c('total')
    vars <- if (status) c(vars, 'status') else vars
    vars <- if(aggregate_space) vars else c('code', vars)
    vars <- if(aggregate_time) vars else c(vars, 'start')
    vars <- if(!aggregate_time & end) c(vars, 'end') else vars
    if (per_class) c(vars, 'class') else vars
  }
  
  print(paste(paste('Aggregate time:', aggregate_time), paste('Aggregate space:', aggregate_space), ifelse(per_class, 'Per class', ''), sep = '. '))
  
  df <- if(category == 'activity') df %>% select(-T3) %>% rename(status = D5) else df %>% select(-D5) %>% rename(status = T3)
  
  getMobilityIntensity <- function(){
    temp <- df %>% 
      distinct_at(distinct_vars(status = FALSE), .keep_all = TRUE) %>%
      group_by_at(group_vars(status = FALSE, pcode = TRUE)) %>%
      mutate(times = 1/n()) %>%
      mutate(total_multi = ifelse(times < 1, times*coem, 0)) %>%
      group_by_at(group_vars(status = FALSE)) %>%
      summarise(total = sum(times*coem))
    
    
    if (per_class){
      keys <- unique(temp$class)
      mobility <- temp %>% select(select_vars(status = FALSE)) %>%
        spread(class, total) %>%
        gather(class, total, keys) %>%
        left_join(temp %>% select(-total), by = by_vars(status = FALSE))
      
      temp <- mobility
    }
    
    if (!aggregate_time){
      keys <- unique(temp$start)
      mobility <- temp %>% ungroup() %>% select(select_vars(status = FALSE, end = FALSE)) %>%
        spread(start, total) %>%
        gather(start, total, as.character(keys)) %>%
        mutate(start = as.numeric(start)) %>%
        left_join(temp %>% select(-total), by = by_vars(status = FALSE, end = FALSE)) %>%
        mutate(end = start + 1)
      
      temp <- mobility
    }
    
    if (aggregate_space){
      # mobility <- if(!per_class) temp %>% mutate(value = total/sum(pop_df$pop))
      #   else temp %>% left_join(pop_df, by = c('class', 'class')) %>% mutate(value = total/pop) %>% select(-pop)
      mobility <- temp %>% mutate(value = total/sum(pop_df$pop))
    }else{
      # mobility <- temp %>% left_join(pop_df, by = if (!per_class) c('code'='code') else c('code'='code', 'class' ='class')) %>%
      #   mutate(value = total/pop) %>% select(-pop)
      mobility <- temp %>% left_join(pop_df, by = c('code'='code')) %>% mutate(value = total/pop) %>% select(-pop)
    }
    
    mobility %>% mutate(indicator = 'general')
  }
  
  getMobilityByStatus <- function(){
    
    temp <- df %>% distinct_at(distinct_vars(), .keep_all = TRUE) %>%
      group_by_at(group_vars(status = FALSE, pcode = TRUE)) %>%
      mutate(times = 1/n()) %>%
      mutate(total_multi = ifelse(times < 1, times*coem, 0)) %>%
      group_by_at(group_vars()) %>%
      summarise(total = sum(times*coem), total_multi = sum(total_multi))
    
    keys <- unique(temp$status)

    mobility <- temp %>%
      select(select_vars()) %>%
      spread(status, total) %>%
      gather(status, total, keys) %>%
      left_join(temp %>% select(-total), by = by_vars())
    
    
    if (!aggregate_time){
      keys <- unique(mobility$start)
      
      temp <- mobility %>% ungroup() %>% select(select_vars(end = FALSE)) %>%
        spread(start, total) %>%
        gather(start, total, as.character(keys)) %>%
        mutate(start = as.numeric(start)) %>%
        left_join(mobility %>% select(-total), by = by_vars(end = FALSE)) %>%
        mutate(end = start + 1)
      
      mobility <- temp
    }
    
    mobility <- if(aggregate_space & aggregate_time & !per_class) mobility %>% mutate(value = total/mob_df$total[1], value_multi = total_multi/total) else  
      mobility %>% left_join(mob_df %>% rename(mov = total), by = by_vars(status = FALSE)) %>% mutate(value = total/mov, value_multi = total_multi/total) %>% select(-mov)
    
    if (!aggregate_time){
      nb <- ifelse(category == 'modes', 7, 6)
      mobility <- mobility %>% replace(is.na(.), 0) %>%
        group_by_at(group_vars(status = FALSE)) %>%
        mutate(invalid = ifelse(sum(total) == 0, TRUE, FALSE)) %>%
        mutate(value = ifelse(invalid, 1/nb, value), status = ifelse(invalid, 'none', status)) %>%
        select(-invalid)
    }
    
    return (mobility)
  }
  
  mob_df <- getMobilityIntensity()
  status_df <- getMobilityByStatus() %>% mutate(indicator = category)
  # compute the mobility intensity twice, but if it is not per class, no need to append twice to the table, so only appen when category == 'activity'
  final_df <- bind_rows(if (per_class) mob_df %>% mutate(status = category) else if (category == 'modes') tibble() else mob_df, status_df)
  
  final_df %>%
    mutate(time = ifelse(aggregate_time, 'aggregate', 'individual'),
           space = ifelse(aggregate_space, 'aggregate', 'individual'))
  
}

generateIndicators <- function(){
  
  # prepare data for using modes of transport
  traj_df <- deplatraj_df %>% rename(code = D7) %>% filter(code %in% space_ref$DTIR)
  
  traj_df <- traj_df %>%
    mutate(T3 = mapvalues(T3, mode_ref$code, mode_ref$desc_en, warn_missing = FALSE)) %>%
    mutate(T3 = replace_na(T3, 'walk')) %>%
    mutate(D5 = mapvalues(D5, activity_ref$code, activity_ref$desc_en, warn_missing = FALSE))
  
  if (args$class)
    traj_df <- traj_df %>% left_join(class_ref, by = c('pcode'='pcode')) # to calculate the presence per activity and class (state distribution plot)
  
  print('Expanding trips tables according to one-hour time intervals...')
  trajexp <- expandTrajet(traj_df) # trajet table expanded per hour and per spatial location
  
  # the final table
  mobility_df <- tibble()
  
  for (p in args$partitions){
    print(paste('Computing indicators for territorial partition', p, '...'))
    
    # map the codes of zones of population table from pulling sectors to the current partition
    pop_bis <- population_df %>% 
      mutate(code = mapvalues(TIRA, from = space_ref$DTIR, to = space_ref[[p]], warn_missing = FALSE),
             code = as.character(code)) %>%
      group_by(code) %>% summarise(pop = sum(pop))
    
    # if the calculations consider the trajectory classification
    # map the sectors code of the population table to the current partition
    if (args$class)
      pop_class <- pop_class %>% ungroup() %>%
        mutate(code = mapvalues(code, from = space_ref$DTIR, to = space_ref[[p]], warn_missing = FALSE)) %>%
        mutate(code = as.character(code)) %>%
        group_by(code, class) %>% summarise(pop = sum(pop)) 
    
    # map the codes of origin and destination zones from fines zones to the current partition
    traj_bis <- trajexp %>% 
      mutate(code = mapvalues(code, from = space_ref$DTIR, to = space_ref[[p]], warn_missing = FALSE)) %>%
      mutate(code = as.character(code))
    
    df <- tibble()
    for (s in c('activity', 'modes')){
      print(paste('Category:', s))
      
      if (p == 'DTIR'){
        df1 <- getIndicators(df = traj_bis, pop_df = pop_bis, aggregate_time = TRUE, aggregate_space = TRUE, category = s)
        df2 <- getIndicators(df = traj_bis, pop_df = pop_bis, aggregate_time = FALSE, aggregate_space = TRUE, category = s)
        
        df <- bind_rows(df, df1, df2)
        
        if (args$class){
          df1_class <- getIndicators(df = traj_bis, pop_df = pop_bis, aggregate_time = TRUE, aggregate_space = TRUE, per_class = TRUE, category = s)
          df2_class <- getIndicators(df = traj_bis, pop_df = pop_bis, aggregate_time = FALSE, aggregate_space = TRUE, per_class = TRUE, category = s)
          
          df <- bind_rows(df, df1_class, df2_class)
        }
      }
      df3 <- getIndicators(df = traj_bis, pop_df = pop_bis, aggregate_time = TRUE, aggregate_space = FALSE, category = s)
      df4 <- getIndicators(df = traj_bis, pop_df = pop_bis, aggregate_time = FALSE, aggregate_space = FALSE, category = s)
      
      df <- bind_rows(df, df3, df4)
      
      if (args$class){
        df3_class <- getIndicators(df = traj_bis, pop_df = pop_bis, aggregate_time = TRUE, aggregate_space = FALSE, per_class = TRUE, category = s)
        df4_class <- getIndicators(df = traj_bis, pop_df = pop_bis, aggregate_time = FALSE, aggregate_space = FALSE, per_class = TRUE, category = s)
        
        df <- bind_rows(df, df3_class, df4_class)
      }
    }
     
    sf <- readRDS(getFilePath(args$rds, paste0(p, '_surface.rds')))
    
    mobility_df <- bind_rows(mobility_df, df %>% mutate(name = mapvalues(code, from = sf$code, to = sf$name, warn_missing = FALSE),
                                                        partition = ifelse(space == 'aggregate', 'none', p)))
  }
  
  mobility_df <- mobility_df %>%
    mutate(name = replace_na(name, args$area), status = replace_na(status, 'none')) %>%
    replace(is.na(.), 0) %>%
    filter(start <= 27 & end <= 28)
  
  
  file_name <- getFilePath(args$csv, 'mobility.csv')
  write_csv(mobility_df, file_name)
  print(paste('Saved as', file_name))
}

print(paste('Generating indicators considering people on displacement for', args$area, 'area'))
generateIndicators()
