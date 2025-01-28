#!/usr/bin/env Rscript

#--------------------------------------------------------------------------------
# aggregate presence per activity and mode, presence density, fluctuation, and attractiveness
#--------------------------------------------------------------------------------

#----------------------------------------------------------
# generate indicators of presence (in general and per activity), fluctuation and attractiveness
# aggregate_time indicates the time aggregation, TRUE for a unique time interval of 24 hours and FALSE for 24 one-hour periods
# partition : the territorial partition over with calculate the indicators (none indicates the whole studied territory)
# sf : the shape file for the corresponding partition, from which we recover the surface for computing presence density
# pop_df : a table indicating the population of each spatial location of the given territorial partition
# depla_df : the deplacement table expanded per hour (generate by expandTime())
# traj_df : the trajet table expanded per hour (generate by expantTrajet()), to compute the indicators of presence per mode of transport

getIndicators <- function(depla_df, traj_df, pop_df, sf = NULL, partition = 'none', aggregate_time = FALSE, aggregate_space = FALSE, per_class = FALSE){

  temporal <- c('start', 'end')
  by_temporal <- c('start'='start', 'end'='end')
  
  # return the variables used for detecting distinct rows
  distinct_vars <- function(status = FALSE){
    vars <- if (status)
        if (per_class) c('pcode', 'status', 'class') else c('pcode', 'status')
      else
        if (per_class) c('pcode', 'class') else c('pcode')
    
    vars <- if (aggregate_time) vars else c(vars, 'start', 'end')
    return(vars)
  }
  
  # return the variables to group
  group_vars <- function(status = FALSE, pcode = FALSE){
    vars <- if (per_class) 
      if (status) c('status', 'class') else if (pcode) c('pcode', 'class') else c('class')
    else 
      if (status) c('status') else if (pcode) c('pcode') else c()


    vars <- if (aggregate_space) vars else c(vars, 'code')
    vars <- if (aggregate_time) vars else c(vars, 'start', 'end')

    return(vars)
  }
  
  # return the variables to select on the modes and activity indicators
  select_vars <- function(){
    vars <- c('status', 'total')
    vars <- if(aggregate_space) vars else c('code', vars)
    vars <- if(aggregate_time) vars else c(vars, 'start', 'end')
    if (per_class) c(vars, 'class') else vars
  }
  
  # return the variables to compare inside a left_join
  compare_vars <- function(status = TRUE){
    vars <- if (aggregate_space) c() else c('code')
    vars <- if (aggregate_time) vars else c(vars, 'start', 'end')
    vars <- if (per_class) c(vars, 'class') else vars
    if (status) c(vars, 'status') else vars
  }
  
  getPresence <- function(df){
    presence <- df %>% distinct_at(distinct_vars(), .keep_all = TRUE) %>%
      group_by_at(group_vars()) %>%
      summarise(moving = sum(coem)) %>%
      mutate(indicator = 'presence')
    
    if (aggregate_space) 
      presence %>%
        rename(total = moving) %>%
        mutate(value = total/sum(pop_df$pop))
    else{
      if (!aggregate_time){
        start <- 4:28
        temp <- tibble(code = unique(presence$code))
        temp <- expand(temp, nesting(code), start)
        
        presence <- temp %>%
          left_join(presence, by = c('code'='code', 'start'='start')) %>%
          mutate(end = start + 1) %>%
          replace(is.na(.), 0) %>%
          distinct(code, start, end, .keep_all = TRUE)
      }
      
      presence %>% left_join(pop_df, by = c('code'='code')) %>%
        mutate(total = moving + not_mov) %>% # consider the moving people and the ones who reported staying at home the period of the survey
        mutate(value = total/sum(pop_df$pop)) %>%
        left_join(sf, by = c("code" = "code")) %>% # recover the shapefile and calculate the density over 24 hours for 97 sectors
        mutate(density = total/sqkm) %>%
        select(-pop, -mov, -not_mov)
    }
  }
  
  getPresencePerStatus <- function(df){
    temp <- df %>%
      distinct_at(distinct_vars(status = TRUE), .keep_all = TRUE) %>%
      group_by_at(group_vars(pcode = TRUE)) %>%
      mutate(times = 1/n()) %>%
      mutate(total_multi = ifelse(times < 1, times*coem, 0)) %>%
      group_by_at(group_vars(status = TRUE)) %>%
      summarise(total = sum(times*coem), total_multi = sum(total_multi)) # sum up the number of people doing each activity
    
    keys <- unique(temp$status)
    activity <- temp %>%
      select(select_vars()) %>%
      spread(status, total) %>%
      gather(status, total, keys) %>%
      left_join(temp %>% select(-total), by = compare_vars())
    
    if (aggregate_time & aggregate_space & !per_class)
      activity %>% mutate(value = total/presence$total[1]) %>% select(status, total, value)
    else 
      activity %>% left_join(presence %>% select(-status, -value, -indicator) %>% rename(pres = total), by = compare_vars(status = FALSE)) %>%
        mutate(value = total/pres, value_multi = total_multi/total)
  }
  
  presence <- getPresence(df = traj_df) %>% mutate(status = 'modes')
  print(paste('Computing indicator of presence PER MODE OF TRANSPORT Space:', ifelse(aggregate_space, 'aggregate.', 'disaggregate.'), 'Time:', ifelse(aggregate_time, 'aggregate.', 'disaggregate.'), ifelse(per_class, 'Per Class.', '')))
  modes <- getPresencePerStatus(df = traj_df) %>% mutate(indicator = 'modes') %>% arrange(match(status, c("car", "walk", "pts", 'bike', 'other')))
  bind_df <- bind_rows(presence, modes)
  
  presence = getPresence(df = depla_df) %>% mutate(status = 'activity')
  print(paste('Computing indicator of presence PER ACTIVITY. Space:', ifelse(aggregate_space, 'aggregate.', 'disaggregate.'), 'Time:', ifelse(aggregate_time, 'aggregate.', 'disaggregate.'), ifelse(per_class, 'Per Class.', '')))
  activity <- getPresencePerStatus(df = depla_df) %>% mutate(indicator = 'activity')
  bind_df <- bind_rows(bind_df, presence, activity)
  
  fluctuation <- tibble()
  attractivity <- tibble()
  
  if (!per_class & !aggregate_space){
    print(paste('Computing FLUCTUATION indicator. Space: disaggregate. Time:', ifelse(aggregate_time, 'aggregate.', 'disaggregate.')))
    
    fluctuation <- presence %>% 
      mutate(pres = total) %>%
      left_join(pop_df, by = c("code"="code")) %>%
      mutate(total = pres - pop) %>%
      mutate(value = total / pop) %>% # fluctuation of population (the proportion between the number of people present on the location and its population)
      select(if(aggregate_time) c('code', 'total', 'value') else c('code', 'total', 'value', temporal)) %>%
      mutate(indicator = 'fluctuation')
    
    if (aggregate_time){
      print('Computing ATTRACTIVENESS indicator. Space: disaggregate. Time: aggregate.')
      total_res <- sum(pop_df$pop) # the territory's population
      total_mov <- sum(presence$moving) # the number of people in activity within the territory (do not take in account people who stays at home the whole time)
      attractivity <- presence %>%
        left_join(pop_df, by = c('code'='code')) %>%
        group_by(code) %>%
        summarise(value = (moving/pop)*(total_res/total_mov)) %>%
        select(code, value) %>%
        mutate(indicator = 'attractiveness') 
    }
  }
  
  print('Binding indicators into a unique table...')
  
  if (aggregate_space) 
    bind_df %>%
      mutate(partition = 'none', space = 'aggregate', time = ifelse(aggregate_time, 'aggregate', 'individual'))
  else
    bind_rows(bind_df %>% select(-moving), fluctuation, attractivity) %>%
      mutate(name = mapvalues(code, from = sf$code, to = sf$name, warn_missing = FALSE),
             partition = partition, 
             time = ifelse(aggregate_time, 'aggregate', 'individual'),
             space = 'individual')
}

generateIndicators <- function(){
  #-----------------------------------------------------------
  # rename the spatial location and motivation variables
  # delete all locations outside the studied territory
  # prepare data for using activities
  depla_df <- deplaexpanded_df %>% rename(code = D7, status = D5) %>%
    filter(code %in% space_ref$DTIR) %>%
    mutate(status = mapvalues(status, as.numeric(activity_ref$code), activity_ref$desc_en, warn_missing = FALSE)) 
  
  if (args$class)
    depla_df <- depla_df %>% left_join(class_ref, by = c('pcode'='pcode')) # to calculate the presence per activity and class (state distribution plot)
  
  # prepare data for using modes of transport
  traj_df <- deplatraj_df %>% rename(code = D7, status = T3) %>%
    filter(code %in% space_ref$DTIR) %>%
    mutate(status = mapvalues(status, as.numeric(mode_ref$code), mode_ref$desc_en, warn_missing = FALSE)) %>%
    mutate(status = replace_na(status, 'walk')) 
  
  if (args$class)
    traj_df <- traj_df %>% left_join(class_ref, by = c('pcode'='pcode')) # to calculate the presence per activity and class (state distribution plot)
  
  print('Expanding deplacement table according to one-hour time intervals...')
  deplaexp <- expandTime(depla_df) # deplacement table hourly expanded per spatial location
  
  print('Expanding trips table according to one-hour time intervals...')
  trajexp <- expandTrajet(traj_df) # trajet table expanded per hour and per spatial location
  
  # presence over the whole studied territory (used for the details on the state distribution plot)
  aggreg <- getIndicators(depla_df = deplaexp, traj_df = trajexp, pop_df = population_df, aggregate_space = TRUE, aggregate_time = TRUE) # indicators of presence general and per activity over 24 hours
  disaggreg <- getIndicators(depla_df = deplaexp, traj_df = trajexp, pop_df = population_df, aggregate_space = TRUE) # indicators of presence general and per activity per time interval
  
  # bind all tables into a big one
  presence_df <- bind_rows(aggreg, disaggreg)
  
  if (args$class){
    aggreg_class <- getIndicators(depla_df = deplaexp, traj_df = trajexp, pop_df = population_df, aggregate_space = TRUE, aggregate_time = TRUE, per_class = TRUE)
    disaggreg_class <- getIndicators(depla_df = deplaexp, traj_df = trajexp, pop_df = population_df, aggregate_space = TRUE, per_class = TRUE)
    
    presence_df <- bind_rows(presence_df, aggreg_class, disaggreg_class)
  }

  # indicators per territorial partition. space : disaggregate.
  for (p in args$partitions){
    print(paste('Computing indicators for territorial partition', p, '...'))

    pop_bis <- population_df %>% ungroup() %>%
      mutate(TIRA = mapvalues(TIRA, from = space_ref$DTIR, to = space_ref[[p]], warn_missing = FALSE)) %>%
      group_by(TIRA) %>% summarise(mov = sum(mov), not_mov = sum(not_mov), pop = mov + not_mov) %>%
      rename(code = TIRA)

    deplaexp_bis <- deplaexp %>% mutate(code = mapvalues(code, from = space_ref$DTIR, to = space_ref[[p]], warn_missing = FALSE))
    trajexp_bis <- trajexp %>% mutate(code = mapvalues(code, from = space_ref$DTIR, to = space_ref[[p]], warn_missing = FALSE))
    
    sf <- readRDS(getFilePath(args$rds, paste0(p, '_surface.rds')))

    aggreg <- getIndicators(depla_df = deplaexp_bis, traj_df = trajexp_bis, pop_df = pop_bis, sf = sf, partition = p, aggregate_time = TRUE)
    disaggreg <- getIndicators(depla_df = deplaexp_bis, traj_df = trajexp_bis, pop_df = pop_bis, sf = sf, partition = p)
    
    presence_df <- bind_rows(presence_df, aggreg, disaggreg)
    
    if (args$class){
      aggreg_class <- getIndicators(depla_df = deplaexp_bis, traj_df = trajexp_bis, pop_df = pop_bis, sf = sf, partition = p, aggregate_time = TRUE, per_class = TRUE)
      disaggreg_class <- getIndicators(depla_df = deplaexp_bis, traj_df = trajexp_bis, pop_df = pop_bis, sf = sf, partition = p, per_class = TRUE)
    
      presence_df <- bind_rows(presence_df, aggreg_class, disaggreg_class)
    }
    
  }
  
  select_final_vars <- function(){
    vars <- c('time', 'space', 'code', 'name', 'start', 'end', 'indicator', 'status', 'total', 'value', 'density', 'total_multi', 'value_multi', 'partition')
    if(args$class) c(vars, 'class') else vars
  }
  
  print('Preparing final file...')
  presence_df <- presence_df %>% ungroup() %>%
    select(select_final_vars()) %>%
    mutate(status = replace_na(status, 'none'), name = replace_na(name, args$area)) %>%
    replace(is.na(.), 0) %>%
    filter(start <= 29 & end <= 30)
    
  file_name <- getFilePath(args$csv, 'presence.csv')
  write_csv(presence_df, file_name)
  print(paste('Saved as', file_name))
}

print(paste('Generating presence indicators for', args$area, 'area'))
generateIndicators()

