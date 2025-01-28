

getIndicators <- function(df, sf, aggregate_time, indicator, partition, per_class = FALSE){
  
  group_vars <- function(multi = FALSE, space = TRUE){
    vars <- if (indicator != 'general') c('status') else c()
    vars <- if (multi) c('pcode', 'D1') else if (space) c(vars, 'origin', 'destin') else vars
    vars <- if (aggregate_time) vars else c(vars, 'start', 'end')
    if (per_class) c(vars, 'class') else vars
  }
  
  distinct_vars <- function(){
    vars <- c('pcode', 'D1')
    vars <- if (aggregate_time) vars else c(vars, 'start', 'end')
    if (per_class) c(vars, 'class') else vars
  }
  
  # return the variables to select on the modes and activity indicators
  select_vars <- function(status = TRUE){
    vars <- c('total')
    vars <- if (status) c(vars, 'status') else vars
    vars <- if(aggregate_time) vars else c(vars, 'start')
    if (per_class) c(vars, 'class') else vars
  }
  
  print(paste(paste('Aggregate time:', aggregate_time), paste('Indicator:', indicator), ifelse(per_class, 'Per class', ''), sep = '. '))
  
  df <- if(indicator == 'activity') df %>% select(-T3) %>% rename(status = D5) else df %>% select(-D5) %>% rename(status = T3)
  
  if (indicator != 'general') # verify whether people do multiple trips for different reasons or using different modes of transport in the same time period
    df <- df %>% group_by_at(group_vars(multi = TRUE)) %>%
      mutate(status = ifelse(n_distinct(status) > 1, 'multi', status)) %>%
      distinct_at(distinct_vars(), .keep_all = TRUE)
  
  df <- df %>% group_by_at(group_vars()) %>%
    summarise(value = sum(coem)) %>%
    group_by_at(group_vars(space = FALSE))
  
  list <- df %>% summarise_all(funs(matrix = list(.))) %>% as.tibble
  
  apply(list, 1, function(x) { 
    df <- as.data.frame(t(do.call(rbind.data.frame, x)))
    
    start <- ncol(df)-2
    end <- ncol(df)
    
    df <- df %>% select(start:end) %>% 
      rename_at(vars(paste0('V', c(start:end))), ~ c('origin', 'dest', 'value')) %>%
      mutate(origin = as.character(origin), dest = as.character(dest), value = as.numeric(as.character(value)))
    
    mat <- prepflows(mat = df, i = 'origin', j = 'dest', fij = "value")
    
    cols <- ncol(mat)-1
    codes <- as.numeric(as.character(colnames(mat)))
    indexing = data.frame(code = codes, index = c(0:cols), name = codes) %>%
      mutate(name = mapvalues(name, from = sf$code, to = sf$name, warn_missing = FALSE))
    list('start' = if(aggregate_time) 0 else x$start, 'end' = if(aggregate_time) 0 else x$end, 'status' = ifelse(is.null(x$status), 'none', x$status), 
         'partition' = partition, 'indicator' = indicator, 'matrix' = list(mat), 'indexing'= list(indexing))
  })
}

generateIndicators <- function(){
  traj_df <- deplatraj_df %>% rename(origin = D3, destin = D7) %>% filter(origin %in% space_ref$DTIR & destin %in% space_ref$DTIR)
  
  traj_df <- traj_df %>%
    mutate(T3 = mapvalues(T3, mode_ref$code, mode_ref$desc_en, warn_missing = FALSE)) %>%
    mutate(T3 = replace_na(T3, 'walk')) %>%
    mutate(D5 = mapvalues(D5, activity_ref$code, activity_ref$desc_en, warn_missing = FALSE))
  
  if (args$class)
    traj_df <- traj_df %>% left_join(class_ref, by = c('pcode'='pcode')) # to calculate the presence per activity and class (state distribution plot)
  
  print('Expanding trips tables according to one-hour time intervals...')
  trajexp <- expandTrajet(traj_df, flows = TRUE) # trajet table expanded per hour and per spatial location
  
  flows <- c()
  
  for (p in args$partitions){
    print(paste('Computing flows for partition', p))
    
    traj_bis <- traj_bis <- trajexp %>% 
      mutate(origin = mapvalues(origin, from = space_ref$DTIR, to = space_ref[[p]], warn_missing = FALSE)) %>%
      mutate(destin = mapvalues(destin, from = space_ref$DTIR, to = space_ref[[p]], warn_missing = FALSE)) 
    
    file_name <- getFilePath(args$rds, paste0(p, '_surface.rds'))
    print(paste('Loading shapefile from', file_name))
    sf <- readRDS(file_name)
      
    for (i in c('general', 'activity', 'modes')){
      aggreg <- getIndicators(df = traj_bis, sf = sf, aggregate_time = TRUE, partition = p, indicator = i)
      disaggreg <- getIndicators(df = traj_bis, sf = sf, aggregate_time = FALSE, partition = p, indicator = i)
      
      flows <- c(flows, aggreg, disaggreg)
    }
    
  }
  
  file_name <- getFilePath(args$csv, 'flows.json')
  write_json(flows, file_name)
  print(paste('Saved as', file_name))
}

generateIndicators()

