#!/usr/bin/env Rscript

source('utils.r')

parser <- setParser('Semantic of Individual Origin-Destination Trajectories Classes')

args <- parser$parse_args()

args$area <- 'lyon'

loadPackages(c('plyr', 'tidyverse', 'sf', 'jsonlite'))

#----------------------------------------------------------
# methods

getSemantic <- function(df, indicator, aggregate = FALSE){
    print('Setting semantic information...')

    select_vars <- function(class){
      vars <- c('pcode', 'key', 'value')
      if (class != 'none') c(vars, 'class') else vars
    }
    
    group_vars <- function(class, key){
      vars <- if (class != 'none') c('class')
      if(key) c(vars, 'key', 'value') else vars
    }
    
    freq_vars <- c("P19", "P20", "P21", "P22", "P23b", "P23c", "P24", "P24b", "P24c")
    extra_vars <- c('class', 'location_names', 'location_codes', 'activity', 'mode')
    
    df <- df %>% gather(key, val, c(vars_ref$original)) %>%
      mutate(code = ifelse(key %in% freq_vars, paste0('freq', str_pad(val, 3, pad = '0')), paste0(key, str_pad(val, 3, pad = '0'))), # create codes to match with semantic_ref codes
          value = ifelse(key == 'P4', val, mapvalues(code, from = semantic_ref$code, to = semantic_ref$desc_en, warn_missing = FALSE)), # recover the description for each aspect
          key = mapvalues(key, from = vars_ref$original, to = vars_ref$name)) %>% # rename variables to names in vars_ref
      select(-code, -val) %>%
      rename(location_codes = space, location_names = name)
    
    
    if (aggregate){ # modify it because the semantic needs to be aggregate per activity and modes classes separately
      print('Generating age ranges...')
      temp <- df %>% select(pcode, key, value) %>% 
          filter(key == 'age') %>% 
          mutate(value = as.numeric(value)) 
      temp$value <- findInterval(temp$value, c(5,18,25,35,50,65,200))
      ages_ref <- tibble(code = c(1:6), value = c("05-17", "18-24","25-34","35-49","50-64","65+"))
  
      print('Aggregating data per class...')
      final_df <- tibble()
      for (i in c('none', 'act')){
        temp_df <- df %>% 
          select(select_vars(i)) %>% 
          distinct_at(select_vars(i)) %>%
          mutate(value = ifelse(key == 'age', mapvalues(pcode, temp$pcode, temp$value, warn_missing = FALSE), value),
                 value = ifelse(key == 'age', mapvalues(value, ages_ref$code, ages_ref$value, warn_missing = FALSE), value)) %>%
          group_by_at(group_vars(i, key = FALSE)) %>%
          mutate(nb = n_distinct(pcode)) %>% # number of people per class
          group_by_at(group_vars(i, key = TRUE)) %>%
          summarise(total = n_distinct(pcode), proportion = total/mean(nb)) # proportion of people per thematic attribute 
        
        final_df <- bind_rows(final_df, temp_df)
      }
        
      final_df <- final_df %>% 
        replace(is.na(.), 'none') %>%
        rename(description = value, aspect = key, value = proportion)
    }
    else {
      print('Spreading variables...')
      df <- df %>% group_by(key) %>%
        mutate(grouped_id = row_number()) %>%
        replace(is.na(.), 'none') %>% 
        spread(key, value) %>%
        select(-grouped_id) 
      
      print ('Creating lists...')
      final_df <- apply(df, 1, function(x){
          x$activity <- unique(x$activity)
          x$mode <- unique(x$mode)
          x$location_codes = unique(x$location_codes)
          x$location_names = unique(x$location_names)
          x # to return the modified list
      })
        
    }

    return(final_df)
}


# ----------------------------------------------------------
# generate a input file for the STC
# df : sequences_df with status = 'activity' or status = 'modes'
getSTCData <- function(df){
  states <- unique(df$activity)
  print('Generating sample points for each status...')

  geopoints_df <- readRDS(getFilePath(args$rds, 'OD_points.rds'))
  
  print('Preparing data for the STC...')
  stc_df <- df %>% 
    mutate(index = as.numeric(as.factor(activity))-1) %>% # give each status an index
    left_join(geopoints_df, by = c('space'='code', 'index'='index')) # enter the geographic coordinates according to each status and location
  
  # ----------------------------------------------------------
  # duplicate each row (STC format)
  # start time at one row and end time at another
  stc_expanded <- stc_df[rep(as.factor(row.names(stc_df)), 2),] %>%
    group_by(pcode, space, start, end, activity) %>%
    mutate(index = row_number()) %>%
    arrange(pcode, start) %>%
    ungroup() %>%
    mutate(date = ifelse(index == 2, lag(end), start),
           lat = ifelse(as.character(activity) == 'none', ifelse(index == 1, lag(lat), lead(lat)), lat),
           long = ifelse(as.character(activity) == 'none', ifelse(index == 1, lag(long), lead(long)), long)) %>%
    mutate(lat = replace_na(lat, 0), long = replace_na(long, 0))
  
  stc_df <- stc_expanded %>%
    mutate(elevation = 0) %>% # the cube requires an elevation, but this is not used so we set to 0
    select(pcode, date, long, lat, elevation, activity, mode) %>%
    rename(longitude = long, latitude = lat) %>% # rename for the cube
    arrange(pcode, date) %>%
    rename(trajectory = pcode) %>% # rename for the cube
    group_by(date, longitude, latitude, activity) %>% 
    mutate(nb_trajs = n()) # count the number of trajectories per geographic coordinates
  
  file_name <- getFilePath(args$csv, paste0(args$area, '_stc', args$sample, '.csv'))
  write_csv(stc_df, file_name)
  print(paste('Saved as', file_name))
}

# -------------------------------------------------------------

toList <- function(x) (list(na.omit(x)))
getFirst <- function(x) (x[1])
getUnique <- function(x) (unique(x))

sequences_df <- readRDS(getFilePath(args$rds, paste0('sequences', args$sample, '.rds')))

file_name <- getFilePath(args$map, 'OD_sectors.shp')
print(paste('Loading shape file from', getFilePath(args$map, 'OD_sectors.shp')))
shape_df <- st_read(file_name, stringsAsFactors = FALSE) %>%
  st_transform("+proj=longlat +ellps=GRS80")

personne_df <- readRDS(getFilePath(args$rds, 'personne.rds'))
semantic_ref <- readRDS(getFilePath(args$rds, 'semantic_ref.rds'))
vars_ref <- readRDS(getFilePath(args$rds, 'semantic_vars_ref.rds'))

# for later matching, creates a code that will serve as reference
semantic_ref <- semantic_ref %>%
  mutate(code = paste0(variable, str_pad(value, 3, pad = '0')))

# --------------------------------------------------------------------
# Prepare semantic info for filtering and information on selected trajectory

persogrenoble_bis <- personne_df %>%
  mutate(pcode = getPCode(TIRA, PP2, ECH, P0)) %>%
  select(pcode, vars_ref$original) %>%
  replace(is.na(.), 0)

semantic_trajs_df <- sequences_df %>% 
  ungroup() %>%
  mutate(pcode = as.character(pcode), mode = ifelse(mode == '*', NA, mode)) %>%
  left_join(persogrenoble_bis, by = c('pcode'='pcode')) 

# generic semantic table
semantic_df <- semantic_trajs_df %>%
  mutate(space = as.numeric(space)) %>%
  left_join(shape_df, by = c('space'='code')) %>%
  mutate(name = substring(name, 1, nchar(name)-3)) %>% # delete the OD after each name
  group_by(pcode) %>%
  mutate_at(c('space', 'name', 'activity', 'mode'), toList) %>%
  mutate_at(vars_ref$original, getFirst) %>%
  select(-start, -end, -geometry) %>%
  distinct(pcode, .keep_all = TRUE) 

#------------------------------------------------------------
# transform the dates according to the STC format
stc_temp <- sequences_df %>%
  mutate(start = paste0(ifelse(start%/%60 > 24, '2019-01-02 ', '2019-01-01 '), 
                        str_pad(ifelse(start%/%60 >= 24, start%/%60 - 24, start%/%60), 2, pad = '0'),
                        ':', str_pad(start%%60, 2, pad = '0'), ':00'),
         end = paste0(ifelse(end%/%60 > 24, '2019-01-02 ', '2019-01-01 '), 
                      str_pad(ifelse(end%/%60 >= 24, end%/%60 - 24, end%/%60), 2, pad='0'), 
                      ':', str_pad(end%%60, 2, pad = '0'), ':00')) %>%
  mutate(space = as.numeric(as.character(space))) %>%
  mutate(activity = ifelse(activity == 'moving', 'none', activity), mode = ifelse(mode == '*', 'none', mode))


# load file with the reference to representative trjectories
rep_ref <- readRDS(getFilePath(args$rds, paste0('act_rep_trajs', args$sample, '.rds')))

# load file with the trajectories and their respective classes
class_ref <- readRDS(getFilePath(args$rds, paste0('act_class_ref', args$sample, '.rds')))

##-----------------------------------------
# generate the stc with coordinates for activities
getSTCData(df = stc_temp %>% select(pcode, space, start, end, activity, mode) %>% filter(pcode %in% rep_ref$pcode))

## --------------------------------------------------------------------------
# Index Plot semantic
# join class information (from previous classification of trajectories)

semantic_bis <- semantic_df %>%
  left_join(class_ref, by = c('pcode'='pcode'))
  
# aggregate semantic information per class
print('Generating aggregate semantic per class...')
file_name <- getFilePath(args$csv, paste0('class_semantic', args$sample, '.csv'))
write_csv(getSemantic(df = semantic_bis, aggregate = TRUE), file_name)
print(paste('Saved as', file_name))


# -----------------------------------------------------------
# STC semantic

# keep only the representative individuals
semantic_bis <- semantic_df %>% filter(pcode %in% rep_ref$pcode) %>% left_join(rep_ref, by = c('pcode'='pcode'))

print('Generating semantic per individual...')
file_name <- getFilePath(args$csv, paste0('stc_semantic', args$sample, '.json'))
write_json(getSemantic(df = semantic_bis, aggregate = FALSE), file_name)
print(paste('Saved as', file_name))

## ------------------------------------------------------------------------------
# Index Plot data

index_df <- sequences_df %>% left_join(class_ref, by = c('pcode'='pcode')) %>% left_join(rep_ref, by = c('pcode'='pcode', 'class'='class')) %>%
  mutate(mode = ifelse(mode == '*', 'stay', as.character(mode))) %>% # if there is no mode, the person was staying somewhere; if there is no criterion, it is not a representative sequence
  replace(is.na(.), 'none') %>%
  mutate(end = ifelse(end > 1680, 1680, end))

file_name <- getFilePath(args$csv, paste0('index_plot', args$sample, '.csv'))
write_csv(index_df, file_name)
print(paste('Saved as', file_name))

