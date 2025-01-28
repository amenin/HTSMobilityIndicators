source('loadRDS.r')
  
#----------------------------------------------------------------------------------------------------
# stc 
data_out_path <- "data/output/"
data_in_path <- "data/input/"

sequences_df <- readRDS(paste0(rds_path, 'sequences_spell.rds'))
sequences_df <- readRDS(paste0(rds_path, 'sequences_OD.rds'))
# sequences_df <- readRDS(paste0(rds_path, 'sequences_sample.rds'))
sequences_df <- readRDS(paste0(rds_path, 'sequences_OD_sample.rds'))

stc_df <- sequences_df %>%
  mutate(start = paste0(ifelse(start%/%60 > 24, '2019-01-02 ', '2019-01-01 '), 
                        str_pad(ifelse(start%/%60 >= 24, start%/%60 - 24, start%/%60), 2, pad = '0'),
                        ':', str_pad(start%%60, 2, pad = '0'), ':00'),
         end = paste0(ifelse(end%/%60 > 24, '2019-01-02 ', '2019-01-01 '), 
                      str_pad(ifelse(end%/%60 >= 24, end%/%60 - 24, end%/%60), 2, pad='0'), 
                      ':', str_pad(end%%60, 2, pad = '0'), ':00')) %>%
  mutate(space = as.numeric(as.character(space)))

# execute to split trajectories per activity / mode of transport
stc_df <- stc_df %>% 
  mutate(index = as.numeric(as.factor(activity))-1)

#-----------------------------------------------------------------
# OD sectors

stc_df <- stc_df %>% 
  left_join(OD_points, by = c('space'='code', 'index'='index'))

# ----------------------------------------------------------
# data format for the cube
stc_expanded <- stc_df[rep(as.factor(row.names(stc_df)), 2),] %>%
  group_by(pcode, space, start, end, activity, mode) %>%
  mutate(index = row_number()) %>%
  arrange(pcode, start) %>%
  ungroup() %>%
  mutate(date = ifelse(index == 2, lag(end), start),
         lat = ifelse(as.character(activity) == 'moving', ifelse(index == 1, lag(lat), lead(lat)), lat),
         long = ifelse(as.character(activity) == 'moving', ifelse(index == 1, lag(long), lead(long)), long)) %>%
  mutate(lat = replace_na(lat, 0), long = replace_na(long, 0), mode = ifelse(mode == '*', 'none', mode))

stc_df <- stc_expanded %>%
  mutate(elevation = 0) %>%
  select(pcode, date, long, lat, elevation, activity, mode) %>%
  rename(longitude = long, latitude = lat) %>%
  arrange(pcode, date) %>%
  rename(trajectory = pcode)

#-------------------------------------------------------
# 
# # attribute random coordinates between 0 and 6 to each person
# stc_t90 <- stc_df %>%
#   left_join(t90_points, by = c('space'='code', 'index'='index')) %>%
#   select(-index)
# 
# # attribute the centroid of each region to each person
# stc_t90 <- stc_df %>%
#   left_join(t90_centroids, by = c('space'='code'))
# 
# # ----------------------------------------------------------
# # data format for the cube
# stc_t90_expanded <- stc_t90[rep(as.factor(row.names(stc_t90)), 2),] %>%
#   group_by(pcode, space, start, end, activity, mode) %>%
#   mutate(index = row_number()) %>%
#   arrange(pcode, start) %>%
#   ungroup() %>%
#   mutate(date = ifelse(index == 2, lag(end), start),
#         lat = ifelse(as.character(activity) == 'moving', ifelse(index == 1, lag(lat), lead(lat)), lat),
#         long = ifelse(as.character(activity) == 'moving', ifelse(index == 1, lag(long), lead(long)), long)) %>%
#   mutate(lat = replace_na(lat, 0), long = replace_na(long, 0), mode = ifelse(mode == '*', 'none', mode))
# 
# stc_t90 <- stc_t90_expanded %>%
#   mutate(elevation = 0, trajectory = paste0(pcode, '090')) %>%
#   select(trajectory, date, long, lat, elevation, activity, mode, space) 
# 
# write_csv(stc_t90, paste0(data_out_path, 'stc_t90.csv'))  
# 
# # t30
# 
# stc_t30 <- stc_df %>%
#   mutate(space = mapvalues(space, from = space_ref$t90, to = space_ref$t30, warn_missing = FALSE))
# 
# # attribute random coordinates
# stc_t30 <- stc_t30 %>%
#   left_join(t30_points, by = c('space'='code', 'index'='index')) %>%
#   select(-index)
# 
# # attribute the centroid of each region
# stc_t30 <- stc_t30 %>%
#   left_join(t30_centroids, by = c('space'='code'))
# 
# stc_t30_expanded <- stc_t30[rep(as.factor(row.names(stc_t30)), 2),] %>%
#   group_by(pcode, space, start, end, activity, mode) %>%
#   mutate(index = row_number()) %>%
#   arrange(pcode, start) %>%
#   ungroup() %>%
#   mutate(date = ifelse(index == 2, lag(end), start),
#          lat = ifelse(as.character(activity) == 'moving', ifelse(index == 1, lag(lat), lead(lat)), lat),
#          long = ifelse(as.character(activity) == 'moving', ifelse(index == 1, lag(long), lead(long)), long)) %>%
#   mutate(lat = replace_na(lat, 0), long = replace_na(long, 0), mode = ifelse(mode == '*', 'none', as.character(mode)))
# 
# stc_t30 <- stc_t30_expanded %>%
#   mutate(elevation = 0, trajectory = paste0(pcode, '030')) %>%
#   select(trajectory, date, long, lat, elevation, activity, mode, space)
# 
# write_csv(stc_t30, paste0(data_out_path, 'stc_t30.csv'))  
# 
# 
# # t10
# 
# stc_t10 <- stc_df %>%
#   mutate(space = mapvalues(space, from = space_ref$t90, to = space_ref$t10, warn_missing = FALSE))
# 
# # attribute random coordinates
# stc_t10 <- stc_t10 %>%
#   left_join(t10_points, by = c('space'='code', 'index'='index')) %>%
#   select(-index)
# 
# # attribute the centroid of each region
# stc_t10 <- stc_t10 %>%
#   left_join(t10_centroids, by = c('space'='code'))
# 
# stc_t10_expanded <- stc_t10[rep(as.factor(row.names(stc_t10)), 2),] %>%
#   group_by(pcode, space, start, end, activity, mode) %>%
#   mutate(index = row_number()) %>%
#   arrange(pcode, start) %>%
#   ungroup() %>%
#   mutate(date = ifelse(index == 2, lag(end), start),
#          lat = ifelse(as.character(activity) == 'moving', ifelse(index == 1, lag(lat), lead(lat)), lat),
#          long = ifelse(as.character(activity) == 'moving', ifelse(index == 1, lag(long), lead(long)), long)) %>%
#   mutate(lat = replace_na(lat, 0), long = replace_na(long, 0), mode = ifelse(mode == '*', 'none', as.character(mode)))
# 
# stc_t10 <- stc_t10_expanded %>%
#   mutate(elevation = 0, trajectory = paste0(pcode, '010')) %>%
#   select(trajectory, date, long, lat, elevation, activity, mode, space)
# 
# write_csv(stc_t10, paste0(data_out_path, 'stc_t10.csv'))  
# 
# #-----------------------------------------------------
# # Merging the stc dat for all territorial partitions
# 
# # stc_df <- rbind(stc_t10, stc_t30, stc_t90) %>%
# #   rename(longitude = long, latitude = lat)


# run if the centroids were used
# stc_df <- stc_df %>%
#   group_by(longitude, latitude) %>%
#   mutate(nb_trajs = n())

# run if the random points were used
stc_df <- stc_df %>%
  group_by(longitude, latitude, activity) %>%
  mutate(nb_trajs = n())

# write_csv(stc_df, paste0(data_out_path, 'stc.per.activity.csv'))
write_csv(stc_df, paste0('data/output/stc_sample.csv'))

#--------------------------------------------------------
# Preparing the semantic info for filtering and information display

# class_ref <- readRDS(paste0(rds_path, 'class_sample_ref.rds'))
class_ref <- readRDS(paste0(rds_path, 'class_ref.rds'))

persogrenoble <- readRDS(paste0(rds_path, 'persogrenoble.rds'))

persogrenoble_sum <- persogrenoble %>%
  mutate(pcode = getPCode(TIRA, PP2, ECH, P0)) %>%
  select(pcode, P2, P4, P9) %>%
  mutate(P2 = ifelse(P2 == 1, 'male', 'female'),
         P9 = mapvalues(P9, from = occupation_ref$code, to = occupation_ref$occupation)) %>%
  rename(sex = P2, age = P4, occupation = P9)

semantic_trajs_df <- sequences_df %>% 
  ungroup() %>%
  mutate(pcode = as.character(pcode), mode = ifelse(mode == '*', NA, mode)) %>%
  left_join(persogrenoble_sum, by = c('pcode'='pcode')) %>%
  left_join(class_ref, by = c('pcode'='pcode')) 

semantic_OD <- semantic_trajs_df %>%
  mutate(space = as.numeric(space)) %>%
  left_join(OD_centroids, by = c('space'='code')) %>%
  select(-lat, -long, -start, -end) 

# semantic_t90 <- semantic_trajs_df %>%
#   mutate(space = as.numeric(space)) %>%
#   left_join(t90_centroids, by = c('space'='code')) %>%
#   mutate(trajectory = paste0(pcode, '090'), partition = 't90') %>%
#   select(-lat, -long, -start, -end, -pcode) 
# 
# semantic_t30 <- semantic_trajs_df %>%
#   mutate(space = as.numeric(space)) %>%
#   mutate(space = mapvalues(space, from = space_ref$t90, to = space_ref$t30, warn_missing = FALSE)) %>%
#   left_join(t30_centroids, by = c('space'='code')) %>%
#   mutate(trajectory = paste0(pcode, '030'), partition = 't30') %>%
#   select(-lat, -long, -start, -end, -pcode) 
# 
# semantic_t10 <- semantic_trajs_df %>%
#   mutate(space = as.numeric(space)) %>%
#   mutate(space = mapvalues(space, from = space_ref$t90, to = space_ref$t10, warn_missing = FALSE)) %>%
#   left_join(t10_centroids, by = c('space'='code')) %>%
#   mutate(trajectory = paste0(pcode, '010'), partition = 't10') %>%
#   select(-lat, -long, -start, -end, -pcode, -area_sqm, -area_sqkm) 

# semantic_df <- rbind(semantic_t10, semantic_t30, semantic_t90) %>%
semantic_df <- semantic_OD %>%
  group_by(pcode) %>%
  group_modify(~tibble(
    class = .x$class[1],
    age = .x$age[1],
    sex = .x$sex[1],
    occupation = .x$occupation[1],
    modes = list(unique(na.omit(.x$mode))),
    activities = list(unique(.x$activity)),
    locations_codes = list(unique(na.omit(.x$space))),
    locations_names = list(unique(na.omit(.x$name)))
  )) %>%
  arrange(pcode) %>%
  rename(trajectory = pcode)

write_json(semantic_df, paste0(app_path, 'semantic_stc.json'))
# write_csv(stc_individuals, paste0(app_path, 'stc.csv'))

# ----------------------------------------------------------------
# trajectories for the index plot

index_df <- sequences_df %>%
  left_join(class_ref, by = c('pcode'='pcode')) %>%
  mutate(space = as.numeric(as.character(space))) %>%
  select(-space) %>%
  mutate(mode = ifelse(mode == '*', 'stay', as.character(mode)))

write_csv(index_df, paste0(app_path, 'index_plot.csv'))

# -----------------------------------------------------------------
# summary of individuals' aspects

getSummarisedIndividuals <- function(classe){
  temp <- semantic_trajs_df %>%
    distinct(pcode, .keep_all = TRUE)
    # mutate(hcode = substr(pcode, 0, 10)) %>%
    # left_join(coem, by = c('hcode'='hcode'))
  
  # the minimum age needs be changed according to the data
  temp$agegroup <- findInterval(temp$age, c(18,25,35,50,65,200))
  temp <- temp %>% 
    mutate(agegroup = as.factor(agegroup))
  levels(temp$agegroup) <- c("18-24","25-34","35-49","50-64","65+")
  
  if (classe != 'none')
    temp <- temp %>% filter(class == classe)
  
  nbSeq <- temp %>%
    summarise(nb = n())
  nbSeq <- nbSeq$nb[1]
  
  occupation <- temp %>%
    group_by(occupation) %>%
    summarise(total = n(), value = total/nbSeq) %>%
    rename(description = occupation) %>%
    mutate(aspect = 'occupation')
  
  age <- temp %>%
    group_by(agegroup) %>%
    summarise(total = n(), value = total/nbSeq) %>%
    rename(description = agegroup) %>%
    mutate(aspect = 'age', description = as.character(description))
    
  sex <- temp %>%
    group_by(sex) %>%
    summarise(total = n(), value = total/nbSeq) %>%
    rename(description = sex) %>%
    mutate(aspect = 'sex')
  
  bind_rows(occupation, age, sex) %>% mutate(class = classe)
}

classes <- unique(class_ref$class)
individuals_summarised <- getSummarisedIndividuals('none')
for (c in classes){
  temp <- getSummarisedIndividuals(c)
  individuals_summarised <- rbind(individuals_summarised, temp)
}

write_csv(individuals_summarised, paste0(app_path, 'individuals_info.csv'))
