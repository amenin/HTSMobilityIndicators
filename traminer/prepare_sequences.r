setwd("~/Documents/mobility_indicators/traminer")

source('utils.r')

# get optional arguments
parser <- setParser('Script to prepare data as activity and modes sequences.')

args <- parser$parse_args()
args$area <- 'lyon'

# ----------------------------------------------------------------
# Transportation means
# Considers the number of rides per transportation mean

loadFile <- function(file_name){
  file_path <- getFilePath(args$rds, file_name)
  print(paste('Loading data from', file_path))
  readRDS(file_path)
}

deplaexpanded_df <- loadFile('deplaexpanded.rds')
deplatraj_df <- loadFile('deplatraj.rds')
activity_ref <- loadFile('activity_ref.rds') %>%
  mutate(code = as.numeric(code))
modes_ref <- loadFile('modes_ref.rds') %>%
  mutate(code = as.numeric(code))

# Time goes from 4 to 28 hours
deplamodes_df <- deplatraj_df %>%
  mutate(mode = mapvalues(T3, from = modes_ref$code, to = modes_ref$desc_en)) %>%
  mutate(mode = replace_na(mode, 'walk')) %>%
  select(hcode, pcode, mode, D4A, D4B, D8A, D8B) %>%
  mutate(start = D4A * 60 + D4B, start = ifelse(start > 1740, 1740, start), end = D8A * 60 + D8B, end = ifelse(end > 1740, 1740, end)) %>%
  select(pcode, start, end, mode) 

# -----------------------------------------------------------------
# Activities
# Consider the number of displacements per activity

deplact_df <- deplaexpanded_df %>%
  mutate(activity = mapvalues(D5, from = as.numeric(activity_ref$code), to = activity_ref$desc_en)) %>%
  group_by(pcode) %>%
  mutate(index = row_number()) %>%
  mutate(start = D8A * 60 + D8B,
         start = ifelse(start > 1740, 1740, start),
         end = ifelse(is.na(lead(index)) | lead(index) == 1, 29*60, lead(D4A) * 60 + lead(D4B)),
         end = ifelse(end > 1740, 1740, end)) %>%
  rename(space = D7) %>%
  select(pcode, space, start, end, activity)

# ----------------------------------------------------------------------
# All activities and modes of transport combined

sequences_df <- bind_rows(deplact_df, deplamodes_df) %>%
  mutate(activity = ifelse(is.na(activity), 'moving', activity),
         space = ifelse(is.na(space), '*', as.character(space)),
         mode = ifelse(is.na(mode), '*', mode)) %>%
  arrange(pcode, start, end) %>%
  filter(end <= 1740)

file_name <- getFilePath(args$rds, 'sequences.rds')
saveRDS(sequences_df, file_name)
print(paste('Saved as', file_name))

# for external inspection
file_name <- getFilePath(args$csv, 'sequences.csv')
write_csv(sequences_df, file_name)
print(paste('Saved as', file_name))
