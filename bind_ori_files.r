source('utils.r')

parser <- setParser('Prepare files for indicators')
parser$add_argument('-files', nargs = 2, type = 'character', help = 'files to bind together')
parser$add_argument('-filename', type = 'character', help='name of the final file')

args <- parser$parse_args()

loadPackages(c('foreign', 'haven', 'tidyverse'))

# args$area <- 'lyon'
# args$files <- c('original/lyon_2015_ori_faf_depl.sav', 'original/lyon_2015_ori_tel_depl.sav')
# args$filename <- 'deplacement.csv'

file_name <- getFilePath(args$spss, args$files[1])
print(paste('Reading dataset 1 from', file_name))
df1 <- read.spss(file_name, to.data.frame=TRUE) %>%
  mutate_all(factorToNumeric)

file_name <- getFilePath(args$spss, args$files[2])
print(paste('Reading dataset 2 from', file_name))
df2 <- read.spss(file_name, to.data.frame=TRUE) %>%
  mutate_all(factorToNumeric)


file_name <- getFilePath(args$spss, args$filename)
write_sav(bind_rows(df1, df2), file_name)
print(paste('Saved as', file_name))
