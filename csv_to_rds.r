#!/usr/bin/env Rscript

source('utils.r')

loadPackages(c('readr'))

parser <- setParser('Convert csv into rds files')

parser$add_argument('-paths', type="character", nargs = 2, help='paths to input and output files')

args <- parser$parse_args()

csv <- read_csv(paste0(args$paths[1]))
saveRDS(csv, args$paths[2])
print(paste('Saved as', args$paths[2]))

