#!/usr/bin/env Rscript

source('utils.r')

parser <- setParser('Script to generate a class referential file.')

args <- parser$parse_args()

loadPackages(c('tidyverse', 'TraMineR', 'plyr'))

# temporary
# args$area <- 'lyon'
# args$class <- 3

print(paste0('Classifying the ', ifelse(args$event == 'act', 'activity', 'mode'), '-based sequences...'))

file_name <- getFilePath(args$rds, paste0(args$event, '_seq', args$sample, '.rds'))
print(paste('Loading sequences from', file_name, '...'))
seq <- readRDS(file_name)

# load the hierarchical clustering 
file_name <- getFilePath(args$rds, paste0(args$event, '_agnes_dhd_ward', args$sample, '.rds'))
print(paste('Loading hierarchical clustering from', file_name, '...'))
seq.agnes <- readRDS(file_name)

print(paste('Extracting', args$class, 'classes...'))
part.dhd <- cutree(seq.agnes, nbcl)
part.dhd <- factor(part.dhd,labels=paste('classe', 1:nbcl,sep='.'))

order <- seq.agnes$order 
classes <- as.character(part.dhd)
pcodes <- row.names(seq)

class_ref <- tibble(pcode = pcodes[order], class = classes[order])

# save file as RDS for loading later on other scripts
file_name <- getFilePath(args$rds, paste0(args$event, '_class_ref', args$sample, '.rds'))
saveRDS(class_ref, file_name)
print(paste('Saved as', file_name))    

# save file as CSV for external inspection
file_name <- getFilePath(args$csv, paste0(args$event, '_class_ref', args$sample, '.csv'))
write_csv(class_ref, file_name)
print(paste('Saved as', file_name))    
