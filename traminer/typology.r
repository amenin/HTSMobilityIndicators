#!/usr/bin/env Rscript

# This script find the representative sequences for each of previous defined classes

source('utils.r')

parser <- setParser('Script to Recover the Representative Sequences of Each Class')
parser$add_argument('-criterion', type = 'character', default = c('freq', 'density', 'dist', 'prob'), nargs = '*', help = 'the criterion for computing representative sequences', choices = c('freq', 'density', 'dist', 'prob'))
parser$add_argument('-coverage', type = 'double', default = 0.25, nargs = 1, help = 'the coverage for representative sequences')

args <- parser$parse_args()

# temporary
args$area <- 'lyon'
args$class <- 3

loadPackages(c('cluster', 'TraMineR', 'TraMineRextras', 'tidyverse', 'plyr'))

#-------------------------------------
# select representative trajectories (pcode)

print(paste0('Getting the representative ', ifelse(args$event == 'act', 'activity', 'modes'), '-based sequences...'))

seq <- readRDS(getFilePath(args$rds, paste0(args$event, '_seq', args$sample, '.rds')))
seq.dhd <- readRDS(getFilePath(args$rds, paste0(args$event, '_dhd', args$sample, '.rds')))
agnes.dhd <- readRDS(getFilePath(args$rds, paste0(args$event, '_agnes_dhd_ward', args$sample, '.rds')))

print(paste(args$class, 'classes...'))

part.dhd <- cutree(agnes.dhd, args$class)
part.dhd <- factor(part.dhd,labels=paste('classe', 1:args$class, sep='.'))

rep_trajs <- tibble()
for (c in args$criterion){
  print(paste('Finding representative individuals for criterion', c))

  # this returns a list with one data frame per class, which rows correspond to representative sequences
  rep.list <- seqrep.grp(seq, diss = seq.dhd, criterion = c, ret = 'rep', group = part.dhd)

  classes <- names(rep.list) # recover the existing classes
  for (cls in classes){
      df <- as.data.frame(rep.list[[cls]])
      for (i in 1:nrow(df)){
          row <- match_df(seq, df[i,]) # find the representative sequence in the set of sequences
          rep_trajs <- bind_rows(rep_trajs, tibble(class = cls, pcode = row.names(row), criterion = c)) # row.names gives the pcode, since in act.seq the row names correspond to the pcode variable
      }
  }
}

# rds format to load into other scripts
file_name <- getFilePath(args$rds, paste0(args$event, '_rep_trajs', args$sample, '.rds'))
saveRDS(rep_trajs, file_name)
print(paste('Saved as', file_name))

# csv for external inspection
file_name <- getFilePath(args$csv, paste0(args$event, '_rep_trajs', args$sample, '.csv'))
write_csv(rep_trajs, file_name)
print(paste('Saved as', file_name))

      