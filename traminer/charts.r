#!/usr/bin/env Rscript

source('utils.r')

parser <- setParser('Script to Generate Charts of the Trajectories Classification')
parser$add_argument('-types', type = 'character', default = c('d', 'f', 'I', 'Ht', 'ms', 'mt', 'pc', 'r'), nargs = '*', help = 'the chart types to generate', choices = c('d', 'f', 'I', 'Ht', 'ms', 'mt', 'pc', 'r'))
parser$add_argument('-criterion', type = 'character', default = c('freq', 'density', 'dist', 'prob'), nargs = '*', help = 'the criterion for computing representative sequences', choices = c('freq', 'density', 'dist', 'prob'))
parser$add_argument('-coverage', type = 'double', default = 0.25, nargs = 1, help = 'the coverage for representative sequences')

args <- parser$parse_args()

colors <- getColors()

loadPackages(c('dplyr', 'TraMineR', 'plyr'))

setPlotsFolder()

savePlot <- function(file_name, type, cpal, criterion = NULL){

  if (args$device == 'png') png(file_name, width = 1500, height = 800)
  else postscript(file_name, width = 1500, height = 800)   
  
  if (!is.null(criterion)) 
      seqplot(seq, type = type, criterion = criterion, diss = seq.dhd, coverage = args$coverage, group=part.dhd, xtlab=240:1740, border=NA, with.legend=T, axes = 'bottom', cpal = cpal, xtstep = 60, xaxis = TRUE)
  else 
      seqplot(seq, type = type, group=part.dhd, xtlab=240:1740, border=NA, with.legend=T, axes = 'bottom', cpal = cpal, pbarw=TRUE, xtstep = 60, xaxis = TRUE)    
  
  dev.off()
  print(paste0('Saved as: ', file_name))
}

print(paste('Generating charts for', ifelse(args$event == 'act', 'activity-based', 'modes-based'), 'sequences...'))
# load the sequences
file_name <- getFilePath(args$rds, paste0(args$event,'_seq', args$sample, '.rds'))
print(paste('Loading sequences from', file_name))
seq <- readRDS(file_name)

# load already computed distance matrix
file_name <- getFilePath(args$rds, paste0(args$event, '_dhd', args$sample, '.rds'))
print(paste('Loading distance matrix from', file_name, '...'))
seq.dhd <- readRDS(file_name)

# load the hierarchical clustering 
file_name <- getFilePath(args$rds, paste0(args$event, '_agnes_dhd_ward', args$sample,'.rds'))
print(paste('Loading hierarchical clustering from', file_name, '...'))
agnes.dhd <- readRDS(file_name)

for (nbcl in args$class){
    print(paste('Creating charts for', nbcl, 'classes...'))
    part.dhd <- cutree(agnes.dhd, nbcl)
    part.dhd <- factor(part.dhd,labels=paste('classe',1:nbcl,sep='.'))
    
    for (t in args$types){
        print(paste('Chart type:', t))
        if (t != 'r'){
            savePlot(getFilePath(args$plots, paste0('type_', t, '_', args$event, '_', nbcl, '_classes', args$sample, '.', args$device)), type = t, cpal = colors[[args$event]])
        }
        else{
            for (c in args$criterion){
                print(paste('Finding representative sequences through criterion:', c))
                savePlot(getFilePath(args$plots, paste0('type_', t, '_crit_', c, '_', args$event, '_', nbcl, '_classes', '_cov_', args$coverage, args$sample, '.', args$device)), type = t, cpal = colors[[args$event]], criterion = c)
            }                    
        }
    }
}    





