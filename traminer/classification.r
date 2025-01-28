#!/usr/bin/env Rscript

source('utils.r')

# get optional arguments
parser <- setParser('Script to Generate the Distance Matrix and the Classification Dendogram')
args <- parser$parse_args()

loadPackages(c('cluster', 'TraMineR', 'tidyverse'))

labels <- getLabels()

file_name <- getFilePath(args$rds, 'sequences.rds')
print(paste('Loading sequences from', file_name))
sequences_df <- readRDS(file_name)

setPlotsFolder()

# sample data as required by the sample argument
if (length(args$sample) > 1){
    print('Sampling data...')
    
    if (args$sample == '_car_users') {
        # sample dataset (contraints voiture) 480 trajectories
        file_name <- paste0(args$extra_path, 'contraints_voiture.csv')
        voiture_df <- read.csv(file_name)
        
        voiture_df <- voiture_df %>% mutate(pcode = getPCode(TIRA, OP2, ECH, P0))
        print(summary(voiture_df))
        sequences_df <- subset(sequences_df, pcode %in% voiture_df$pcode)
        print(summary(sequences_df))
    }else{
        # sample dataset based on the previous classification on the whole dataset

        parts <- strsplit(args$sample, '_')[[1]]

        # load the classes extracter from the whole dataset
        class_ref <- readRDS(paste0(args$data_path, parts[2], '_class_ref.rds'))

        # select the given classes
        classes <- paste('classe', strsplit(parts[3], '')[[1]], sep = '.')

        keep <- class_ref %>% filter(class %in% classes)
        sequences_df <- subset(sequences_df, pcode %in% keep$pcode)
    }
} 

sequences_df <- as.data.frame(sequences_df) %>% # traminer only deals with the data.frame format
  filter(start <= end) # to avoid inconsistencies

print(paste0('Classifying ', ifelse(args$event == 'act', 'activity', 'mode'), '-based sequences...'))
seq.spell <- if(args$event == 'act') sequences_df %>% select(pcode, activity, start, end) %>% dplyr::rename(status = activity) else 
    sequences_df %>% select(pcode, mode, start, end) %>% mutate(mode = ifelse(mode == '*', 'stay', mode)) %>% dplyr::rename(status = mode)

seq.labels <- labels[[args$event]]
seq.scodes <- 1:length(seq.labels)

# transform the data for entering the traminer functions
seq.sts <- seqformat(seq.spell, from = 'SPELL', to = 'STS', id = 'pcode', begin = 'start', end = 'end', status = 'status', process = FALSE)

# create and write the sequences in the traminer format
print('Transforming sequences into STS format...')
seq <- seqdef(seq.sts, alphabet = seq.labels, states = seq.scodes, labels = seq.labels, xtstep = 10)

file_name <- getFilePath(args$rds, paste0(args$event,'_seq', args$sample, '.rds'))
saveRDS(seq, file_name)
print(paste('Saved as', file_name))

# distance matrix
print(paste('Computing Distance Matrix...'))
seq.dhd <- seqdist(seq, method = "DHD")

file_name <- getFilePath(args$rds, paste0(args$event, '_dhd', args$sample, '.rds'))
saveRDS(seq.dhd, file_name)
print(paste('Saved as', file_name))

# agnes
print(paste('Computing Agglomerative Nesting (Hierarchical Clustering)...'))
seq.agnes.dhd <- agnes(as.dist(seq.dhd), method="ward", keep.diss=FALSE)

file_name <-  getFilePath(args$rds, paste0(args$event, '_agnes_dhd_ward', args$sample, '.rds'))
saveRDS(seq.agnes.dhd, file_name)
print(paste('Saved as', file_name))

# dendogram
file_name <- getFilePath(args$plots, paste0('/dendogram_', args$event, args$sample, '.ps'))
postscript(file_name, width = 1500, height = 800)
plot(as.dendrogram(seq.agnes.dhd), leaflab='none')
dev.off()
print(paste('Saved as', file_name))
