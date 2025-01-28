#!/usr/bin/env Rscript
setwd("~/Documents/MobilityRScripts")

source('packages.R')

getPCode <- function(val1, val2, val3, val4){
  paste0(val1, str_pad(val2, 3, pad = '0'), str_pad(val3, 4, pad = '0'), str_pad(val4, 2, pad = '0'))
}

getHCode <- function(val1, val2, val3){
  paste0(val1, str_pad(val2, 3, pad = '0'), str_pad(val3, 4, pad = '0'))
}

rds_path <- "data/rds/"
app_path <- '../estime/WebContent/data/'

# territorial partition data
OD_sf <- readRDS(paste0(rds_path, 'OD_shape.rds'))
OD_centroids <- readRDS(paste0(rds_path, 'OD_centroids.rds'))
OD_points <- readRDS(paste0(rds_path, 'OD_sample_points.rds'))

t90_sf <- readRDS(paste0(rds_path, 't90_shape.rds'))
t90_centroids <- readRDS(paste0(rds_path, 't90_centroids.rds'))
t90_points <- readRDS(paste0(rds_path, 't90_sample_points.rds'))
t30_sf <- readRDS(paste0(rds_path, 't30_shape.rds'))
t30_centroids <- readRDS(paste0(rds_path, 't30_centroids.rds'))
t30_points <- readRDS(paste0(rds_path, 't30_sample_points.rds'))
t10_sf <- readRDS(paste0(rds_path, 't10_shape.rds'))
t10_centroids <- readRDS(paste0(rds_path, 't10_centroids.rds'))
t10_points <- readRDS(paste0(rds_path, 't10_sample_points.rds'))

# reference files
space_ref <- readRDS(paste0(rds_path, 'space_ref.rds'))
activity_ref <- readRDS(paste0(rds_path, 'activity_ref.rds'))
modes_ref <- readRDS(paste0(rds_path, 'modes_ref.rds'))
occupation_ref <- readRDS(paste0(rds_path, 'occupation_ref.rds'))
coem <- readRDS(paste0(rds_path, 'coem.rds'))

# datasets (not sure if this is being used in the other scripts)

deplagrenoble_OD <-readRDS(paste0(rds_path, 'OD_deplagrenoble.rds'))
deplagrenoble_t90 <-readRDS(paste0(rds_path, 't90_deplagrenoble.rds'))
deplagrenoble_t30 <-readRDS(paste0(rds_path, 't30_deplagrenoble.rds'))
deplagrenoble_t10 <-readRDS(paste0(rds_path, 't10_deplagrenoble.rds'))

# population per sector

people_t90 <- readRDS(paste0(rds_path, 't90_people.rds'))
people_t30 <- readRDS(paste0(rds_path, 't30_people.rds'))
people_t10 <- readRDS(paste0(rds_path, 't10_people.rds'))

# expanded datasets

deplaexpanded_OD <- readRDS(paste0(rds_path, 'OD_deplaexpanded.rds'))
deplaexpanded_t90 <- readRDS(paste0(rds_path, 't90_deplaexpanded.rds'))
deplaexpanded_t30 <- readRDS(paste0(rds_path, 't30_deplaexpanded.rds'))
deplaexpanded_t10 <- readRDS(paste0(rds_path, 't10_deplaexpanded.rds'))

deplatraj_OD <- readRDS(paste0(rds_path, 'OD_deplatraj.rds'))
deplatraj_t90 <- readRDS(paste0(rds_path, 't90_deplatraj.rds'))
deplatraj_t30 <- readRDS(paste0(rds_path, 't30_deplatraj.rds'))
deplatraj_t10 <- readRDS(paste0(rds_path, 't10_deplatraj.rds'))