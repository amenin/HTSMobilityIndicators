#!/usr/bin/env Rscript

source('utils.r')

parser <- setParser('Script to extract centroids from ESRI Shapefiles and save it as GeoJSON files')

parser$add_argument('-prefix', type="character", nargs = 1, help='prefix of files (the partitions)')

args <- parser$parse_args()

loadPackages(c('sf'))

file_name <- getFilePath(args$shape, paste0(args$prefix, '_sectors.shp'))
print(paste("Reading ESRI Shapefile from", file_name))

sf <- st_read(file_name) %>%
  st_transform("+proj=longlat +ellps=GRS80") 
centroids <- st_centroid(sf, byid = TRUE)

file_name <- getFilePath(args$shape, paste0(args$prefix, '_centroids.geojson'))
st_write(centroids, file_name, driver = 'GeoJSON', delete_dsn = TRUE)
print(paste('Saved as', file_name))