#!/usr/bin/env Rscript

source('utils.r')

parser <- setParser('Convert ESRI Shapefiles into GeoJSON files')

parser$add_argument('-prefix', type="character", nargs = 1, help='prefix of files (the partitions)')

args <- parser$parse_args()

loadPackages(c('sf'))

file_name <- getFilePath(args$shape, paste0(args$prefix, '_sectors.shp'))
print(paste("Reading ESRI Shapefile from", file_name))

sf <- st_read(file_name) %>%
  st_transform("+proj=longlat +ellps=GRS80") 

file_name <- getFilePath(args$shape, paste0(args$prefix, '_sectors.geojson'))
st_write(sf, file_name, driver = 'GeoJSON', delete_dsn = TRUE)
print(paste('Saved as', file_name))
