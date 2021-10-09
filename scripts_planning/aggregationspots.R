# this script is to prepare the various aggregation spot rasters

# packages
library(raster)
library(dplyr)
library(sf)
library(fasterize)

# template raster
template = raster(list.files(pattern = "template.tif"))

# 1 - estuaries

# load data
estuaries_raw = read.csv(list.files(pattern = "estuaries",recursive=TRUE))
table(estuaries_raw$Estuary_class.)
# classify as factor
estuaries_raw$Estuary_class. = as.factor(estuaries_raw$Estuary_class.)
estuaries = estuaries_raw %>%
  filter(as.integer(Estuary_class.)==1)
# add spatial info
estuaries_sf = st_as_sf(estuaries,coords = c("Longitude","Latitude"))
estuaries_sf$id = 1
# rasterize
estuaries_raster = rasterize(estuaries_sf,template,field = "id")
plot(estuaries_raster)
writeRaster(estuaries_raster,"estuaries_planninglayers.tif")

# 2 aggregation spots - estuaries

# load data
spots_raw = shapefile(list.files(pattern = "aggregationspots.shp",recursive=TRUE))
# add spatial info
spots_raw_sf = st_as_sf(spots_raw)
spots_raw_sf$id = 1
plot(spots_raw_sf)
spots = as(spots_raw_sf,Class = "Spatial")
# rasterize
spots_raw_raster = rasterize(spots_raw,template)
plot(spots_raw_raster)
writeRaster(estuaries_raster,"estuaries_planninglayers.tif")

cellFromXY(template,coordinates(spots))

table(values(spots_raw_raster))


