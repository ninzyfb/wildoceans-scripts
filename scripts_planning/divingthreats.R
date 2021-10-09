getwd()

library(raster)
library(sf)
library(dplyr)

# load template layer
eez = raster("template.tif")

# load diving spreadsheet
sites  = shapefile(list.files(pattern = "cagediving.shp",recursive = TRUE))

# add spatial info
sites_sf = st_as_sf(sites)
sites_sf$id = c(1,1,1,1)

# isolate cells in which you can find shark cage diving
cage_raster = rasterize(sites_sf,eez,field = "id")
writeRaster(cage_raster,"cagediving.tif")


