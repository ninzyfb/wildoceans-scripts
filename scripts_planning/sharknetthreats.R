getwd()

library(raster)
library(sf)
library(dplyr)

# load template layer
eez = raster("template.tif")

# load diving spreadsheet
nets  = shapefile(list.files(pattern = "sharknetinfluence.shp",recursive = TRUE))

# isolate cells in which you can find shark nets 
nets_raster = rasterize(nets,eez)
writeRaster(nets_raster,"sharknets.tif")
