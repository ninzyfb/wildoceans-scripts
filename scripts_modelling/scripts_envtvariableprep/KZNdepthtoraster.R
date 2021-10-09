setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling") # set directory to parent modelling folder

# Packages
library(sf)
library(dplyr)
library(fasterize)

# Data
kzn = st_read(list.files(pattern = "SDRZ_BRUV_Isobaths.shp", recursive = TRUE))
template = raster(list.files(pattern = "template.tif", recursive = TRUE))
crs(kzn) # check crs

# rasterize using fasterize
r = raster(kzn, res = 0.001)
kzn$VALUE = as.factor(kzn$VALUE)
test = fasterize(kzn, r, field = "VALUE", fun = 'sum')
plot(test)
writeRaster(test,"test.tif")
