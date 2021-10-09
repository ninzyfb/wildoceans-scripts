# Set directory
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling")
getwd()

# Packages
library(raster)
library(sf)

# Template raster
# raster from which the extent and resolution will be taken from for the 5km models
# This is one of the daily SST rasters
template = raster(list.files(pattern = "OSTIA_SST_5KM_20070102.tif", recursive = TRUE))

# simplify values (not necessary, but just for me)
values(template)[!is.na(values(template))] = 1
table(values(template))

# Find lat lon for each cell
cells = Which(!is.na(template), cells = TRUE)
coords = xyFromCell(template, cells)
lon = coords[,1]
lat = coords[,2]

# create rasters
longitude = template
latitude = template
values(longitude)[!is.na(values(longitude))] = lon
values(latitude)[!is.na(values(latitude))] = lat

writeRaster(longitude, "longitude.tif")
writeRaster(latitude, "latitude.tif")

