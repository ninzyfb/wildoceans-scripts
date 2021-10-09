# Set directory
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling")
getwd()

# Packages
library(dplyr)
library(raster)
library(rgdal)
library(rgeos)
library(gstat)
library(rgl)
library(rasterVis)

# Data
template = raster("template.tif")
soundings = shapefile(list.files(pattern="SANHO_soundings_all.shp",recursive=TRUE))
contours = shapefile(list.files(pattern="depth.shp",recursive=TRUE))
crs(soundings) = crs(template)
crs(contours) = crs(template)


plot(contours)

