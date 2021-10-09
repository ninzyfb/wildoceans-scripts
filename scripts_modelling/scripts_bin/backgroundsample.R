##### Directory
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling") # set to modelling folder
getwd()

##### Packages
library(dismo)
library(sf)

##### Create a mask layer
# this is important to make sure that when you select backgroudn points later
# that you only select them from within the mask (and not land basically)
mask = raster(list.files(pattern = "template.tif", recursive = TRUE))

##### Create random background points in extent of occurrence data

# identify grid cells in mask in which you have an occurrence point
cells = cellFromXY(mask,pts_subset)

# turn those cells to NA 
# prevents background points being picked from occurrence cells
values(mask)[cells] = NA

# select same amount of random points as data points
# set seed to assure that the examples will always have the same random sample
# sample from geographic extent of occurrences
pts_background = SpatialPoints(randomPoints(mask, length(pts_subset)))
# subset background 
pts_background = SpatialPoints(gridSample(pts_background, mask, n=1))

# plot
plot(pts_subset)
points(pts_background)

rm(mask,cells) # remove unnecessary variables
