# Set directory
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/environmental_variables/")

# Packages
library(raster)

# template raster
t = raster(list.files(pattern="template.tif", recursive = TRUE))

# calculate mean SST over entire years
files = list.files(pattern = "sst_", recursive = TRUE)
stack = stack(files)
sst_average =  calc(stack, fun = mean)
plot(sst_average2)
sst_average2 = sst_average - 273.15
writeRaster(sst_average2, "sst_average.tif")

# calculate maximum monthly mean SST over entire years
sst_max = calc(stack, fun = max)
sst_max = sst_max - 273.15
# calculate minimum monthly mean SST over entire years
sst_min = calc(stack, fun = min)
sst_min = sst_min - 273.15


# calculate range
sst_range = (sst_max - sst_min) 
sst_range2 = ((sst_max - sst_min)*100)/sst_max
plot(sst_range)
plot(sst_range2)
writeRaster(sst_range, "sst_range.tif")

plot(sst_range)
