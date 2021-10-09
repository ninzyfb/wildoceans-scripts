# load environmental data
depth = raster(list.files(pattern = "bathymtery_deWetandGEBCO.tif", recursive = TRUE))

# plot
plot(depth)

# calculate slope
library(raster)
slope = terrain(depth, opt = "slope")

# plot
plot(slope)

# save
writeRaster(slope, "slope.tif")
