library(fasterize)
library(raster)

# load one of the temperature rasters
temp = raster(list.files(pattern="sst_2020_12.tif",recursive=TRUE))
plot(temp)

# turn all values to 1
values(temp)[!is.na(values(temp))] = 1

# crop to eez
eez = shapefile(list.files(pattern = "eez2.shp", recursive = TRUE))
temp = mask(temp,eez2)

# write template raster
# this will be the grid used for distribution modelling and spatial planning
writeRaster(temp,"template.tif", overwrite=TRUE)
