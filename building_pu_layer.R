# planning unit creation

# South African EEZ
eez = shapefile(list.files(pattern="eez.shp", recursive = TRUE, full.names = TRUE)) # load eez

# load raster of 10 x 10 grid cells in extent
grid = raster(list.files(pattern = "template_10km.tif",full.names = TRUE,recursive = TRUE))
grid = raster(list.files(pattern = "template_5km.tif",full.names = TRUE,recursive = TRUE))

# rasterize eez getting proportion of land that covers each grid cell
pu = rasterize(eez,grid,getCover=T)
plot(pu)

# only keep cells with cover 0f 0.5 or more
values(pu)[which(values(pu)>=0.5)] = 1
values(pu)[which(values(pu)<0.5)] = NA

# check
plot(pu)

# save as template 
writeRaster(pu,"template_10km.tif")

