library(raster)

# data
bathy1 = raster(list.files(path = "/Users/nfb/Dropbox/6-WILDOCEANS/",pattern = "bathymetry_deWet.grd",recursive = TRUE, full.names = TRUE))

# Template raster
# raster from which the extent and resolution will be taken from for the 5km models
# This is one of the daily SST rasters
template = raster(list.files(path = "/Users/nfb/Dropbox/6-WILDOCEANS/",pattern = "template.tif", recursive = TRUE, full.names = TRUE))
plot(template)
crs(template)

# set crs
crs(bathy1) = crs(template)

# re-project bathymetry
bathy2 = projectRaster(bathy1,template, method = 'bilinear') # project the raster to the template

# plot
plot(bathy2)

# now load GEBCO bathymetry
gebco1 = raster(list.files(pattern = "bathymetry-GEBCO.tif", recursive = TRUE))
# re-project bathymetry
gebco2 = projectRaster(gebco1,template, method = 'bilinear') # project the raster to the template
rm(gebco1)

# both bathymtry plots overlap each other
plot(gebco2)
plot(bathy2, add = TRUE)

# crop gebco raster using bathy2
  # first extend extent of bathy2
newextent = extent(gebco2)
mask = extend(bathy2, newextent)
extent(mask) == extent(gebco2)
  # mask gebco2 using bathy2
gebco3 = mask(gebco2,bathy2, inverse = TRUE)
plot(gebco3)
  # add both rasters
plot(gebco3)
plot(bathy2, add = TRUE)

stack = stack(gebco3,bathy2)
plot(stack)
meanIgnoringZeroes <- function(x) {
  mean(x[x!=0],na.rm=T)
}
combined = overlay(stack, fun=meanIgnoringZeroes)
plot(combined)

# Load eez
combined2 = mask(combined,template)
plot(combined2)

# save new bathymetry shapefile
writeRaster(combined2,"bathymtery_deWetandGEBCO.tif")
