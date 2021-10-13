## Packages
library(raster) # package for raster manipulation
library(sf) # package for shapefile data

# Set directory
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling")
getwd()

template = raster("template.tif") # load template file

setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/environmental_variables/BioOracle")

files = list.files() # list of bio oracle files

# start raster stack
start = raster(files[1])
start = crop(start,template)
start = projectRaster(start, template, method = "bilinear")
stack = stack(start)
rm(start)

# add to raster stack
for(i in 1:length(files)){
  temp = raster(files[i])
  temp = crop(temp, template)
  temp = projectRaster(temp, template, method = "ngb")
  stack = addLayer(stack, temp)}

plot(stack)

# for now remove Par max mean and pH as doesnt seem to be working
for(i in 1:nlayers(stack)){
  name = names(stack[[i]])
  writeRaster(stack[[i]],paste0("clipped",name,".tif"))
}


names(stack[[i]])

