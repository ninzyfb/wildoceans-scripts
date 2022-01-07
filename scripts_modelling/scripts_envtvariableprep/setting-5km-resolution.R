# Set directory
path = "/home/nina/" #path for linux
path =  "/Users/nfb/" # path for mac
setwd(paste0(path,"Dropbox/6-WILDOCEANS")) # set directory

# Packages
library(raster)
library(sf)

# Template raster
# raster from which the extent and resolution will be taken from for the 5km models
# This is one of the daily SST rasters
template = raster(list.files(pattern = "template.tif", recursive = TRUE))
template_10 = raster(list.files(pattern = "template_10km.tif", recursive = TRUE))

# now we transform each variable to match the extent, resolution and crs of the template
# this is important for modelling as all envt ratsters need to be the same
# the technique is use is ngb which means nearest neighbour.

# list each environmental layer that we want to use for modelling
files = list.files("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/ALL_LAYERS/",full.names = TRUE) 

# project continuous variables using bilinear method
continuous = files[-length(files)] # remove substrate simplified
# 5km res
for(name in continuous){
  temp = raster(name) # load raster
  temp_modified = projectRaster(temp,template, method = 'bilinear') # project the raster to the template
  temp_modified = mask(temp_modified,template)
  name = strsplit(name,"Users/nfb/Dropbox/6-WILDOCEANS/Modelling/ALL_LAYERS/")[[1]][2]
  writeRaster(temp_modified,paste0("Modelling/ALL_LAYERS_5km_resolution",name), overwrite=TRUE) # write raster
}
# 10km res
for(name in continuous){
  temp = raster(name) # load raster
  temp_modified = projectRaster(temp,template_10, method = 'bilinear') # project the raster to the template
  temp_modified = mask(temp_modified,template_10)
  name = strsplit(name,"Users/nfb/Dropbox/6-WILDOCEANS/Modelling/ALL_LAYERS/")[[1]][2]
  writeRaster(temp_modified,paste0("Modelling/ALL_LAYERS_10km_resolution",name), overwrite=TRUE) # write raster
}

# project categorical variables using nearest neighbour method
categorical = files[length(files)] # remove substrate simplified
# 5km res
for(name in categorical){
  temp = raster(name) # load raster
  temp_modified = projectRaster(temp,template, method = 'ngb') # project the raster to the template
  temp_modified = mask(temp_modified,template)
  name = strsplit(name,"Users/nfb/Dropbox/6-WILDOCEANS/Modelling/ALL_LAYERS/")[[1]][2]
  writeRaster(temp_modified,paste0("Modelling/ALL_LAYERS_5km_resolution",name), overwrite=TRUE) # write raster
}
# 10km res
for(name in categorical){
  temp = raster(name) # load raster
  temp_modified = projectRaster(temp,template_10, method = 'ngb') # project the raster to the template
  temp_modified = mask(temp_modified,template_10)
  name = strsplit(name,"Users/nfb/Dropbox/6-WILDOCEANS/Modelling/ALL_LAYERS/")[[1]][2]
  writeRaster(temp_modified,paste0("Modelling/ALL_LAYERS_10km_resolution",name), overwrite=TRUE) # write raster
}

# stack them
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/ALL_LAYERS_5km_resolution/")
test1 = stack(list.files("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/ALL_LAYERS_5km_resolution/"))

# extend NA values with values from nearest neighbours
list = list()
stack = stack()
list2 = list()
for(i in 1:nlayers(test1)){
  temp = test1[[i]] # isolate layer
  diff  = mask(template,temp, inverse = TRUE) # identify cells with no value in raster
  val = sum(values(diff), na.rm = TRUE) # if value is more than 0 then raster does not overlap perfectly with template
  list[[i]] = val
  f <- focal(temp, w=matrix(1,nrow=3, ncol=3), fun=modal, NAonly=TRUE, na.rm=TRUE) # fill gaps using neighbouring cells
  stack = addLayer(stack,f)
  diff  = mask(template,f, inverse = TRUE)
  val = sum(values(diff), na.rm = TRUE) # if value is more than 0 then raster does not overlap perfectly with template
  list2[[i]] = val # how many cells have NA value now
  name = names(test1)[i]
  f = mask(f,template) # crop to template now
  writeRaster(f,paste(name,".tif",sep=""), overwrite=TRUE) # write raster
} 

setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/ALL_LAYERS_10km_resolution/")
test2 = stack(list.files("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/ALL_LAYERS_10km_resolution/"))

#extend NA values with values from nearest neighbours
list = list()
stack = stack()
list2 = list()
for(i in 1:nlayers(test2)){
  temp = test2[[i]] # isolate layer
  diff  = mask(template_10,temp, inverse = TRUE) # identify cells with no value in raster
  val = sum(values(diff), na.rm = TRUE) # if value is more than 0 then raster does not overlap perfectly with template
  list[[i]] = val
  f <- focal(temp, w=matrix(1,nrow=3, ncol=3), fun=modal, NAonly=TRUE, na.rm=TRUE) # fill gaps using neighbouring cells
  stack = addLayer(stack,f)
  diff  = mask(template_10,f, inverse = TRUE)
  val = sum(values(diff), na.rm = TRUE) # if value is more than 0 then raster does not overlap perfectly with template
  list2[[i]] = val # how many cells have NA value now
  name = names(test2)[i]
  f = mask(f,template_10) # crop to template now
  writeRaster(f,paste(name,".tif",sep=""), overwrite=TRUE) # write raster
} 


