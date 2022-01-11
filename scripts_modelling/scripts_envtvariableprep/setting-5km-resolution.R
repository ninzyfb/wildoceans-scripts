# Set directory
path = "/home/nina/" #path for linux
path =  "/Users/nfb/" # path for mac
setwd(paste0(path,"Dropbox/6-WILDOCEANS")) # set directory

# Packages
library(raster)
library(sf)

# eez
eez = shapefile(list.files(pattern="eez.shp", recursive = TRUE, full.names = TRUE)) # load eez
crs(eez)
# remove unecessary data columns
eez <- eez[,-(1:31)]

# 5km resolution
r_5km <- raster(extent(eez), resolution = 0.05)
values(r_5km) = 1
# getcover = true means that the entire extent is rasterized
# and that each cell gets a value showing the proportion of the polygon in the raster cell
r_5km = rasterize(eez,r_5km,getCover = TRUE)
r_5km[which(values(r_5km)==0)] = NA
r_5km[which(values(r_5km)>0)] = 1
crs(r_5km) = crs(eez)

# 10km resolution
r_10km <- raster(extent(eez), resolution = 0.1)
values(r_10km) = 1
# getcover = true means that the entire extent is rasterized
# and that each cell gets a value showing the proportion of the polygon in the raster cell
r_10km = rasterize(eez,r_10km,getCover = TRUE)
r_10km[which(values(r_10km)==0)] = NA
r_10km[which(values(r_10km)>0)] = 1
crs(r_10km) = crs(eez)

# check
res(r_5km)
res(r_10km)

# write
writeRaster(r_5km,"template_5km.tif",overwrite=TRUE)
writeRaster(r_10km,"template_10km.tif",overwrite=TRUE)

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
  temp_modified = projectRaster(temp,r_5km, method = 'bilinear') # project the raster to the template
  temp_modified = mask(temp_modified,r_5km)
  name = strsplit(name,"Users/nfb/Dropbox/6-WILDOCEANS/Modelling/ALL_LAYERS/")[[1]][2]
  writeRaster(temp_modified,paste0("Modelling/ALLLAYERS5kmresolution",name), overwrite=TRUE) # write raster
}
# 10km res
for(name in continuous){
  temp = raster(name) # load raster
  temp_modified = projectRaster(temp,r_10km, method = 'bilinear') # project the raster to the template
  temp_modified = mask(temp_modified,r_10km)
  name = strsplit(name,"Users/nfb/Dropbox/6-WILDOCEANS/Modelling/ALL_LAYERS/")[[1]][2]
  writeRaster(temp_modified,paste0("Modelling/ALLLAYERS10kmresolution",name), overwrite=TRUE) # write raster
}

# project categorical variables using nearest neighbour method
categorical = files[length(files)] # remove substrate simplified
# 5km res
for(name in categorical){
  temp = raster(name) # load raster
  temp_modified = projectRaster(temp,r_5km, method = 'ngb') # project the raster to the template
  temp_modified = mask(temp_modified,r_5km)
  name = strsplit(name,"Users/nfb/Dropbox/6-WILDOCEANS/Modelling/ALL_LAYERS/")[[1]][2]
  writeRaster(temp_modified,paste0("Modelling/ALLLAYERS5kmresolution",name), overwrite=TRUE) # write raster
}
# 10km res
for(name in categorical){
  temp = raster(name) # load raster
  temp_modified = projectRaster(temp,r_10km, method = 'ngb') # project the raster to the template
  temp_modified = mask(temp_modified,r_10km)
  name = strsplit(name,"Users/nfb/Dropbox/6-WILDOCEANS/Modelling/ALL_LAYERS/")[[1]][2]
  writeRaster(temp_modified,paste0("Modelling/ALLLAYERS10kmresolution",name), overwrite=TRUE) # write raster
}

# stack them
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/ALL_LAYERS_5km_resolution/")
stack_5km = stack(list.files("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/ALL_LAYERS_5km_resolution/", recursive = TRUE, full.names = TRUE))
stack_10km = stack(list.files("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/ALL_LAYERS_10km_resolution/", recursive = TRUE, full.names = TRUE))

# extend NA values with values from nearest neighbours
list = list()
stack = stack()
list2 = list()
for(i in 1:nlayers(stack_5km)){
  temp = stack_5km[[i]] # isolate layer
  diff  = mask(template,temp, inverse = TRUE) # identify cells with no value in raster
  val = sum(values(diff), na.rm = TRUE) # if value is more than 0 then raster does not overlap perfectly with template
  list[[i]] = val
  f <- focal(temp, w=matrix(1,nrow=3, ncol=3), fun=modal, NAonly=TRUE, na.rm=TRUE) # fill gaps using neighbouring cells
  stack = addLayer(stack,f)
  diff  = mask(template,f, inverse = TRUE)
  val = sum(values(diff), na.rm = TRUE) # if value is more than 0 then raster does not overlap perfectly with template
  list2[[i]] = val # how many cells have NA value now
  name = names(stack_5km)[i]
  f = mask(f,template) # crop to template now
  writeRaster(f,paste(name,".tif",sep=""), overwrite=TRUE) # write raster
} 

setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/ALL_LAYERS_10km_resolution/")

#extend NA values with values from nearest neighbours
list = list()
stack = stack()
list2 = list()
for(i in 1:nlayers(stack_10km)){
  temp = stack_10km[[i]] # isolate layer
  diff  = mask(template_10,temp, inverse = TRUE) # identify cells with no value in raster
  val = sum(values(diff), na.rm = TRUE) # if value is more than 0 then raster does not overlap perfectly with template
  list[[i]] = val
  f <- focal(temp, w=matrix(1,nrow=3, ncol=3), fun=modal, NAonly=TRUE, na.rm=TRUE) # fill gaps using neighbouring cells
  stack = addLayer(stack,f)
  diff  = mask(template_10,f, inverse = TRUE)
  val = sum(values(diff), na.rm = TRUE) # if value is more than 0 then raster does not overlap perfectly with template
  list2[[i]] = val # how many cells have NA value now
  name = names(stack_10km)[i]
  f = mask(f,template_10) # crop to template now
  writeRaster(f,paste(name,".tif",sep=""), overwrite=TRUE) # write raster
} 


