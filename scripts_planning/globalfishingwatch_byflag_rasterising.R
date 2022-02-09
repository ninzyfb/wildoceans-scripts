# packages
library(raster)
library(sf)


# load eez
setwd('/Users/nfb/Dropbox/6-WILDOCEANS/Planning')
eez = raster("template.tif")

# set directory
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Planning/fishing_effort/fishing_effort_byflag")

# list files
files = list.files()
i = 1
for(i in 1:length(files)){
  temp = read.csv(files[i])
  temp$X = NULL
  temp$X.1 = NULL
  temp = st_as_sf(temp, coords = c("lon","lat"))
  raster_temp = rasterize(temp,eez, field = "fishing_hours")
  raster_temp <- focal(raster_temp, w=matrix(1,nrow=3, ncol=3), fun=mean, NAonly=TRUE, na.rm=TRUE) # fill gaps using neighbouring cells
  name = files[i]
  name = strsplit(name,".csv")[[1]]
  writeRaster(raster_temp,paste0(name,".tif"))}

rasters = list.files(pattern = "_sa.tif")
a = 1
b = 30
for(i in 1:length(rasters)){
  stack = stack(rasters[a:b])
  avg = calc(stack,mean, na.rm = TRUE)
  writeRaster(avg,paste0(b,".tif"))
  a = b+1
  b = b+30}
# for the last rasters that don't make up 30 files
a = 3271
b = 3285
for(i in 1:length(rasters)){
  stack = stack(rasters[a:b])
  avg = calc(stack,mean, na.rm = TRUE)
  writeRaster(avg,paste0(b,".tif"))
  a = b+1
  b = b+30}

# now calculate average fishing pressure from all the files combined
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Planning/fishing_effort/fishing_effort_byvessel_sa_2012-2020/fishingeffort_rasterfiles_per30days")
rasters = list.files(pattern = ".tif")
all = stack(rasters) 
avg = calc(all,mean, na.rm = TRUE)
plot(avg)
writeRaster(avg,"pressuremap_2012-2020.tif")


pressuremap = raster("pressuremap_2012-2020.tif")
f <- focal(pressuremap, w=matrix(1,nrow=3, ncol=3), fun=mean, NAonly=TRUE, na.rm=TRUE) # fill gaps using neighbouring cells
plot(f)
f2 = mask(f,eez)
plot(f2)
writeRaster(f2,"pressuremap_2012-2020_v2.tif")

