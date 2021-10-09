# directory
getwd()
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/")

## Packages
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(sf) # package for shapefile data
library(tidyverse) # for naming files

## read in eez shapefile
eez = st_read(list.files(pattern = "eez.shp", recursive = TRUE),crs = 4326)
extent = extent(eez)

## Loop 1 to do this for every netcdf file 
# for when every file is a day
# these files are on Matthew Carr's hardrive

# read in names of net cdf files
PATH = "environmental_variables/SST_OSTIA_1993-2019_5km/"
temp = list.files(path = PATH, pattern="*.nc", recursive = TRUE)

# new loop 
for(i in 1:length(temp)){
  file = temp[i]
  rasterbrick = brick(paste(PATH,file, sep =""), varname="analysed_sst")
  for(layer in 1:length(names(rasterbrick))){
    r = subset(rasterbrick, layer)
    r = crop(r, eez)
    year = strsplit(names(r), split = "[.]")[[1]][1]
    year = strsplit(year, split = "X")[[1]][2]
    writeRaster(r, paste("sst_",year,"_",layer,".tif",sep=""))
  }
  }


# loop
for(i in temp){
  file = temp[i]
  # read in single .nc file for one day
  nc_data <- nc_open(paste(PATH,file, sep =""))
  # extract dimension variables (lat, lon, SST)
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat", verbose = F)
  sst <- ncvar_get(nc_data,"analysed_sst")
  # extract fill values
  fillvalue <- ncatt_get(nc_data, "analysed_sst", "_FillValue")
  # change fill values to NA
  sst[sst == fillvalue$value] <- NA
  # CONVERT SST TO MATRIX
  sst = as.matrix(sst)
  # convert to a raster
  r <- raster(t(sst), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  # flip y coordinates
  r <- flip(r, direction='y')
  # crop raster using eez
  cropped <- crop(r, eez)
  # write raster to folder
  writeRaster(cropped, paste(substr(i,1,8),".tif",sep =""))
}

plot(r)
## Loop 2 to do this for every netcdf file
# for when every file is a year

# read in data
# there are two different files (2019 and 2020)
# read in other file after running the loop on this file
nc_data <- nc_open('METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2_1620051661864.nc')

## extract dimension variables variables
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
time <- ncvar_get(nc_data,"time")

# extract SST data only
sst = ncvar_get(nc_data,"analysed_sst")

# change fill values to NA
fillvalue <- ncatt_get(nc_data, "analysed_sst", "_FillValue")
sst[sst == fillvalue$value] <- NA
rm(fillvalue)

# loop
for(i in 1:length(time)){
  # get a time slice (in this case one day)
  ndvi.slice <- sst[, , 1] 
  # convert to a raster
  r <- raster(t(ndvi.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  # flip y coordinates
  r <- flip(r, direction='y')
  # crop raster using eez
  cropped <- crop(r, eez)
  # extract date
  date = as.POSIXct(time[i], origin = "1981-01-01 00:00:00")
  date = as.Date(date,format = "%Y%m%d")
  date = str_remove_all(date,"[-]")
  # write raster to folder
  writeRaster(cropped, paste("SST_OSTIA_SA_EEZ_rasters/",date,".tif",sep =""),overwrite=TRUE)
}