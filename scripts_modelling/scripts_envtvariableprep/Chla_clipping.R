## data was downloaded from:
# https://polarwatch.noaa.gov/catalog/chl-aqua/download/?dataset=daily&var=chlorophyll&time_min=2020-01-01T12:00:00Z&time_stride=1&time_max=2020-12-31T12:00:00Z&y_min=-38.1752&y_stride=1&y_max=-26.8621&x_min=13.348&x_stride=1&x_max=36.5307&fmt=nc

## Packages
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(sf) # package for shapefile data
library(tidyverse) # for naming files

## read in eez shapefile
eez = st_read("EEZ/eez.shp",crs = 4326)

## read in .nc file
nc_data = nc_open("erdMH1chlamday_f5bd_9ad3_85de.nc")

## get variables
t <- ncvar_get(nc_data, "time")
lon <- ncvar_get(nc_data, "longitude")
lat <- ncvar_get(nc_data, "latitude", verbose = F)
chl <- ncvar_get(nc_data,"chlorophyll")

# extract fill values
fillvalue <- ncatt_get(nc_data, "chlorophyll", "_FillValue")
# change fill values to NA
chl[chl == fillvalue$value] <- NA

## Loop 1 to extract monthly averages
# the file is from MODIS aqua mapped 4km resolution

# loop
for(i in 1:length(t)){
  # extract chlorophyll from one month
  chl.slice <- chl[, ,i] 
  # convert to a raster
  r <- raster(t(chl.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  # crop raster using eez
  cropped <- crop(r, eez)
  # extract date
  date = as.POSIXct(t[i], origin = "1970-01-01 00:00:00")
  date = as.Date(date,format = "%Y%m%d")
  date = str_remove_all(date,"[-]")
  # write raster to folder
  writeRaster(cropped, paste("CHL_SA_EEZ_rasters/",date,".tif",sep =""))
}
