# Load packages
library(tidyverse) # for general data wrangling and plotting
library(furrr) # for parallel operations on lists
library(lubridate) # for working with dates
library(sf) # for vector data 
library(raster) # for working with rasters
library(maps) # additional helpful mapping packages
library(maptools)
library(rgeos)
library(fasterize)
 
# World polygons from the maps package
#world_shp <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

# Create dataframe of filenames dates and filter to date range of interest
setwd('/Users/nfb/Dropbox/6-WILDOCEANS/Planning')
eez = raster("template.tif")

# Load each folder (different year each)
folders = list.files(pattern = "mmsi-daily")
list = list()
for(i in 1:length(folders)){
  list[[i]] = effort_files <- tibble(
  file = list.files(paste0(data_dir, folders[i]), 
                    pattern = '.csv', recursive = T, full.names = T),
  date = ymd(str_extract(file, 
                         pattern = '[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}')))
}
rm(i)

# for year 2020 only
data_dir <- '/Users/nfb/Dropbox/6-WILDOCEANS/Planning/fishing_effort/fishing_effort_byvessel/'
setwd('/Users/nfb/Dropbox/6-WILDOCEANS/Planning/fishing_effort/fishing_effort_byvessel/')
effort_files <- tibble(
  file = list.files(data_dir, 
                    pattern = '.csv', recursive = T, full.names = T),
  date = ymd(str_extract(file, 
                         pattern = '[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}')))

# Read in data (uncomment to read in parallel)
#plan(multiprocess) # Windows users should change this to plan(multisession)
# this takes an extremely long time so for now test with one file
#effort_df <- purrr::map(effort_files$file, .f = read_csv)

# clip each file to the south african EEZ
for(y in 1:length(list)){
  effort_files = list[[y]]
for(i in 1:nrow(effort_files)){
effort_df <- purrr::map(effort_files[i,1], .f = read_csv)
temp = effort_df[[1]]
temp2 = st_as_sf(temp, coords = c("cell_ll_lon", "cell_ll_lat"))
temp3 = st_crop(temp2, extent(eez))
date = unique(temp3$date)
if(length(date) != 0){write.csv(temp3,paste0(date,"_sa.csv"))}
}}

# turn each file into a georeferenced file
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Planning/fishing_effort_new")
files = list.files()
for(i in 1:length(files)){
  name = files[i]
  temp = read.csv(files[i])
  colnames(temp) = c("date","mmsi","hours","fishing_hours","lon","lat")
  temp$lon = as.numeric(str_remove_all(temp$lon,"c\\("))
  temp$lat = as.numeric(str_remove_all(temp$lat,"\\)"))
  write.csv(temp,name)
  }

# now turn each file into a raster
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Planning/fishing_effort/fishing_effort_byvessel_sa_2012-2020/fishingeffort_csvfiles_daily")
files = list.files()
for(i in 1:length(files)){
temp = read.csv(files[i])
temp$X = NULL
names = colnames(temp)[1:6]
colnames(temp)[2:7] = names[1:6]
temp[,1] = NULL
temp2 = st_as_sf(temp, coords = c("lon","lat"))
temp3 = rasterize(temp2,eez, field = "fishing_hours")
temp4 <- focal(temp3, w=matrix(1,nrow=3, ncol=3), fun=mean, NAonly=TRUE, na.rm=TRUE) # fill gaps using neighbouring cells
name = files[i]
name = strsplit(name,".csv")[[1]]
writeRaster(temp4,paste0(name,".tif"))}

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

