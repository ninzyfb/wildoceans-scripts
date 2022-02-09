# packages
library(stringr)
library(dplyr)
library(raster)
library(sf)
library(xlsx)

# Create dataframe of filenames dates and filter to date range of interest
setwd('/Users/nfb/Dropbox/6-WILDOCEANS/Planning/fishing_effort/fishing_effort_byflag')

# Load each folder (there is one per year)
folders = list.files(pattern = "fleet-daily")
list = list() # empty list
for(i in 1:length(folders)){ # for each folder
  list[[i]] = effort_files <- tibble( # create a tibble
    file = list.files(folders[i], # 1st collumn: each row is the name of a csv
                      pattern = '.csv', recursive = T, full.names = T),
    date = ymd(str_extract(file, # 2nd collumn: each row is the date extracted from the title of the file
                           pattern = '[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}')))
}
rm(i)

# clip each file to the south african EEZ
eez = raster("/Users/nfb/Dropbox/6-WILDOCEANS/Planning/template.tif")
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

# reformat file column names and geometry
# this got modified when saving them
files = list.files(pattern = "_sa.csv",recursive = TRUE)

for(i in 1:length(files)){
  name = strsplit(files[i],"/")[[1]][2]
  temp = read.csv(files[i])
  colnames(temp) = c("date","flag","geartype","hours","fishing_hours","mmsi_present","lon","lat")
  temp$lon = as.numeric(str_remove_all(temp$lon,"c\\("))
  temp$lat = as.numeric(str_remove_all(temp$lat,"\\)"))
  write.csv(temp,name)
}

######## STEP 2

# load eez
setwd('/Users/nfb/Dropbox/6-WILDOCEANS/Planning')
eez = raster("template.tif")

setwd('/Users/nfb/Dropbox/6-WILDOCEANS/Planning/fishing_effort/fishing_effort_byflag')

# combine data by year
years = as.character(c(2012,2013,2014,2015,2016,2017,2018,2019,2020))

# read in every year by fishing gear seperately
for(year in years){ # for every year concatenate all days into one file
for (file in list.files(pattern = year,recursive = TRUE)){
  # Create the first data if no data exist yet
  if (!exists("fishingeffort_temp")){
    fishingeffort_temp <- read.csv(file, header=TRUE)
  }
  # if data already exist, then append it together
  if (exists("fishingeffort_temp")){
    temp <-read.csv(file, header=TRUE)
    fishingeffort_temp <-unique(rbind(fishingeffort_temp, temp))
    rm(temp)
  }
}
  # then filter by gear and save each as its own tif file
  gears = unique(fishingeffort_temp$geartype)
  for(gear in gears){
    temp = fishingeffort_temp %>%
      filter(geartype == gear) %>%
      filter(fishing_hours>0)
    temp = st_as_sf(temp, coords = c("lon","lat"))
    if(nrow(temp)!=0){
    # turn any NA values to 0
    raster_temp = rasterize(temp,eez, field = "fishing_hours", fun = mean, background = 0)
    #raster_temp <- focal(raster_temp, w=matrix(1,nrow=3, ncol=3), fun=mean, NAonly=TRUE, na.rm=TRUE) # fill gaps using neighbouring cells
    name = paste(gear,year,"fishingeffort")
    writeRaster(raster_temp,paste0(name,".tif"))}
  }
  rm(fishingeffort_temp, temp,raster_temp)}

# load eez
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Planning")
eez = raster("template.tif")
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Planning/fishing_effort/fishing_effort_byflag")

# list all file names
files = list.files()
# identify all unique gear types
list = list()
for(i in 1:length(files)){
list[[i]] = strsplit(files, " ")[[i]][1]}
filenames = unlist(list)
gears = unique(unlist(list)) 
# remove the filename from the folder with all the csvs
remove = gears == "fishing_effort_byflag_csv_sa"
gears = gears[!remove]
# combine all identical geartypes across all years into one
for(gear in gears){
all = files[grepl(paste0("^",gear,"$"),filenames)] # identify all files from that gear
list = list() # empty list
for(i in 1:length(all)){
list[[i]] = strsplit(all," ")[[i]][2]} # get all years with data from that gear
years = unlist(list) # put all years into a vector (this is for naming the rasters)
temp_stack = stack(all) # stack all files from that gear
result = calc(temp_stack,mean) # calculate the average
result = mask(result,eez)
writeRaster(result,paste0(gear,"_",years[1],"-",years[length(years)],"_sa.tif"))
}

# combine similar gear types again
getwd()
dest = "finalfishingpressuremaps/"
files = list.files("/Users/nfb/Dropbox/6-WILDOCEANS/Planning/fishing_effort/fishing_effort_byflag/fishingeffort_byflag_rasters_bygear_acrossallyears")
path = "/Users/nfb/Dropbox/6-WILDOCEANS/Planning/fishing_effort/fishing_effort_byflag/fishingeffort_byflag_rasters_bygear_acrossallyears/"
# purse seines
ps = str_detect(files,"purse_seines")
ps = files[ps]
ps = paste0(path,ps)
ps = stack(ps)
ps = calc(ps,sum)
plot(ps)
writeRaster(ps,paste0(dest,"purseseines_2012-2020_sa.tif"))

# longline
lg = str_detect(files,"longlines")
lg = files[lg]
lg = paste0(path,lg)
lg = stack(lg)
lg = calc(lg,sum)
plot(lg)
writeRaster(lg,paste0(dest,"longlines_2012-2020_sa.tif"))

# trawls/dredges
d = str_detect(files,"dredge")
t = str_detect(files,"trawl")
d = files[d]
t = files[t]
dt = c(d,t)
dt = paste0(path,dt)
dt = stack(dt)
dt = calc(dt,sum)
plot(dt)
writeRaster(dt,paste0(dest,"dredge_trawls_2012-2020_sa.tif"))

write.xlsx(as.data.frame(files),"geartypes.xlsx")

# plot final fishing pressure maps
getwd()
setwd( "/Users/nfb/Dropbox/6-WILDOCEANS/Planning/fishing_effort/fishing_effort_byflag/finalfishingpressuremaps")
list.files()
plot(stack(list.files()))
plot(stack(list.files()[c(1,4,7,8)]))
