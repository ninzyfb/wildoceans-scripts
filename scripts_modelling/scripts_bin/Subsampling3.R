# packages
library(dplyr)
library(sf)
library(dismo)

# template raster has same extent and resolution as environmental variables
template = raster(list.files(pattern = "template.tif", recursive = TRUE))

# first make sure all observations are within a gridcell of the predictors
obs.data = crop(obs.data,stack[[1]])

# STATIC

# read in ecoregions
regions = raster(list.files(pattern = "ecoregion_simplified.tif", recursive = TRUE))
# turn to factor
regions = as.factor(regions)
# convert to same extent and resolution as your predictor variables
# the nearest neighbour method is used as these are factors so you dont want any cells to take an average value
regions = projectRaster(regions,stack, method = "ngb")
# extract region to each occurrence point
region = extract(regions,obs.data)
# add to obs.data
obs.data@data = cbind(obs.data@data,region)
rm(region,regions)

# this is the number of observations with no regions associated to them
# this is due to differences in the coast and how the pixels cover the coast
nonregions = nrow(obs.data@data[which(is.na(obs.data@data$region)),])
rm(nonregions)

# now thin the data per region making sure no points at less than 5km appart
list = list()
for(i in 1:length(unique(obs.data@data$region))){
  temp = obs.data@data[which(obs.data@data$region == i),]
  if(nrow(temp)>0){
    coordinates(temp) =  ~cbind(temp$LONGITUDE,temp$LATITUDE)
    thinned = SpatialPoints(gridSample(temp, template, n=1))
    list[[i]] = thinned
  }}
rm(temp,thinned)

pts_sub = do.call(rbind,list) # new subset of points
rm(list,thinned_data,i)

# SEASONAL

pts_sub_seasons = list()
seasons = unique(obs.data$SEASON)
seasons = seasons[!is.na(seasons)]
levels(obs.data$SEASON) # 1 is autumn, 2 is spring, 3 is summer, 4 is winter
# create seperate dataframe for each season
for(season in 1:length(seasons)){
  temp = obs.data[!is.na(obs.data$SEASON),]
  s = levels(temp$SEASON)[season]
  temp = temp[temp$SEASON == s,] # isolate a season
  pts_sub_seasons[[season]] = temp} # add to list
rm(s,season,seasons,temp)

list = list()
count = 1
# thin data per season
for(a in 1:length(pts_sub_seasons)){
  # temp is the data frame for 1 season
  temp = pts_sub_seasons[[a]]
  # then for each region in temp
  for(b in 1:length(unique(temp@data$region))){
    # carry out the thinning function
    temp2 = temp@data[which(temp@data$region == b),]
    if(nrow(temp2)>0){
      coordinates(temp2) =  ~cbind(temp2$LONGITUDE,temp2$LATITUDE)
      thinned = SpatialPoints(gridSample(temp2, template, n=1))
      list[[count]] = thinned
      names(list)[count] = a
    }
    count = count+1}}

names(list)
rm(a,b,temp,temp2,thinned,count)

pts_sub_seasons[[1]] = do.call(rbind,list[names(list)==1]) # new subset of points per season
pts_sub_seasons[[2]] = do.call(rbind,list[names(list)==2]) # new subset of points per season
pts_sub_seasons[[3]] = do.call(rbind,list[names(list)==3]) # new subset of points per season
pts_sub_seasons[[4]] = do.call(rbind,list[names(list)==4]) # new subset of points per season


rm(list,thinned_data,i)


