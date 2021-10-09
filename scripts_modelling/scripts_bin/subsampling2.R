# packages
library(spThin)
library(dplyr)
library(sf)
library(dismo)

# template raster has same extent and resolution as environmental variables
template = raster(list.files(pattern = "template.tif", recursive = TRUE))

# first make sure all observations are within a gridcell of the predictors
obs.data = crop(obs.data,stack[[1]])

# many papers argue on what the best way to reduce sampling bias is
# a recurring suggested method is to subsample your data by envt variables rather than distance
# this allows useful combinations of predictor variables to be retained in the data
# a proxy for envt variables are the ecoregions created during the national biodiversity assessment
# therefore a subsample will be taken per ecoregion for the static model
# the same process will be done individually also for each points grouped by season

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

  #thinned_data = thin(obs.data@data[which(obs.data@data$region == i),],
  #                    lat.col = "LATITUDE",
  #                    long.col = "LONGITUDE",
  #                    spec.col = "SPECIES_SCIENTIFIC",
  #                    thin.par = 5,reps = 10, # thinning data at 5km apart, 100 reps
  #                    locs.thinned.list.return = TRUE,
  #                    write.files = TRUE,
  #                    out.base = paste0(i), # name of file containing subset points for each region
  #                    out.dir = "Thinneddata", # save all files to folder names Thinneddata
  #                    max.files = 1,
  #                    write.log.file = FALSE)

#rm(i,thinned_data)
# list all thinned files
#files = list.files(pattern = "thin1.csv",recursive = TRUE)
#list = list()
# read them in a combine into on dataframe
#for(i in 1:length(files)){
#  list[[i]] = read.csv(files[i])
#}
#rm(i,files)
thinned = do.call(rbind,list)
#thinned$SPECIES_SCIENTIFIC = NULL
rm(list)
pts_sub = SpatialPoints(thinned) # new subset of points
rm(thinned)
# remove all files from folder
file.remove(list.files(pattern = "thin1.csv",recursive = TRUE))

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

# thin data per season
for(a in 1:length(pts_sub_seasons)){
  # temp is the data frame for 1 season
  temp = pts_sub_seasons[[a]]
  # then for each region in temp
  for(b in 1:length(unique(temp@data$region))){
    # carry out the thinning function
    thinned_data = thin(temp@data[which(temp@data$region == b),],
                        lat.col = "LATITUDE",
                        long.col = "LONGITUDE",
                        spec.col = "SPECIES_SCIENTIFIC",
                        thin.par = 5,reps = 100, # thinning data at 5km apart, 100 reps
                        locs.thinned.list.return = TRUE,
                        write.files = TRUE,
                        out.base = paste0("season",a,"region",b), # name of file containing subset points for each region
                        out.dir = "Thinneddata", # save all files to folder names Thinneddata
                        max.files = 1,
                        write.log.file = FALSE)}}

# re-combine thinned data per season
# for each season
for(a in 1:length(levels(obs.data$SEASON))){
  # list all the thinned files (each from a separate region) for that season
files = list.files(pattern = paste0("season",a),recursive = TRUE) 
list = list() # empty list
for(i in 1:length(files)){
  # read the files into a list 
  list[[i]] = read.csv(files[i])}
# thinned is now the combined datapoints for one season
thinned = do.call(rbind,list)
# remove the species name
thinned$SPECIES_SCIENTIFIC = NULL
# remove the list
rm(list)
# add thinned seasonal data to original list 
pts_sub_seasons[[a]] = SpatialPoints(thinned) # new subset of points
rm(thinned)}
rm(a,b,files,i,thinned_data,temp)
# remove all files from folder
file.remove(list.files(pattern = "thin1.csv",recursive = TRUE))

