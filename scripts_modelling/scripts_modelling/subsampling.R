# ---------------------------------------------------------------------------------
# AUTHORS: Nina Faure Beaulieu, Dr. Victoria Goodall (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST
# CONTACTs: ninab@wildtrust.co.za; victoria.goodall@mandela.ac.za 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script extracts a sub-sample of data points (data thinning)
# The thinning results in only retaining one occurrence point per grid cell
# This is to correct for data bias (i.e. variance in effort across datasets)
# ---------------------------------


# ---------------------------------
# PACKAGES
# ---------------------------------
library(dismo)
# ---------------------------------


# ---------------------------------
# FORMATTING
# ---------------------------------
# Subsample one occurrence point per grid cell across entire dataset
crs(obs.data) = crs(stack_subset)
pts_sub = SpatialPoints(gridSample(obs.data, stack_subset, n=1),bbox =  bbox(stack_subset))
# set crs to match that of environmental variables
crs(pts_sub) = crs(stack_subset)

# Subsample one occurrence point per grid cell across summer and winter datasets separately
# this only runs if you have specified in the parent script that you are running seasonal models
if(seasonal == 'yes' & !is.na(seasonal)){

# turn season to factor
obs.data$SEASON = as.factor(obs.data$SEASON)
# create a vector with season names
seasons = unique(obs.data$SEASON) 
# remove any NAs from the vector (these come from datapoints with no associated season)
seasons = seasons[!is.na(seasons)] 
# ensure only data with associated season are kept
obs.data = obs.data[!is.na(obs.data$SEASON),] 
# empty list to store summer and winter thinned dataset
pts_sub_seasons = list()

# the following loop will run for each season
  for(i in 1:length(seasons)){ 
  # create temporary dataframe with all data from one season
  temp = obs.data[obs.data$SEASON == levels(obs.data$SEASON)[i],] 
  
  # continue with loop only if there is more than one data point in the seasonal dataset
  if(nrow(temp@data)>1){ 
  # sub-sample one occurrence point per grid cell and add this data set to the list
  pts_sub_seasons[[i]] = SpatialPoints(gridSample(temp, stack_subset, n=1),bbox = bbox(stack_subset))
  # name the dataset with the correct season
  names(pts_sub_seasons)[i] = levels(obs.data$SEASON)[i]
  # set the crs to match that of environmental variables
  crs(pts_sub_seasons[[i]]) = crs(stack_subset)}
  }
rm(temp, seasons,i,obs.data)
}else{rm(obs.data)}
