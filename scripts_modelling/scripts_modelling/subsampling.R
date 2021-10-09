# ---------------------------------------------------------------------------------
######### Shark and ray species distribution models (SDMs) - sub-sampling script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: extracts a sub-sample of data points to only keep one occurrence point per grid cell

# ---------------------------------
# FORMATTING
# ---------------------------------

# Subsample one occurrence point per grid cell for static model
pts = SpatialPoints(obs.data)
pts_sub = SpatialPoints(gridSample(pts, template, n=1))
rm(pts)

# Subsample one occurrence point per grid cell for seasonal models
if(seasonal == 'yes'){
pts_sub_seasons = list()
# turn season to factor
obs.data$SEASON = as.factor(obs.data$SEASON) # turn season to factor
seasons = unique(obs.data$SEASON) # vector of seasons
seasons = seasons[!is.na(seasons)] # remove any NAs
for(i in 1:length(seasons)){ # for each season
  temp = obs.data[!is.na(obs.data$SEASON),] # remove any NAs from season variable
  temp = temp[temp$SEASON == levels(temp$SEASON)[i],] # filter to keep one season
  if(nrow(temp@data)>1){ # if there is more than one data point
  pts_sub_seasons[[i]] = SpatialPoints(gridSample(temp, template, n=1)) # sub-sample one data point per grid cell
  names(pts_sub_seasons)[i] = levels(obs.data$SEASON)[i]} # name the list with the season
}
rm(temp, seasons,i)} # remove unnecessary variables

rm(obs.data) # remove unnecessary variables
