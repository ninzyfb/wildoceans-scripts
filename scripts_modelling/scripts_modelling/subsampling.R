# ---------------------------------------------------------------------------------
# AUTHORS: Nina Faure Beaulieu, Dr. Victoria Goodall (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST
# CONTACT: ninab@wildtrust.co.za; victoria.goodall@mandela.ac.za 
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
# assign grid cell id to each data point
obs.data$cellid = cellFromXY(stack_subset,obs.data)
# get most recent date for each grid cell with data point
presencecells = obs.data@data %>%
  group_by(cellid)%>%
  summarise(date_recent = max(DATE))%>%
  filter(!is.na(cellid))
# create spatial points object from presence cell ids
pts_sub = SpatialPoints(xyFromCell(stack_subset,presencecells$cellid))
# ---------------------------------


# ---------------------------------
# DISTRIBUTION OF POINTS BY DATE (for interest only)
# ---------------------------------
# scale date from 0 to 1
presencecells$date_recent = as.Date(presencecells$date_recent)
scaled_date = scales::rescale(presencecells$date_recent)
# add to presence cells
presencecells= cbind(presencecells,scaled_date)
# add to filtered points
pts_sub$scaled_date = scaled_date
# plot to show scaled points by date (dark blue will be most recent)
cols = brewer.pal(4,"Blues")
pal = colorRampPalette(cols)
pts_sub$order = findInterval(pts_sub$scaled_date,sort(pts_sub$scaled_date))
plot(pts_sub@coords,pch=19,col=pal(nrow(pts_sub))[pts_sub$order])
# legend
legend("topright",col=pal(2),pch=19,legend = c(round(range(pts_sub$scaled_date),1)))
# remove order variable
pts_sub$order=NULL
rm(scaled_date,cols,presencecells,pal)
# ---------------------------------


# ---------------------------------
# SPATIAL THINNING
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
# ---------------------------------
