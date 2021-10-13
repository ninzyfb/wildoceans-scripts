# ---------------------------------------------------------------------------------
######### Shark and ray species distribution models (SDMs) - fisheries data script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: loads in fisheries data and formats it properly

# ---------------------------------
# DATA
# ---------------------------------

# add raster data if available
files = list.files(path = paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Singlespecies_Rasters"),pattern = target, ignore.case = TRUE,full.names = TRUE)
if(length(files)>0){
list_coords = list() # empty list
for(a in 1:length(files)){ # add any raster files to list
  list_coords[[a]] = raster(files[a])
}
fishingdata = stack(list_coords) # stack the rasters
rm(list_coords,files,a)

# add value of 1 to any presence cell
for(b in 1:nlayers(fishingdata)){
  values(fishingdata[[b]])[values(!is.na(fishingdata[[b]]))] = 1}
rm(b) # remove unnecessary variables

# the following loop extracts coordinates for presence for each season
list_coords = list()
for(c in 1:nlayers(fishingdata)){
  cells = which(values(fishingdata[[c]]) == 1) # identify cells with a value of 1
  list_coords[[c]] = xyFromCell(fishingdata[[c]],cells) # get xy (coordinates) from those cells
  season = str_split(names(fishingdata)[i], pattern = "_")[[1]][4] # extract which seasons each fishing data layer came from
  length  = nrow(list_coords[[c]]) # get total number of coordinates/cells for each season
  season = rep(season,length) # assign the season to those coordinates
  list_coords[[c]] = as.data.frame(cbind(list_coords[[c]],season))} # convert to a dataframe
rm(cells,c,length,season) # remove unnecessary variables

# ---------------------------------
# FORMATTING
# ---------------------------------

# this renames collumns and adds dataset
fishingdata  = bind_rows(list_coords) # combine the dataframes from the list and rename 
rm(list_coords) # remove unnecessary variables
names(fishingdata) = c("LONGITUDE","LATITUDE","SEASON") # rename columns
fishingdata$SPECIES_SCIENTIFIC = target # add target species
fishingdata$LONGITUDE = as.numeric(fishingdata$LONGITUDE) # convert lat and lon to numeric
fishingdata$LATITUDE = as.numeric(fishingdata$LATITUDE) # convert lat and lon to numeric
fishingdata$SEASON = as.factor(fishingdata$SEASON) # convert season to a factor
fishingdata$DATASET = "DEFF" # assign correct owner of dataset

# add to main observation data
if(exists('fishingdata')){
  obs.data = full_join(obs.data,fishingdata)
}
rm(fishingdata) # remove unnecessary variables
}