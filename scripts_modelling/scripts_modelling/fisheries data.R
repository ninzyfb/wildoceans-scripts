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

# add raster data for target species if available
files = list.files(path = paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Singlespecies_Rasters"),pattern = target, ignore.case = TRUE,full.names = TRUE)
if(length(files)>0){fishingdata = raster(files)
rm(files)

# the following extracts coordinates for presence cells
cells = which(values(fishingdata == 1)) # identify cells with a value of 1
coords = xyFromCell(fishingdata,cells) # get xy (coordinates) from those cells
coords = as.data.frame(coords) # convert to a dataframe
rm(cells,fishingdata) # remove unnecessary variables

# ---------------------------------
# FORMATTING
# ---------------------------------
names(coords) = c("LONGITUDE","LATITUDE") # rename columns
coords$SPECIES_SCIENTIFIC = target # add target species
coords$LONGITUDE = as.numeric(coords$LONGITUDE) # convert lat and lon to numeric
coords$LATITUDE = as.numeric(coords$LATITUDE) # convert lat and lon to numeric
coords$DATASET = "DEFF" # assign correct owner of dataset

# add to main observation data if it exists
if(exists('coords')){
  obs.data = full_join(obs.data,coords)
}
}
