# ---------------------------------------------------------------------------------
######### Shark and ray species distribution models (SDMs) - envt_variable_stack script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: loads all of the environmental variable rasters

# ---------------------------------
# DATA
# ---------------------------------
if(res == 5){
# list files from 5km folder with final raster layers
variable_names = list.files(path = paste0(path,"Dropbox/6-WILDOCEANS/Modelling/ALLLAYERS5kmresolution"),pattern = ".tif", recursive = TRUE, full.names = TRUE)
stack = stack(variable_names) # stack all rasters 
stack$substrate_simplified = as.factor(stack$substrate_simplified) # turn substrate to a factor
rm(variable_names) # remove unnecessary variable
}
if(res ==10){
# list files from 10km folder with final raster layers
variable_names = list.files(path = paste0(path,"Dropbox/6-WILDOCEANS/Modelling/ALLLAYERS10kmresolution"),pattern = ".tif", recursive = TRUE, full.names = TRUE)
stack = stack(variable_names) # stack all rasters 
stack$substrate_simplified = as.factor(stack$substrate_simplified) # turn substrate to a factor
rm(variable_names) # remove unnecessary variable
}

# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------
setwd(paste0(path,"Dropbox/6-WILDOCEANS"))
