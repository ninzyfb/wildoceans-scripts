# ---------------------------------------------------------------------------------
######### Shark and ray species distribution models (SDMs) - envt_variable_stack script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: loads all of the environmental variable rasters

# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------
setwd(paste0(path,"Dropbox/6-WILDOCEANS/Modelling/ALL_LAYERS_5km_resolution"))

# ---------------------------------
# DATA
# ---------------------------------
variable_names = list.files(pattern = ".tif", recursive = TRUE) # list files with variable names
stack = stack(variable_names) # stack all rasters 
stack$substrate_simplified = as.factor(stack$substrate_simplified) # turn substrate to a factor
rm(variable_names) # remove unnecessary variable

# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------
setwd(paste0(path,"Dropbox/6-WILDOCEANS/Modelling"))
