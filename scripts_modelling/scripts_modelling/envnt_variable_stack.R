# ---------------------------------------------------------------------------------
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script loads all of the environmental variable rasters
# these rasters will be used as predictor variables in the models 
# ---------------------------------


# ---------------------------------
# ENVIRONMENTAL VARIABLES
# ---------------------------------
# load appropriate variable rasters based on chosen resolution
# resolution is defined in the parents script
if(res == 5){
# 5 x 5 km raster layers
variable_names = list.files(path = paste0(my.directory,"/ALLLAYERS5kmresolution"),pattern = ".tif", recursive = TRUE, full.names = TRUE)
# stack all rasters 
stack = stack(variable_names) 
# turn substrate layer to a factor
stack$substrate_simplified = as.factor(stack$substrate_simplified)
rm(variable_names)}

if(res ==10){
  # 10 x 10 km raster layers
variable_names = list.files(path = paste0(my.directory,"/ALLLAYERS10kmresolution"),pattern = ".tif", recursive = TRUE, full.names = TRUE)
# stack all rasters 
stack = stack(variable_names)
# turn substrate layer to a factor
stack$substrate_simplified = as.factor(stack$substrate_simplified)
rm(variable_names)}
# ---------------------------------

# ---------------------------------
# SUBSET ENVIRONMENTAL VARIABLES
# ---------------------------------
# read in selected group of variables
# these were subset by looking at pearson correlation coefficients
# only variables with a correlation coefficient <0.7 were retained
varselect = read.csv(list.files(pattern = "selectedvariables_all.csv", recursive = TRUE, full.names = TRUE))

# filter stack to only keep chosen variables
stack_subset = subset(stack,varselect$Variables)
stack_subset$substrate_simplified = as.factor(stack_subset$substrate_simplified)
rm(varselect,stack)
# ---------------------------------
