# ---------------------------------------------------------------------------------
# AUTHORS: Nina Faure Beaulieu, Dr. Victoria Goodall (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST
# CONTACT: ninab@wildtrust.co.za; victoria.goodall@mandela.ac.za 
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
# 10 x 10 km raster layers
temp_dir = list.dirs()[which(str_detect(list.dirs(),"ALLLAYERS10kmresolution"))]
variable_names = list.files(path = temp_dir,pattern = ".tif", recursive = TRUE, full.names = TRUE)
# stack all rasters 
stack = stack(variable_names)
# turn substrate layer to a factor
stack$substrate_simplified = as.factor(stack$substrate_simplified)
rm(variable_names)
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
