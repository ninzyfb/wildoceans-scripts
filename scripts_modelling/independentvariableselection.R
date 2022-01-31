# ---------------------------------------------------------------------------------
# AUTHOR: Nina Faure Beaulieu (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script identifies collinearity between predictor variables and only keeps a subset
# ---------------------------------

# ---------------------------------
# PACKAGES
# ---------------------------------
library(sdmpredictors)
# ---------------------------------


# ---------------------------------
# VARIABLE SELECTION
# ---------------------------------
# extract values from raster stack and drop all NA values (i.e. land)
stack_matrix = as.data.frame(values(stack)) %>% drop_na()
# identify pairwise correlations between variables
varselect_2 = correlation_groups(stack_matrix,max_correlation=0.7)
# extract names from list
varselect_2 = names(varselect_2[[1]])
# save as dataframe
write.csv(as.data.frame(varselect_2),paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs/selectedvariables/","selectedvariables_all.csv"))
# ---------------------------------

