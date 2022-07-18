# ---------------------------------------------------------------------------------
# AUTHOR: Nina Faure Beaulieu (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script identifies colinearity between predictor variables and only keeps a subset
# ---------------------------------

# ---------------------------------
# PACKAGES
# ---------------------------------
# list of required packages
requiredpackages = c("sdmpredictors","raster","corrplot","dplyr","tidyr")
# check which packages you need to install
requiredpackages = requiredpackages[which(!(requiredpackages %in% installed.packages()))]
# install packages
install.packages(requiredpackages)
# list of required packages
requiredpackages = c("sdmpredictors","raster","corrplot","dplyr","tidyr")
# load packages
lapply(requiredpackages,require, character.only = TRUE)
# ---------------------------------


# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------
# set directory to same parent folder where sub-scripts are found
# the subs-scripts can be in folders within this directory as the code will look through all the folders
my.directory = getwd()
# set directory
setwd(my.directory) 
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
# extract values from raster stack and drop all NA values (i.e. land)
stack_matrix = as.data.frame(values(stack)) %>% drop_na()
# ---------------------------------


# ---------------------------------
# CORRELATION PLOT
# ---------------------------------
cors = abs(cor(stack_matrix, use = "complete.obs"))
corrplot.mixed(cors,tl.pos='lt', tl.cex=0.6, number.cex=0.5, addCoefasPercent=T)
# save to output folder
png(file=paste0(my.directory,"/wildoceans-scripts/Outputs/modelling/correlation_matrix.png"),width=3000, height=2000, res=300)
corrplot.mixed(cors,tl.pos='lt', tl.cex=0.6, number.cex=0.5, addCoefasPercent=T)
dev.off()
# ---------------------------------


# ---------------------------------
# VARIABLE SELECTION
# ---------------------------------
# identify pairwise correlations between variables
varselect_2 = correlation_groups(stack_matrix,max_correlation=0.7)
# extract names from list
varselect_2 = names(varselect_2[[1]])
# save as dataframe
write.csv(as.data.frame(varselect_2),paste0(my.directory,"/wildoceans-scripts/selectedvariables_all.csv"))
# ---------------------------------