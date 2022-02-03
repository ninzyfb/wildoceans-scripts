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
library(sdmpredictors)
library(raster)
library(corrplot)
library(dplyr)
library(tidyr)
# ---------------------------------


# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------
# set directory to same parent folder where sub-scripts are found
# the subs-scripts can be in folders within this directory as the code will look through all the folders
path = "/home/nina/" # path for linux
path =  "/Users/nfb/" # path for mac
# define name of directory
my.directory = paste0(path,"Dropbox/6-WILDOCEANS")
# set directory
setwd(my.directory) 
# ---------------------------------


# ---------------------------------
# ENVIRONMENTAL VARIABLES
# ---------------------------------
# load appropriate variable rasters based on chosen resolution
res = 10
if(res == 5){
  # 5 x 5 km raster layers
  variable_names = list.files(path = paste0(path,"Dropbox/6-WILDOCEANS/Modelling/ALLLAYERS5kmresolution"),pattern = ".tif", recursive = TRUE, full.names = TRUE)
  # stack all rasters 
  stack = stack(variable_names) 
  # turn substrate layer to a factor
  stack$substrate_simplified = as.factor(stack$substrate_simplified)
  rm(variable_names)}

if(res ==10){
  # 10 x 10 km raster layers
  variable_names = list.files(path = paste0(path,"Dropbox/6-WILDOCEANS/Modelling/ALLLAYERS10kmresolution"),pattern = ".tif", recursive = TRUE, full.names = TRUE)
  # stack all rasters 
  stack = stack(variable_names)
  # turn substrate layer to a factor
  stack$substrate_simplified = as.factor(stack$substrate_simplified)
  rm(variable_names)}

# extract values from raster stack and drop all NA values (i.e. land)
stack_matrix = as.data.frame(values(stack)) %>% drop_na()
# ---------------------------------


# ---------------------------------
# CORRELATION PLOT
# ---------------------------------
cors = abs(cor(stack_matrix, use = "complete.obs"))
corrplot.mixed(cors,tl.pos='lt', tl.cex=0.6, number.cex=0.5, addCoefasPercent=T)
# ---------------------------------


# ---------------------------------
# VARIABLE SELECTION
# ---------------------------------
# identify pairwise correlations between variables
varselect_2 = correlation_groups(stack_matrix,max_correlation=0.7)
# extract names from list
varselect_2 = names(varselect_2[[1]])
# save as dataframe
write.csv(as.data.frame(varselect_2),paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs/selectedvariables/","selectedvariables_all.csv"))
# ---------------------------------



