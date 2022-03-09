# ---------------------------------------------------------------------------------
# AUTHOR: Nina Faure Beaulieu (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST
# CONTACT: ninab@wildtrust.co.za 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script is the parent modelling script, it calls a series of scripts which:
# load in clean occurrence data and predictor variable raster stack
# run modelling algorithms and produce an ensemble model and plots for each species
# a small description of each script is present before calling them
# and further descriptions can otherwise be found within each script

# IMPORTANT: Even when you know each script works, i suggest running each script one at a time as running the whole parent script at once seems to cause some issues

# IMPORTANT: increasing number of parameters
# ---------------------------------


# ---------------------------------
# DATA AVAILABILITY 
# ---------------------------------
# The raw occurrence data used to run this script is not available due to data sharing agreements in place
# For an example of what format the data should enter as see: example_data.csv on github page
# The predictor variable stack is available as well as the code used to deal with collinearity and decide on final number of variables used
# See independentvariableselection.R on github for the script that analysed for collinearity in predictor variables
# ---------------------------------


# ---------------------------------
# PACKAGES
# ---------------------------------
# list of required packages
requiredpackages = c("devtools","readxl","viridis","devtools","fuzzySim","dismo","rgdal","rgeos","sf","rasterVis","ggplot2","raster","stringr","readxl", "raster", "sp", "dplyr", "lubridate")
# check which packages you need to install
requiredpackages = requiredpackages[which(!(requiredpackages %in% installed.packages()))]
# install packages
install.packages(requiredpackages)
# list of required packages
requiredpackages = c("devtools","readxl","viridis","devtools","fuzzySim","dismo","rgdal","rgeos","sf","rasterVis","ggplot2","raster","stringr","readxl", "raster", "sp", "dplyr", "lubridate")
# load packages
lapply(requiredpackages,require, character.only = TRUE)
devtools::install_github("biomodhub/biomod2", dependencies = TRUE)
rm(requiredpackages)
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
#  - ENVIRONMENTAL VARIABLES 
# ---------------------------------
# specify model resolution
# we chose between a grid of 5 x 5 km (res = 5) or 10 x 10 km (res = 10)  
res = 10
source(list.files(pattern = "envnt_variable_stack.R", recursive = TRUE, full.names = TRUE))
# ---------------------------------


# ---------------------------------
#  SPECIES SPECIFIC MODEL PARAMETERS 
# ---------------------------------
# read master file with species-specific modelling parameters
# this sheet contains a list of all species with some additional data details
master = read_xlsx(list.files(pattern = "data_summary_master.xlsx", recursive = TRUE,full.names = TRUE))
# filter master sheet to keep species with a minimum prevalence of 1
# the prevalence value indicates how much data there is for this species relative to entire modeling surface
# i.e. it is the percentage of cells with data out of total cells, so the lower your resolution, the higher the prevalence
if(res == 5){
  master_keep = master %>%
    filter(rounded >=1)}
if(res ==10){
  master_keep = master %>%
    filter(rounded_10 >=1)}
# ---------------------------------


# ---------------------------------
#  RUNNING THE MODELS
# ---------------------------------
# the following loop runs the models for each species and creates plots
# this loop is based around the master_keep sheet which is a data frame of species names

# IMPORTANT: to simply run the loop with the example data
# go to the species_data.R subscript and follow the instructions in the subscript
# then come back and run each line in the loop one by one
# make sure to enter i = 1 before doing so
for(i in 1:nrow(master_keep)){
  
  # MODEL PARAMATERS
  target = master_keep$SPECIES_SCIENTIFIC[i] # species name
  substrate = master_keep$Substrate[i] # specifies if substrate layer is to be included
  seasonal = master_keep$Seasonality[i] # specifies if seasonal (summer & winter) models are too also be run

  # OCCURRENCE DATA
  source(list.files(pattern = "species_data.R", recursive = TRUE, full.names = TRUE)) 
  
  # SAMPLING BIAS
  source(list.files(pattern = "subsampling.R", recursive = TRUE, full.names = TRUE))
  
  # BACKGROUND SAMPLE
  source(list.files(pattern ="pseudoabsence.R", recursive = TRUE, full.names = TRUE))
  
  # BIOMOD OBJECT CREATION
  source(list.files(pattern = "Biomod.R", recursive = TRUE, full.names = TRUE))
  
  # ASEASONAL MODEL RUNS AND PROJECTIONS
  model_type = "Aseasonal" # specify model_type
  data = biomod_obj # specify which biomod_obj
  source(list.files(pattern = "modelling.R", recursive = TRUE, full.names = TRUE))

  # SEASONAL MODEL RUNS AND PROJECTIONS
  if(seasonal == "yes"){
    model_type = "summer"
    season = 1
    data = biomod_obj_seasons[[season]]
    source(list.files(pattern = "modelling.R", recursive = TRUE, full.names = TRUE))

    model_type = "winter"
    season = 2
    data = biomod_obj_seasons[[season]]
    source(list.files(pattern = "modelling.R", recursive = TRUE, full.names = TRUE))}
  rm(stack_new)}
# ---------------------------------

# ---------------------------------
#  PLOTTING
# ---------------------------------
source(list.files(pattern = "datavisualisation.R", recursive = TRUE, full.names = TRUE))
# ---------------------------------

# END OF SCRIPT