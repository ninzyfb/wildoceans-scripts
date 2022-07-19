# ---------------------------------------------------------------------------------
# AUTHORS: Nina Faure Beaulieu, Dr. Victoria Goodall (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST
# CONTACT: ninab@wildtrust.co.za; victoria.goodall@mandela.ac.za 
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
requiredpackages = c("RColorBrewer","scales","devtools","readxl","viridis","devtools","fuzzySim","dismo","rgdal","rgeos","sf","rasterVis","ggplot2","raster","stringr","readxl", "raster", "sp", "dplyr", "lubridate")
# check which packages you need to install
requiredpackages = requiredpackages[which(!(requiredpackages %in% installed.packages()))]
# install packages
install.packages(requiredpackages)
# list of required packages
requiredpackages = c("RColorBrewer","scales","devtools","readxl","viridis","devtools","fuzzySim","dismo","rgdal","rgeos","sf","rasterVis","ggplot2","raster","stringr","readxl", "raster", "sp", "dplyr", "lubridate")
# load packages
lapply(requiredpackages,require, character.only = TRUE)
# install latest version of biomod2
options(timeout=10000) # increase max timeout so that it lets you install the package
install_github("biomodhub/biomod2", dependencies = TRUE)
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
# output is a RasterStack object of 18 layers named stack_subset
# all models are run on the same subset of environmental variables
# these are detailed in selectedvariables_all.csv
# these were chosen after testing for collinearity across 27 variables using the independentvariableselection.R script
source(list.files(pattern = "envnt_variable_stack.R", recursive = TRUE, full.names = TRUE))
# ---------------------------------


# ---------------------------------
#  SPECIES SPECIFIC MODEL PARAMETERS 
# ---------------------------------
# read master file with species-specific modelling parameters
# this sheet contains a list of all species with some additional data details
master = read_xlsx(list.files(pattern = "data_summary_master.xlsx", recursive = TRUE,full.names = TRUE))
# you can filter master sheet to keep species you want to model
# prevalence value indicates % of cells with data out of all cells in the EEZ
master_keep = master
# ---------------------------------


# ---------------------------------
#  RUNNING THE MODELS
# ---------------------------------
# the following loop runs the models for each species and creates plots
# this loop is based around the master_keep sheet which is a data frame of species names

# IMPORTANT: to run the loop with the example data make sure exampledata = "yes"
exampledata = "yes"
if(exampledata == "yes"){master_keep = master_keep %>% filter(SPECIES_SCIENTIFIC == "ACROTERIOBATUS ANNULATUS")}
# remember to set i = 1 when you are running example code
i = 1

for(i in 1:nrow(master_keep)){
  
  # MODEL PARAMATERS
  target = master_keep$SPECIES_SCIENTIFIC[i] # species name
  substrate = "yes" # specifies if substrate layer is to be included
  seasonal = "no" # specifies if you should run a seasonal model as well

  # OCCURRENCE DATA
  # output is a SpatialPointsDataFrame object named obs.data
  source(list.files(pattern = "species_data.R", recursive = TRUE, full.names = TRUE)) 
  
  # SPATIAL THINNING
  # output is a SpatialPoints object named pts.sub
  # this object contains all the data which has been filtered to only retain one presence per gridcell
  # this is to deal with sampling bias
  source(list.files(pattern = "subsampling.R", recursive = TRUE, full.names = TRUE))
  
  # BACKGROUND SAMPLE
  # output is a DataFrame object named pts_env
  # this is object contains all data as well as background points with associated envnt variables at each cell
  source(list.files(pattern ="pseudoabsence.R", recursive = TRUE, full.names = TRUE))
  
  # BIOMOD OBJECT CREATION
  # output is a Biomod object named biomod_obj
  # this packages all the data in a format that the biomod2 functions can understand
  source(list.files(pattern = "Biomod.R", recursive = TRUE, full.names = TRUE))
  
  # ASEASONAL MODEL RUNS AND PROJECTIONS
  model_type = "Aseasonal" # specify model_type
  data = biomod_obj # specify which biomod_obj
  # there are many outputs to this script
  # these will all be saved in an output folder
  source(list.files(pattern = "model_runs.R", recursive = TRUE, full.names = TRUE))

  # SEASONAL MODEL RUNS AND PROJECTIONS
  if(seasonal == "yes"){
    model_type = "summer"
    season = 1
    data = biomod_obj_seasons[[season]]
    source(list.files(pattern = "model_runs.R", recursive = TRUE, full.names = TRUE))

    model_type = "winter"
    season = 2
    data = biomod_obj_seasons[[season]]
    source(list.files(pattern = "model_runs.R", recursive = TRUE, full.names = TRUE))}
  rm(stack_new)}
# ---------------------------------


# ---------------------------------
#  PLOTTING
# ---------------------------------
source(list.files(pattern = "sdm_plots.R", recursive = TRUE, full.names = TRUE))
# ---------------------------------