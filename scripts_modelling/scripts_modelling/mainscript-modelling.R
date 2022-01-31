# ---------------------------------------------------------------------------------
# AUTHOR: Nina Faure Beaulieu (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script is the parent modelling script, it calls all sub-scripts
# IMPORTANT: Run each subscript one at a time as running the whole parent script at once seems to cause some issues
# ---------------------------------


# ---------------------------------
# PACKAGES
# ---------------------------------
# list of required packages
requiredpackages = c("readxl","viridis","devtools","fuzzySim","dismo","rgdal","rgeos","sf","rasterVis","ggplot2","mecofun","raster","stringr","readxl", "raster", "sp", "dplyr", "lubridate")
# load packages
lapply(requiredpackages,require, character.only = TRUE)
rm(requiredpackages)
# ---------------------------------


# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------
# set directory to same parent folder where sub-scripts are found
# the subs-scripts can be in folders within this directory as the code will look through all the folders
path = "/home/nina/" # path for linux
path =  "/Users/nfb/" # path for mac
my.directory = paste0(path,"Dropbox/6-WILDOCEANS")
# set directory
setwd(my.directory) 
# ---------------------------------


# ---------------------------------
#  SPECIES SPECIFIC MODEL PARAMETERS 
# ---------------------------------
# read master file with species-specific modelling parameters
# this sheet contains a list of all species with some additional data details
# the prevalence value indicates how much data there is for this species
# prevalence is the percentage of cells with data out of total cells
master = read_xlsx(list.files(pattern = "data_summary_master.xlsx", recursive = TRUE,full.names = TRUE))
# ---------------------------------


# ---------------------------------
#  PLOTTING LAYERS
# ---------------------------------
source(list.files(pattern = "plottingparameters.R", recursive = TRUE, full.names= TRUE))
# ---------------------------------


# ---------------------------------
#  GRID
# ---------------------------------
# specify your chosen resolution of models
# here we chose a grid of either 5 x 5 km (res = 5) or 10 x 10 km (res = 10)  
res = 10

if(res == 5){
  # load appropriate grid
  template = raster(list.files(pattern = "template_5km.tif", recursive = TRUE, full.names = TRUE))
  # filter master sheet to keep species with enough prevalence for chosen resolution
  # in our case we required a minimum prevalence of 1
  master_keep = master %>%
    filter(rounded >=1)
  # number of background points to use during model development 
  # we went with 20% of total cells
  # code to figure out 20% of cells: 0.2*length(which(values(template)==1))
  n_bckg_pts = 8410}

if(res == 10){
  # load appropriate grid
  template = raster(list.files(pattern = "template_10km.tif", recursive = TRUE, full.names = TRUE))
  # filter master sheet to keep species with enough prevalence for chosen resolution
  # in our case we required a minimum prevalence of 1
  master_keep = master %>%
    filter(rounded_10>=1)
  # number of background points to use during model development 
  # we went with 20% of total cells
  # code to figure out 20% of cells: 0.2*length(which(values(template)==1))
  n_bckg_pts = 2162}
# ---------------------------------


# ---------------------------------
#  - ENVIRONMENTAL VARIABLES 
# ---------------------------------
source(list.files(pattern = "envnt_variable_stack.R", recursive = TRUE, full.names = TRUE))
# ---------------------------------


# ---------------------------------
#  RUNNING THE MODELS
# ---------------------------------
# the following loop runs the models for each species and creates plots
for(i in 1:nrow(master_keep)){
  
  # MODEL PARAMATERS
  target = master_keep$SPECIES_SCIENTIFIC[i] # species name
  folder = "speciesdata/" # folder with occurrence data files
  substrate = master_keep$Substrate[i] # specifies if substrate layer is to be included
  seasonal = master_keep$Seasonality[i] # specifies if seasonal (summer & winter) models are too also be run

  # OCCURRENCE DATA
  source(list.files(pattern = "species_data.R", recursive = TRUE, full.names = TRUE)) # finds script in directory
  rm(folder)
  
  # SAMPLING BIAS
  source(list.files(pattern = "subsampling.R", recursive = TRUE, full.names = TRUE))
  
  # BACKGROUND SAMPLE
  source(list.files(pattern ="pseudoabsence_1.R", recursive = TRUE, full.names = TRUE))
  
  # COLLINEARITY
  #source(list.files(pattern = "variableselection.R", recursive = TRUE, full.names = TRUE))
  
  # BIOMOD OBJECT CREATION
  source(list.files(pattern = "Biomod.R", recursive = TRUE, full.names = TRUE))
  
  # ASEASONAL MODEL RUNS AND PROJECTIONS
  model_type = "Aseasonal" # specify model_type
  data = biomod_obj # specify which biomod_obj
  source(list.files(pattern = "modelling.R", recursive = TRUE, full.names = TRUE)[3])

  # SEASONAL MODEL RUNS AND PROJECTIONS
  if(seasonal == "yes"){
    model_type = "summer"
    season = 1
    data = biomod_obj_seasons[[season]]
    source(list.files(pattern = "modelling.R", recursive = TRUE, full.names = TRUE)[3])

    model_type = "winter"
    season = 2
    data = biomod_obj_seasons[[season]]
    source(list.files(pattern = "modelling.R", recursive = TRUE, full.names = TRUE)[3])}
  rm(stack_subset,stack_new)}
# ---------------------------------

# END OF SCRIPT