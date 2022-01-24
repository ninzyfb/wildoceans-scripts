# ---------------------------------------------------------------------------------
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT:  Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST 
# ---------------------------------------------------------------------------------

####
# THIS SCRIPT: This is the parent modelling script
# it calls all sub-scripts
# !! Run each subscript one at a time.
# Running the whole parent script at once seems to cause some issues
####

# ---------------------------------
# PACKAGES
# ---------------------------------
library(sp)
library(dplyr)
library(raster)
library(stringr)
library(lubridate)
library(ggplot2)
library(sf)
library(rgeos)
library(rgdal)
library(dismo)
library(fuzzySim) 
library(devtools)
library(mecofun)
library(rasterVis)
library(viridis)
library(readxl)

# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------
# set directory to same parent folder where sub-scripts are found
# the subs-scripts can be in folders within this directory as the code will look through all the folders
path = "/home/nina/" #path for linux
path =  "/Users/nfb/" # path for mac
setwd(paste0(path,"Dropbox/6-WILDOCEANS")) # set directory

# ---------------------------------
#  - SPECIES SPECIFIC MODEL PARAMETERS 
# output: data frame with species names and modelling parameters (master)
# ---------------------------------
# read master file with species-specific modelling parameters
# i.e. restrict range modelled?, seasonal model?, include substrate? etc...
master = read_xlsx(list.files(pattern = "data_summary_master.xlsx", recursive = TRUE,full.names = TRUE))

# ---------------------------------
#  - PLOTTING LAYERS
# output: plotting layers for model projections (EEZ, coastal provinces, bathymetric contours)
# ---------------------------------
source(list.files(pattern = "plottingparameters.R", recursive = TRUE, full.names= TRUE))

# ---------------------------------
#  - GRID
# output: template grid (5 and 10km resolution)
# ---------------------------------

# decide if you are running the 5km res or 10km res
res = 10
if(res == 5){
  # load appropriate template
  template = raster(list.files(pattern = "template_5km.tif", recursive = TRUE, full.names = TRUE))
  # species with prevalence >1
  master_keep = master %>%
    filter(rounded >=1)
  # number of background points (n_bckg_pts) to use
  # code to figure out 20% of cells: 0.2*length(which(values(template)==1))
  n_bckg_pts = 8410}

if(res == 10){
  # load appropriate template
  template = raster(list.files(pattern = "template_10km.tif", recursive = TRUE, full.names = TRUE))
# species with prevalence >1
  master_keep = master %>%
    filter(rounded_10>=1)
# number of background points (n_bckg_pts) to use
# code to figure out 20% of cells: 0.2*length(which(values(template)==1))
  n_bckg_pts = 2162}

# ---------------------------------
#  - ENVIRONMENTAL VARIABLES 
# output: a predictor variable stack (stack)
# ---------------------------------
source(list.files(pattern = "envnt_variable_stack.R", recursive = TRUE, full.names = TRUE))


# loop goes through each species to run and project the models
for(i in 25:nrow(master_keep)){
  
  # ---------------------------------
  # - MODEL PARAMATERS
  # outputs: all the model parameters that are important in the subscripts
  # ---------------------------------
  target = master_keep$SPECIES_SCIENTIFIC[i] # species name
  folder = "speciesdata/" # for now all data is species only, the other folder if "generadata/"
  substrate = "yes"
  #substrate = master_keep$Substrate[i] # include substrate layer?
  seasonal = "no" # prioritizing aseasonal models for now
  #seasonal = master_keep$Seasonality[i] # run seasonal (summer & winter) model?

  # ---------------------------------
  #  - LOAD SPECIES DATA
  # outputs: occurrences (obs.data) and when applicable polygon occurrences (obs.data_poly)
  # ---------------------------------
  source(list.files(pattern = "species_data.R", recursive = TRUE, full.names = TRUE)) # finds script in directory
  rm(folder) # no longer needed
  
  # ---------------------------------
  #  - SEASONALITY 
  # output: if applicable occurrence points are grouped by austral summer and winter
  # this script also formats aseasonal data
  # ---------------------------------
  source(list.files(pattern = "seasonality.R", recursive = TRUE, full.names = TRUE))
  
  # ---------------------------------
  # - REDUCING SAMPLING BIAS
  # output: subset of occurrence points for static model (pts_sub) and seasonal models (pts_subs_seasons)
  # ---------------------------------
  source(list.files(pattern = "subsampling.R", recursive = TRUE, full.names = TRUE))
  
  # ---------------------------------
  # 8 - BACKGROUND SAMPLE
  # output: data frame of presence absence points for static (pts_env) and seasonal models (pts_env_seasons)
  # ---------------------------------
  source(list.files(pattern ="pseudoabsence_1.R", recursive = TRUE, full.names = TRUE))
  
  # ---------------------------------
  # 9 - COLLINEARITY CHECK
  # output: refines variables to remove co-linear ones (variables)
  # ---------------------------------
  source(list.files(pattern = "variableselection.R", recursive = TRUE, full.names = TRUE))
  
  # ---------------------------------
  # 10 - BIOMOD OBJECT CREATION
  # output: create static and seasonal biomod objects
  # ---------------------------------
  source(list.files(pattern = "Biomod.R", recursive = TRUE, full.names = TRUE))
  
  # ---------------------------------
  # 12 - MODEL RUNS AND PROJECTIONS
  # ---------------------------------
  
  model_type = "Aseasonal" # specify model_type
  data = biomod_obj # specify which biomod_obj
  source(list.files(pattern = "modelling.R", recursive = TRUE, full.names = TRUE)[3])
  #source(list.files(pattern = "Evaluation.R", recursive = TRUE, full.names = TRUE))
  
  if(seasonal == "yes"){
    model_type = "summer" # specify model_type
    season = 1
    data = biomod_obj_seasons[[season]] # specify which biomod_obj
    source(list.files(pattern = "modelling.R", recursive = TRUE, full.names = TRUE)[3])
    #source(list.files(pattern = "Evaluation.R", recursive = TRUE, full.names = TRUE))
    
    model_type = "winter" # specify model_type
    season = 2
    data = biomod_obj_seasons[[season]] # specify which biomod_obj
    source(list.files(pattern = "modelling.R", recursive = TRUE, full.names = TRUE)[3])
    #source(list.files(pattern = "Evaluation.R", recursive = TRUE, full.names = TRUE))
  }
  rm(stack_subset,stack_new)
}
