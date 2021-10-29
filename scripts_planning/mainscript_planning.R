# ---------------------------------------------------------------------------------
######### Shark and ray species conservation planning using prioritizr - Parent script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
#!Run each script one at a time as running the whole at once seems to cause some bugs
#the output line describes what each script produces
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: This is the parent script which calls all of the other scripts
####

# ---------------------------------
# PACKAGES
# ---------------------------------
library(dplyr)
library(tidyr)
library(prioritizr)
library(gurobi)
library(stringr)
library(rasterVis)
library(viridis)
library(rgeos)
library(raster)
library(scales)
library(readxl)
library(fasterize)
library(sdmvspecies)

# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------

# all the subscripts are in the scripts folder
# all the data is in the planning folder
# make sure you choose the coorect patg and set the directory to the overall WILDOCEANS folder so you can find both
path = "/home/nina/Documents/" # linux path
path = "/Users/nfb/" # mac path
setwd(paste0(path,"Dropbox/6-WILDOCEANS"))

# ---------------------------------
# 1 - DEFINE PLANNING SCENARIO 
# these parameters define what happens in the subscripts
# these parameters need to be entered manually depending on which species you want to model
# ---------------------------------

# load scenario sheet
#scenario_sheet = read_xlsx(path=paste0(path,"Dropbox/6-WILDOCEANS/Planning/scenarios.xlsx"),sheet = 1)

# load data summary sheet
#master_sheet = read_xlsx(path=paste0(path,"Dropbox/6-WILDOCEANS/data_summary_master.xlsx"),sheet = 1)

# filter to only keep modeled species
#master_sheet = master_sheet %>%
#  filter(Round3_static == "yes")

# ---------------------------------
# 2 - PLANNING UNIT (pu)
# output: planning unit grid (EEZ with uniform value of 1)
# ---------------------------------
# grid cell value = cost of grid cell
# this is the basic planning unit, every 5km2 grid cell has a value of 1
pu = raster(list.files(pattern = "template.tif",full.names = TRUE,recursive = TRUE))

# ---------------------------------
# 3 - CONSERVATION FEATURES
# output: 
# ---------------------------------
source(list.files(pattern = "Conservationfeatures.R", recursive = TRUE)) 

# ---------------------------------
# 4 - COSTS
# output: 
# ---------------------------------
source(list.files(pattern = "costs_2018NBA.R", recursive = TRUE))

# ---------------------------------
# 5 - LOCKED IN AREAS
# output: 
# ---------------------------------
#source(list.files(pattern = "Lockedin.R", recursive = TRUE))

# ---------------------------------
# 6 - CONSERVATION TARGETS
# output: 
# ---------------------------------
source(list.files(pattern = "Speciestargets.R", recursive = TRUE)) 

# ---------------------------------
# 6 - CONSERVATION PROBLEM
# output: prioritizr problem object
# ---------------------------------
source(list.files(pattern = "Problem.R", recursive = TRUE))

# ---------------------------------
# 7 - CONSTRAINTS
# output: 
# ---------------------------------

# ---------------------------------
# 8 - SOLUTION
# output: prioritizr solution object
# ---------------------------------
source(list.files(pattern = "Solution", recursive = TRUE)) 

# ---------------------------------
# 9 - PERFORMANCE
# output: 
# ---------------------------------
source(list.files(pattern = "Performances", recursive = TRUE)) 




# Name the scenario
scenario = "A" # the description of each scenario is in a CSV file

p = 0 # this should be the problem number you want - 1

costs = "yes" # does it include costs?

penalty = 0 # is there a penalty?
