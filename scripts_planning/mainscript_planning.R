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

# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------

# all the subscripts are in the scripts folder
# all the data is in the planning folder
# make sure you set the directory to the overall WILDOCEANS folder so you can find both
path = "/Users/nfb/"
setwd(paste0(path,"Dropbox/6-WILDOCEANS"))

# ---------------------------------
# 1 - DEFINE PLANNING SCENARIO 
# these parameters define what happens in the subscripts
# these parameters need to be entered manually depending on which species you want to model
# ---------------------------------

# load scenario sheet
scenario_sheet = read_xlsx(path=paste0(path,"Dropbox/6-WILDOCEANS/Planning/scenarios.xlsx"),sheet = 1)

# load data summary sheet
master_sheet = read_xlsx(path=paste0(path,"Dropbox/6-WILDOCEANS/data_summary_master.xlsx"),sheet = 1)

# filter to only keep modelled species
master_sheet = master_sheet %>%
  filter(Round3_static == "yes")

# ---------------------------------
# 2 - PLANNING UNITS
# output: planning unit grid (eez with no values)
# ---------------------------------
source(list.files(pattern = "Planningunit.R", recursive = TRUE)) 

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
source(list.files(pattern = "Lockedin.R", recursive = TRUE))

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
