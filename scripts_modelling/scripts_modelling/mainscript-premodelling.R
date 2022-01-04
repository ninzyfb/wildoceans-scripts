# ---------------------------------------------------------------------------------
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: This is the parent pre-modelling script
# it calls all sub-scripts
# the aim is to calculate prevalence values for each species and produce some basic plots
# !! Run each subscript one at a time. Running the whole parent script at once seems to cause some issues
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
library(rgeos)
library(rgdal)
library(dismo)
library(fuzzySim) 
library(devtools)
library(mecofun)
library(rasterVis)
library(viridis)
library(readxl)
library(xlsx)

# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------
# set directory to same parent folder where sub-scripts are found
# the subs-scripts can however be in folder within parent folder
path = "/home/nina/Documents/" #path for linux
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
#  - PRE-MODELING RUN
# to look at data prevalence per species
# IMPORTANT: THIS STEP CAN BE SKIPPED, GO STRAIGHT TO MODELING WORKFLOW
# ---------------------------------

# RATIONALE FOR A PRE-MODELING RUN
# for each species this loop
# outputs the abundance of data points
# outputs prevalence of data points (% cells with data out of all cells in study area)
# this helps narrow down which species have enough data to model

count = 1 # count to fill lists during iterations
list_prevalence = list() # list of prevalence values 5km res
list_prevalence_10 = list() # list of prevalence values 10km res
list_abundance = list() # list of abundance values
list_cells = list() # list of cells values 5km res
list_cells_10 = list() # list of cells values 10km res

# each iteration looks at one species from the master sheet
for(i in 1:nrow(master)){

  # template grid (5 and 10km resolution)
  template = raster(list.files(pattern = "template.tif", recursive = TRUE, full.names = TRUE))
  template_10 = raster(list.files(pattern = "template_10km.tif", recursive = TRUE, full.names = TRUE))

  # ---------------------------------
  # - MODEL PARAMATERS
  # output: species-specific model parameters
  # ---------------------------------
  target = master$SPECIES_SCIENTIFIC[i] # species name
  folder = "speciesdata/" # for now all data is species only, the other folder if "generadata/"
  substrate = master$Substrate[i] # inclusion of substrate layer?
  restrictedrange = "no" # for now I am not restricted the range of species 
  #restrictedrange = master$Restricted_range[i] # range restriction?
  #if(restrictedrange == "yes"){ # specify which areas to clip range to
  #  range = toupper(master$areas[i]) # name chosen areas
  #  range = c(strsplit(range,",")[[1]][1],strsplit(range,",")[[1]][2]) # collate them into a vector
  #  }
  
  # ---------------------------------
  #  - SPECIES DATA
  # outputs: occurrences (obs.data)
  # ---------------------------------
  source(list.files(pattern = "species_data.R", recursive = TRUE,full.names = TRUE)) # finds script in directory
  rm(folder) # no longer needed
  
  if(length(files)>0){ # proceed only if data was available for that species
  
  # ---------------------------------
  #  - FISHERIES DATA
  # outputs: if applicable adds fishing data to obs.data
  # ---------------------------------
  source(list.files(pattern = "fisheries data.R", recursive = TRUE)) # list.files() allows you to search for that script anywhere in the parent folder

  # ---------------------------------
  #  - SEASONALITY 
  # output: occurrence points are grouped by austral summer and winter
  # ---------------------------------
  source(list.files(pattern = "seasonality.R", recursive = TRUE, full.names = TRUE))
  
  # ---------------------------------
  #  - CROP MODEL EXTENT
  # output: if applicable refines range to be modelled
  # ---------------------------------
  #if(restrictedrange == "yes"){
  #  source(list.files(pattern = "modelextent.R", recursive = TRUE,full.names = TRUE))}

  # ---------------------------------
  #  - PREVALENCE
  # output: calculate prevalence score for species data
  # ---------------------------------
  source(list.files(pattern = "Prevalence.R", recursive = TRUE,full.names = TRUE))
  
  }
  # fill empty lists with prevalence and abundance values
  # if absent i.e. no data for species, then fill with 0 (prevents mismatches later on)
  if(exists("perc")){  
    list_prevalence[[count]] = perc}else{
      list_prevalence[[count]] = 0}
  if(exists("perc_10")){  
    list_prevalence_10[[count]] = perc_10}else{
      list_prevalence_10[[count]] = 0}
  if(exists("abundance")){
    list_abundance[[count]] = abundance}else{list_abundance[[count]] = 0}
  if(exists("obscells")){
    list_cells[[count]] = obscells}else{
      list_cells[[count]] = 0}
  if(exists("obscells_10")){
    list_cells_10[[count]] = obscells_10}else{
      list_cells_10[[count]] = 0}
  
  count = count+1 # increase count
  rm(perc, abundance,stack_subset,obscells) # clear for next species
}
# END OF LOOP ###############

# ---------------------------------
# - FORMATTING STEPS FOR OUTPUT OF PREVALENCE LOOP
# ---------------------------------

# remove
rm(allcells,allcells_10,count,restrictedrange,substrate,target,i,files,table,obs.data,obscells_10,perc_10)

# format number of occurrence points, cells with data and prevalence scores to a data frame
abundance = as.data.frame(unlist(list_abundance))

cells = as.data.frame(unlist(list_cells))
cells_10 = as.data.frame(unlist(list_cells_10))

prevalence = as.data.frame(unlist(list_prevalence))
prevalence_10 = as.data.frame(unlist(list_prevalence_10))

rm(list_abundance,list_prevalence, list_prevalence_10,list_cells,list_cells_10) # remove

# add species name to prevalence sheet
prevalence$SPECIES_SCIENTIFIC = master$SPECIES_SCIENTIFIC
# add abundance values to prevalence sheet
prevalence = cbind(prevalence,prevalence_10)
prevalence = cbind(prevalence,abundance)
prevalence = cbind(prevalence,cells)
prevalence = cbind(prevalence,cells_10)

rm(abundance,cells,cells_10,prevalence_10) # remove

# rename headers
names(prevalence)[1] = "prevalence"
names(prevalence)[3] = "prevalence_10"
names(prevalence)[4] = "abundance"
names(prevalence)[5] = "cells"
names(prevalence)[6] = "cells_10"

# round prevalence value to 1 integer
prevalence$rounded = round(prevalence$prevalence, digits = 0)
prevalence$rounded_10 = round(prevalence$prevalence_10, digits = 0)

# add prevalence and abundance data to master sheet
master$cells = NULL
master$cells_10= NULL
master$rounded= NULL
master$rounded_10= NULL
master$prevalence = NULL
master$prevalence_10 = NULL
master$abundance = NULL
master = left_join(master,prevalence)

# re-write master sheet
write.xlsx(as.data.frame(master),"data_summary_master.xlsx",row.names = FALSE)

# ---------------------------------
# - CREATE RAW PLOTS
# output: plots for kept species with raw data and when available IUCN range + expert range
# ---------------------------------
source(list.files(pattern = "rawplots.R", recursive = TRUE))
