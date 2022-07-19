# ---------------------------------------------------------------------------------
# AUTHOR: Nina Faure Beaulieu (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script aims to calculate prevalence values for each species
# this will help in reducing the number of models to run
# as a species requires a minimum amount of data for a model to run successfully
# ---------------------------------


# ---------------------------------
# PACKAGES
# ---------------------------------
# list of required packages
requiredpackages = c("sp","dplyr","raster","stringr","lubridate","ggplot2","rgeos","rgdal","dismo","fuzzySim" ,"devtools","mecofun","rasterVis","viridis","readxl","xlsx")
# load packages
lapply(requiredpackages,require, character.only = TRUE)
rm(requiredpackages)
# ---------------------------------


# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------
# set directory to same parent folder where sub-scripts are found
# the subs-scripts can be in folders within this directory as the code will look through all the folders
path =  "/Users/nfb/" # path for mac
my.directory = paste0(path,"Dropbox/6-WILDOCEANS")
# set directory
setwd(my.directory) 
# ---------------------------------


# ---------------------------------
#  - SPECIES MASTER SHEET
# ---------------------------------
# read master file
master = read_xlsx(list.files(pattern = "data_summary_master.xlsx", recursive = TRUE,full.names = TRUE)[1])
# ---------------------------------


# ---------------------------------
#  PRE-MODELING RUN: computes data prevalence per species at 5 and 10 km resolution
# ---------------------------------

# RATIONALE FOR A PRE-MODELING RUN
# for each species this loop
# outputs the abundance of data points
# outputs prevalence of data points (% cells with data out of all cells in study area)
# this helps narrow down which species have enough data to model

list_prevalence_10 = list() # list of prevalence values 10km res
list_abundance = list() # list of abundance values
list_cells_10 = list() # list of cells values 10km res

# each iteration looks at one species from the master sheet
for(i in 1:nrow(master)){

  # template grids (10km resolution)
  template_10 = raster(list.files(pattern = "template_10km.tif", recursive = TRUE, full.names = TRUE))
  # species name
  target = master$SPECIES_SCIENTIFIC[i]
  # folder with data
  folder = "speciesdata/" 

  # SPECIES DATA: formats occurrence points
  # extracts total number of individual data points (removes duplicates of same date+GPS occurrences)
  exampledata = "no"
  source(list.files(pattern = "species_data.R", recursive = TRUE,full.names = TRUE)) # finds script in directory
  rm(folder)
  
  # proceed only if data was available for that species
  if(length(file)>0){
  # PREVALENCE: calculate prevalence score for species data
  source(list.files(pattern = "Prevalence.R", recursive = TRUE,full.names = TRUE))}else{abundance = 0}
  
  # fill empty lists with prevalence and abundance values
  # if absent i.e. no data for species, then fill with 0 (prevents mismatches later on)
  if(exists("perc_10")){  
    list_prevalence_10[[i]] = perc_10}else{
      list_prevalence_10[[i]] = 0}
  if(exists("abundance")){
    list_abundance[[i]] = abundance}else{list_abundance[[count]] = 0}
  if(exists("obscells_10")){
    list_cells_10[[i]] = obscells_10}else{
      list_cells_10[[i]] = 0}
  
  rm(perc, abundance,stack_subset,obscells_10,perc_10) # clear for next species
}
# END OF LOOP ###############

# ---------------------------------
# - FORMATTING STEPS FOR OUTPUT OF PREVALENCE LOOP
# ---------------------------------

# remove
rm(allcells_10,count,substrate,target,i,obs.data,obscells_10,perc_10)

# format number of occurrence points, cells with data and prevalence scores to a data frame
abundance = as.data.frame(unlist(list_abundance))
cells_10 = as.data.frame(unlist(list_cells_10))
prevalence_10 = as.data.frame(unlist(list_prevalence_10))

rm(list_abundance,list_prevalence_10,list_cells_10) # remove

# add species name to prevalence sheet
prevalence_10$SPECIES_SCIENTIFIC = master$SPECIES_SCIENTIFIC
# add abundance values to prevalence sheet
prevalence = cbind(prevalence_10,abundance)
prevalence = cbind(prevalence,cells_10)

rm(abundance,cells_10,prevalence_10) # remove

# rename headers
names(prevalence)[1] = "prevalence_10"
names(prevalence)[3] = "abundance"
names(prevalence)[4] = "cells_10"

# add prevalence and abundance data to master sheet
master$cells_10 = NULL
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
