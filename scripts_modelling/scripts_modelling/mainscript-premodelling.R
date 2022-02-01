# ---------------------------------------------------------------------------------
# AUTHOR: Nina Faure Beaulieu (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script aims to calculate prevalence values for each species
# this will help in reducing the number of models to run
# as a species requires a minimum amount of data for a model to run succesfully
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
# the subs-scripts can however be in folder within parent folder
path = "/home/nina/Documents/" #path for linux
path =  "/Users/nfb/" # path for mac
setwd(paste0(path,"Dropbox/6-WILDOCEANS")) # set directory
# ---------------------------------


# ---------------------------------
#  - SPECIES SPECIFIC MODEL PARAMETERS 
# output: data frame with species names and modelling parameters (master)
# ---------------------------------
# read master file with species-specific modelling parameters
master = read_xlsx(list.files(pattern = "data_summary_master.xlsx", recursive = TRUE,full.names = TRUE))
# read in file names for which there is actually data for
alldata = list.files(path = paste0(path,"Dropbox/6-WILDOCEANS/Modelling/speciesdata"),pattern = ".csv",recursive = TRUE,full.names = TRUE, ignore.case = TRUE)
# extract species name from file name
alldata = str_split(alldata, "/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/speciesdata/", simplify = TRUE)[,2]
alldata = str_split(alldata, ".csv", simplify = TRUE)[,1]

# find species with data but that are not in master sheet
excl = which(alldata %in% master$SPECIES_SCIENTIFIC == FALSE)
excl = as.data.frame(alldata[excl])
colnames(excl) = "SPECIES_SCIENTIFIC"
# add these species to master sheet
master = full_join(master,excl)
rm(excl)

# filter out species in master sheet but with no data
master = master %>%
  filter((SPECIES_SCIENTIFIC %in% alldata))

# find any species with only genus
names = as.data.frame(str_split(master$SPECIES_SCIENTIFIC, " ", simplify = TRUE))
# filter master sheet
master = master[which(names$V2 != ""),]

# ---------------------------------
#  PRE-MODELING RUN: computes data prevalence per species at 5 and 10 km resolution
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
for(i in 157:nrow(master)){

  # template grids (5 and 10km resolution)
  template = raster(list.files(pattern = "template_5km.tif", recursive = TRUE, full.names = TRUE))
  template_10 = raster(list.files(pattern = "template_10km.tif", recursive = TRUE, full.names = TRUE))
  # species name
  target = master$SPECIES_SCIENTIFIC[i]
  # folder with data
  folder = "speciesdata/" 

  # SPECIES DATA: formats occurrence points
  source(list.files(pattern = "species_data.R", recursive = TRUE,full.names = TRUE)) # finds script in directory
  rm(folder)
  
  # proceed only if data was available for that species
  if(length(files)>0){
  # PREVALENCE: calculate prevalence score for species data
  source(list.files(pattern = "Prevalence.R", recursive = TRUE,full.names = TRUE))}
  
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
rm(allcells,allcells_10,count,substrate,target,i,files,obs.data,obscells_10,perc_10)

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
master$cells_10 = NULL
master$rounded = NULL
master$rounded_10 = NULL
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
