# ---------------------------------------------------------------------------------
######### Shark and ray species distribution models (SDMs) - Parent script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: This is the parent script which calls all of the other scripts in order to build SDMs for a multitude of shark and ray species
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
# all the subscripts are in the scripts_modelling folder
# make sure to set the directory to the same one within which the scripts are found
# the scripts can be in a separate folder within the directory

# define your path
# for me it changes based on if I am working on pc or mac
#path for linux
path = "/home/nina/Documents/"
path =  "/Users/nfb/"
setwd(paste0(path,"Dropbox/6-WILDOCEANS"))

# ---------------------------------
#  - ENVIRONMENTAL VARIABLES
# output: a predictor variable raster stack (stack)
# ---------------------------------
source(list.files(pattern = "envnt_variable_stack.R", recursive = TRUE))

# ---------------------------------
#  - MARINE REGIONS
# output: three marine regions within the EEZ (regions)
# ---------------------------------

# three marine regions as defined by Ebert et al.
# this is used to refine the range of range restricted species
regions = shapefile(list.files(pattern = "ebert_regions.shp", recursive = TRUE,full.names = TRUE))
# turn region names to upper case
regions$Region = toupper(regions$Region)

# ---------------------------------
#  - SPECIES SPECIFIC MODEL PARAMETERS 
# output: a data frame with species names and specific modelling features (master)
# ---------------------------------

# read master file
# species-specific modelling features i.e. to restrict the range modelled or not
master = read_xlsx(list.files(pattern = "data_summary_master.xlsx", recursive = TRUE,full.names = TRUE))

# ---------------------------------
#  - PRE-MODELING RUN
# run some initial functions to check data prevalence per species
# this filters which species to keep for model runs
# ---------------------------------

# this loop looks at the data for each species
# it outputs the abundance of data points as well as prevalence
# prevalence is the percentage of cells with data out of all cells in study area
# this helps narrow down which species have enough data to model
# IMPORTANT: THIS STEP CAN BE SKIPPED, GO STRAIGHT TO MODELING WORKFLOW

count = 1 # count to fill list with prevalence and abundance values
list_prevalence = list() # list of prevalence values
list_abundance = list() # list of abundance values

for(i in 1:nrow(master)){

  # ---------------------------------
  # - MODEL PARAMATERS
  # outputs: extracts species-specific model parameters from master sheet
  # ---------------------------------
  target = master$SPECIES_SCIENTIFIC[i] # species name
  folder = "speciesdata/" # for now all data is species only, the other folder if "generadata/"
  substrate = master$Substrate[i] # inclusion or not of substrate layer?
  fisheries = master$Fisheries[i] # incorporate fisheries data?
  restrictedrange = master$Restricted_range[i] # range restriction?
  if(restrictedrange == "yes"){ # specify which areas to clip range to
    range = toupper(master$areas[i]) # extract areas
    range = c(strsplit(range,",")[[1]][1],strsplit(range,",")[[1]][2]) # collate them
    }
  
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
  if(fisheries == "yes"){
    source(list.files(pattern = "fisheries data.R", recursive = TRUE)) # list.files() allows you to search for that script anywhere in the parent folder
  }
  rm(fisheries) # no longer needed
  
  # ---------------------------------
  #  - SEASONALITY 
  # output: occurrence points are grouped by austral summer and winter
  # ---------------------------------
  source(list.files(pattern = "seasonality.R", recursive = TRUE, full.names = TRUE))
  
  # ---------------------------------
  #  - CROP MODEL EXTENT
  # output: if applicable refines range to be modelled
  # ---------------------------------
  if(restrictedrange == "yes"){
    source(list.files(pattern = "modelextent.R", recursive = TRUE,full.names = TRUE))}

  # ---------------------------------
  #  - PREVALENCE
  # output: calculate prevalence score for species data
  # ---------------------------------
  source(list.files(pattern = "Prevalence.R", recursive = TRUE,full.names = TRUE))
  
  }
  if(exists("perc")){  # if applicable fill list with prevalence value
    list_prevalence[[count]] = perc}else{list_prevalence[[count]] = 0}
  if(exists("abundance")){  # if applicable fill list with abundance value
    list_abundance[[count]] = abundance}else{list_abundance[[count]] = 0}
  
  count = count+1 # increase count
  rm(perc, abundance,stack_subset) # clear for next species
}

# remove
rm(allcells,count,restrictedrange,range,seasonal,substrate,target,i,files,table,obs.data,stack)

# ---------------------------------
#  - REFINE SPECIES TO MODEL BASED ON PREVALENCE
# ---------------------------------

# Get all prevalence scores in a data frame
prevalence = as.data.frame(unlist(list_prevalence))
# Get all abundance scores in a data frame
abundance = as.data.frame(unlist(list_abundance))
rm(list_abundance,list_prevalence) # remove
# add species name
prevalence$species = master$SPECIES_SCIENTIFIC
# add abundance values
prevalence = cbind(prevalence,abundance)
rm(abundance) # remove
# rename headers
names(prevalence)[1] = "prevalence"
names(prevalence)[3] = "abundance"
# round prevalence value to 1 integer
prevalence$rounded = round(prevalence$prevalence, digits = 0)
# as a rule we are only keeping species with a prevalence of 1 or above
species_keep = prevalence %>%
  filter(rounded >=1 )
write.csv(species_keep,"species_tokeep.csv") # write csv of kept species
write.csv(prevalence,"species_all.csv") # write csv of all species

# ---------------------------------
# - CREATE RAW PLOTS
# output: plots for kept species with raw data and when available IUCN range + expert range
# ---------------------------------
source(list.files(pattern = "rawplots.R", recursive = TRUE))

# ---------------------------------
# - MODELLING WORKFLOW
# the following loop will now run through each kept species again and run the models
# ---------------------------------
species_keep = read.csv(list.files(pattern ="species_tokeep.csv" ,full.names=TRUE,recursive = TRUE))

# if prevalence loop was run then filter master file to only keep relevant species
if(exists("species_keep")){master_keep = master %>% filter(SPECIES_SCIENTIFIC %in% species_keep$species)}else{
  # if wanting to model a specific species enter species name
  master_keep = master %>% filter(SPECIES_SCIENTIFIC == "SPECIES NAME HERE")}

rm(master,species_keep)

# TEMPORARY
temp_sheet = master_keep %>%
  filter(Restricted_range == "yes")

# ---------------------------------
#  - PLOTTING LAYERS
# output: plotting layers for model projections (EEZ, coastal provinces, bathymetric contours)
# ---------------------------------
source(list.files(pattern = "plottingparameters.R", recursive = TRUE, full.names= TRUE))

# ---------------------------------
#  - ENVIRONMENTAL VARIABLES 
# output: a predictor variable stack (stack)
# ---------------------------------
source(list.files(pattern = "envnt_variable_stack.R", recursive = TRUE, full.names = TRUE))


# loop goes through each species to run and project the models
for(i in 1:nrow(master_keep)){
  
  # ---------------------------------
  # - MODEL PARAMATERS
  # outputs: all the model parameters that are important in the subscripts
  # ---------------------------------
  target = master_keep$SPECIES_SCIENTIFIC[i] # species name
  folder = "speciesdata/" # for now all data is species only, the other folder if "generadata/"
  substrate = master_keep$Substrate[i] # include substrate layer?
  seasonal = "no"
  #seasonal = master_keep$Seasonality[i] # run seasonal (summer & winter) model?
  fisheries = "no" # incorporate fisheries data?
  restrictedrange = "no" # is the range restricted?
  if(restrictedrange == "yes"){ # specify range if applicable
    range = toupper(master_keep$areas[i]) # extract areas
    range = c(strsplit(range,",")[[1]][1],strsplit(range,",")[[1]][2]) # collate them
  }
  
  # ---------------------------------
  #  - LOAD SPECIES DATA
  # outputs: occurrences (obs.data) and when applicable polygon occurrences (obs.data_poly)
  # ---------------------------------
  source(list.files(pattern = "species_data.R", recursive = TRUE, full.names = TRUE)) # finds script in directory
  rm(folder) # no longer needed
  
    # ---------------------------------
    #  - ADD FISHERIES DATA
    # outputs: adds fishing data to obs.data if applicable
    # ---------------------------------
    if(fisheries == "yes"){
      source(list.files(pattern = "fisheries data.R", recursive = TRUE, full.names = TRUE)) # list.files() allows you to search for that script anywhere in the parent folder
    }
    rm(fisheries) # no longer needed
    
    # ---------------------------------
    #  - SEASONALITY 
    # output: if applicable occurrence points are grouped by austral summer and winter
    # this script also formats aseasonal data
    # ---------------------------------
    source(list.files(pattern = "seasonality.R", recursive = TRUE, full.names = TRUE))

    # ---------------------------------
    #  - CROP MODEL EXTENT
    # output: refines the range that will be modelled if required for the target species
    # ---------------------------------
    if(restrictedrange == "yes"){
      source(list.files(pattern = "modelextent.R", recursive = TRUE, full.names = TRUE))}
    
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
    # output: refines variables to remove collinear ones (variables)
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
    source(list.files(pattern = "modelling.R", recursive = TRUE, full.names = TRUE))
    #source(list.files(pattern = "Evaluation.R", recursive = TRUE, full.names = TRUE))
    
    if(seasonal == "yes"){
      model_type = "summer" # specify model_type
      season = 1
      data = biomod_obj_seasons[[season]] # specify which biomod_obj
      source(list.files(pattern = "modelling.R", recursive = TRUE, full.names = TRUE))
      #source(list.files(pattern = "Evaluation.R", recursive = TRUE, full.names = TRUE))
      
      model_type = "winter" # specify model_type
      season = 2
      data = biomod_obj_seasons[[season]] # specify which biomod_obj
      source(list.files(pattern = "modelling.R", recursive = TRUE, full.names = TRUE))
      #source(list.files(pattern = "Evaluation.R", recursive = TRUE, full.names = TRUE))
    }
    rm(stack_subset,stack_new)
}
