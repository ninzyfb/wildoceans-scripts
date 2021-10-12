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
library(biomod2)
library(rasterVis)
library(viridis)
library(readxl)

# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------

# all the subscripts are in the scripts folder
# make sure you set the directory to the same one within which the scripts are found
# the scripts can be in a separate folder in this directory, they just need to be somewhere within the directory

# define your path
# for me it changed based on if i was working on pc or mac
#path =  "C:/Users/Administrator/"
path =  "/Users/nfb/"
setwd(paste0(path,"Dropbox/6-WILDOCEANS"))

# ---------------------------------
#  - ENVIRONMENTAL VARIABLES 
# output: a predictor variable stack (stack)
# ---------------------------------
source(list.files(pattern = "envnt_variable_stack.R", recursive = TRUE))

# ---------------------------------
#  - DEFINE MODEL PARAMETERS 
# these parameters define what happens in the subscripts
# these parameters need to be entered manually depending on which species you want to model
# ---------------------------------

# read master sheet with all species and modelling parameters
master = read_xlsx(paste0(path,"Dropbox/6-WILDOCEANS/data_summary_master.xlsx"))

# this loop goes through each species
# it specifies all the model parameters first
# then it loads all the data and calculates the prevalence of data for that species
# this helps narrow down which species have enough data to model

count = 1 # this is relevant for the prevalence script
list = list() # this is relevant for the prevalence script

for(i in 1:nrow(master)){
  # ---------------------------------
  # - MODEL PARAMATERS
  # outputs: all the model parameters that are important in the subscripts
  # ---------------------------------
  target = master$SPECIES_SCIENTIFIC[i] # species name
  folder = "speciesdata/" # for now all data is species only, the other folder if "generadata/"
  substrate = master$Round3_Substrate[i] # include substrate layer (species dependent based on species sheets and workshops)
  seasonal = master$Round3_seasonal[i] # run seasonal (summer & winter) model?
  fisheries = master$Round3_Fisheries[i] # incorporate fisheries data?
  restrictedrange = master$Restricted_range[i] # is the range restricted? (this is  species dependent based on their species sheets and workshops)
  if(restrictedrange == "yes"){ # specify range if needs to be clipped to
    range = toupper(master$areas[i]) # extract areas
    range = c(strsplit(range,",")[[1]][1],strsplit(range,",")[[1]][2]) # collate them
    } # options are one or two from East, South, West
  
  # ---------------------------------
  #  - LOAD SPECIES DATA
  # outputs: occurrences (obs.data) and when applicable polygon occurrences (obs.data_poly)
  # ---------------------------------
  source(list.files(pattern = "species_data.R", recursive = TRUE)) # finds script in directory
  rm(folder,group) # no longer needed
  
  if(length(files)>0){
  
  # ---------------------------------
  #  - ADD FISHERIES DATA
  # outputs: adds fishing data to obs.data if applicable
  # ---------------------------------
  if(fisheries == "yes"){
    source(list.files(pattern = "fisheries data.R", recursive = TRUE)) # list.files() allows you to search for that script anywhere in the parent folder
  }
  rm(fisheries) # no longer needed
  
  # ---------------------------------
  #  - SEASONALITY 
  # output: occurrence points can now be grouped by season (summer and winter)
  # ---------------------------------
  # run seasonality script
  if(seasonal == "yes"){source(list.files(pattern = "seasonality.R", recursive = TRUE))}
  # running an aseasonal model only then simply format data
  if(seasonal == "no"){
    obs.data$LONGITUDE = as.numeric(obs.data$LONGITUDE)
    obs.data$LATITUDE = as.numeric(obs.data$LATITUDE)
    coordinates(obs.data) =  ~ cbind(obs.data$LONGITUDE,obs.data$LATITUDE)
    # set CRS of observations based on CRS of template
    template = raster(list.files(pattern = "template.tif", recursive = TRUE))
    crs(obs.data) = crs(template)}
  
  # ---------------------------------
  #  - CROP MODEL EXTENT
  # output: refines the range that will be modelled if required for the target species
  # ---------------------------------
  if(restrictedrange == "yes"){ # defined in model parameters
    source(list.files(pattern = "modelextent.R", recursive = TRUE))}

  # ---------------------------------
  #  - PREVALENCE
  # output: refines the range that will be modelled if required for the target species
  # ---------------------------------
  source(list.files(pattern = "Prevalence.R", recursive = TRUE))
  
  }
  if(exists("perc")){
  list[[count]] = perc}else{list[[count]] = 0}
  count = count+1
  rm(perc)
}

rm(allcells,count,restrictedrange,range,seasonal,substrate,target,i,files,table,obs.data,stack_temp) # remove

# ---------------------------------
#  - PREVALENCE (continued)
# ---------------------------------

# Get all prevalence scores in a data frame
prevalence = as.data.frame(unlist(list))
# add species name
prevalence$species = master$SPECIES_SCIENTIFIC
# round to 1 integer
# as a rule we are only keeping species with a prevalence of 1 or above
names(prevalence)[1] = "prevalence"
prevalence$rounded = round(prevalence$prevalence, digits = 0)
# filter to keep only species with prevalence values of 1 or higher
species_keep = prevalence %>%
  filter(rounded >=1 )
rm(prevalence,list) # remove

# ---------------------------------
# - CREATE RAW PLOTS
# output: plots with raw data and when available IUCN extent and expert extent
# ---------------------------------
source(list.files(pattern = "rawplots.R", recursive = TRUE))

# ---------------------------------
#  - MODEL PLOTTING PARAMETERS
# output: additional features required for plotting (eez, provinces, contours)
# ---------------------------------
source(list.files(pattern = "plottingparameters.R", recursive = TRUE))

# ---------------------------------
# - RUN MODELLING WORKFLOW ON SPECIES WITH HIGH ENOUGH PREVALENCE
# output: plots with raw data and when available IUCN extent and expert extent
# ---------------------------------
master_keep = master %>%
  filter(SPECIES_SCIENTIFIC %in% species_keep$species)

# ! If you just want to run the modelling code you can
# just follow the steps below
master_keep = master %>%
  filter(SPECIES_SCIENTIFIC == "RAJA OCELLIFERA")

# looping through each species to produce the model
for(i in 1:nrow(master_keep)){
  # ---------------------------------
  # - MODEL PARAMATERS
  # outputs: all the model parameters that are important in the subscripts
  # ---------------------------------
  target = master_keep$SPECIES_SCIENTIFIC[i] # species name
  folder = "speciesdata/" # for now all data is species only, the other folder if "generadata/"
  substrate = master_keep$Substrate[i] # include substrate layer (species dependent based on species sheets and workshops)
  seasonal = master_keep$Seasonality[i] # run seasonal (summer & winter) model?
  fisheries = master_keep$Fisheries[i] # incorporate fisheries data?
  restrictedrange = master_keep$Restricted_range[i] # is the range restricted? (this is  species dependent based on their species sheets and workshops)
  if(restrictedrange == "yes"){ # specify range if needs to be clipped to
    range = toupper(master_keep$areas[i]) # extract areas
    range = c(strsplit(range,",")[[1]][1],strsplit(range,",")[[1]][2]) # collate them
  } # options are one or two from East, South, West
  
  # ---------------------------------
  #  - ENVIRONMENTAL VARIABLES 
  # output: a predictor variable stack (stack)
  # ---------------------------------
  source(list.files(pattern = "envnt_variable_stack.R", recursive = TRUE))
  
  # ---------------------------------
  #  - LOAD SPECIES DATA
  # outputs: occurrences (obs.data) and when applicable polygon occurrences (obs.data_poly)
  # ---------------------------------
  source(list.files(pattern = "species_data.R", recursive = TRUE)) # finds script in directory
  rm(folder,group) # no longer needed
  
    # ---------------------------------
    #  - ADD FISHERIES DATA
    # outputs: adds fishing data to obs.data if applicable
    # ---------------------------------
    if(fisheries == "yes"){
      source(list.files(pattern = "fisheries data.R", recursive = TRUE)) # list.files() allows you to search for that script anywhere in the parent folder
    }
    rm(fisheries) # no longer needed
    
    # ---------------------------------
    #  - SEASONALITY 
    # output: occurrence points can now be grouped by season (summer and winter)
    # ---------------------------------
    # run seasonality script (this only plots observations per month and season so shoudl be run irrespective of if running seasonal models)
    source(list.files(pattern = "seasonality.R", recursive = TRUE))

    # ---------------------------------
    #  - CROP MODEL EXTENT
    # output: refines the range that will be modelled if required for the target species
    # ---------------------------------
    if(restrictedrange == "yes"){ # defined in model parameters
      source(list.files(pattern = "modelextent.R", recursive = TRUE))}
    
    # ---------------------------------
    # - REDUCING SAMPLING BIAS
    # output: a subset of occurrence points for static model (pts_sub) and seasonal model (pts_subs_seasons)
    # ---------------------------------
    source(list.files(pattern = "subsampling.R", recursive = TRUE))
    
    # ---------------------------------
    # 8 - BACKGROUND SAMPLE
    # output: concatenated data frame of presence absence points for static (pts_env) and seasonal models (pts_env_seasons)
    # ---------------------------------
    source(list.files(pattern ="pseudoabsence_1.R", recursive = TRUE))
    
    # ---------------------------------
    # 9 - COLLINEARITY CHECK
    # output: set of chosen variables (variables)
    # ---------------------------------
    source(list.files(pattern = "variableselection.R", recursive = TRUE)) # list.files() allows you to search for that script anywhere in the parent folder
    
    # ---------------------------------
    # 10 - BIOMOD OBJECT CREATION
    # output: static and seasonal biomod objects
    # ---------------------------------
    source(list.files(pattern = "Biomod.R", recursive = TRUE))
    
    # Ctrl + Alt + E tu run all code from this line to end
    
    # ---------------------------------
    # 12 - MODEL RUNS AND PROJECTIONS
    # output: 
    # ---------------------------------
    #detach("package:ggplot2", unload = TRUE) # ggplot2 seems to mess with the plotting down the line
    
    model_type = "Aseasonal" # specify model_type
    data = biomod_obj # specify which biomod_obj
    source(list.files(pattern = "modelling_static", recursive = TRUE))
    
    if(seasonal == "yes"){
      model_type = "summer"
      season = 1
      data = biomod_obj_seasons[[season]]
      source(list.files(pattern = "modelling_static", recursive = TRUE))
      
      model_type = "winter"
      season = 2
      data = biomod_obj_seasons[[season]]
      source(list.files(pattern = "modelling_static", recursive = TRUE))}

}
