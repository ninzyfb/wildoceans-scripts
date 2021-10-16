# ---------------------------------------------------------------------------------
######### Shark and ray species distribution models (SDMs) - Biomod script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: prepares the data so that it can be run vie the biomod package

# ---------------------------------
# PACKAGES
# biomod2 is loaded in seperately as it intercats with functions from other packages
# ---------------------------------
library(biomod2)

# ---------------------------------
# FORMATTING
# ---------------------------------

# Set model parameters
Print_Default_ModelingOptions() # these are the default parameters used for each model

# specify MAXENT path, and polynomial GLM
mxtPh = BIOMOD_ModelingOptions(MAXENT = list(path_to_maxent.jar = paste0(path,"Dropbox/6-WILDOCEANS/Modelling/maxent")),
                               GLM = list(type = 'polynomial'))

# Formatting the data as a BIOMOD object (static)
pa = pts_env$pa # presence absence column
pa_xy = pts_env[,c(2,3)] # presence absence coordinates
exp =  pts_env[,-c(1:3)] # environmental variables
biomod_obj =  BIOMOD_FormatingData(resp.var = pa, # presence/absence data
                                                expl.var = exp, # environmental variables
                                                resp.xy = pa_xy,
                                                resp.name = target, # species name
                                                na.rm = TRUE) 
rm(pa_xy,exp,pa) # remove unnecessary variables

# Formatting the data as a BIOMOD object (seasonal)
if(seasonal == 'yes'){
biomod_obj_seasons = list()
for(i in 1:length(pts_env_seasons)){
  temp = pts_env_seasons[[i]]
  pa = temp$pa # presence absence column
  pa_xy = temp[,c(2,3)] # presence absence coordinates
  exp =  temp[,-c(1:3)] # environmental variables
  biomod_obj_seasons[[i]] =  BIOMOD_FormatingData(resp.var = pa, # presence/absence data
                                         expl.var = exp, # environmental variables
                                         resp.xy = pa_xy,
                                         resp.name = target, # species name
                                         na.rm = TRUE) 
  }

rm(i,pa_xy,exp,pa,temp) # remove unnecessary variables
rm(pts_env,pts_env_seasons)} # remove unnecessary variables

