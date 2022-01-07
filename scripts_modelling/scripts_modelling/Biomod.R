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
#Print_Default_ModelingOptions() # these are the default parameters used for each model

# specify MAXENT path, and polynomial GLM
mxtPh = BIOMOD_ModelingOptions(MAXENT = list(path_to_maxent.jar = paste0(path,"Dropbox/6-WILDOCEANS/Modelling/maxent")),
                               GLM = list(type = 'polynomial'))

# Formatting the data as a BIOMOD object (static)
# TEST WITH PSEUDOABSENCE SELECTION USING BIOMOD
pts_env = na.omit(pts_env) # remove all NAs
pts_env = pts_env %>% # convert 0 to NAs (allows them to be seen as backgorund and not true absence)
  mutate(pa = ifelse(pa==0,NA,pa))
pa = pts_env$pa # presence absence column
pa_xy = pts_env[,c(2,3)] # presence absence coordinates
exp =  pts_env[,-c(1:3)] # environmental variables

biomod_obj =  BIOMOD_FormatingData(resp.var = pa, # presence/absence data
                                                expl.var = exp, # environmental variables
                                                resp.xy = pa_xy,
                                                resp.name = target, # species name
                                   # this will pick the pseudo-absences from your NAs in pa
                                   PA.nb.absences = n_bckg_pts,
                                   PA.nb.rep = 1,
                                   # for high specificity pseudo-absences should be randomly selected 
                                   # OR they can also be chosen at a minimal and maximum distance from presence points
                                   PA.strategy = 'random') 

# Formatting the data as a BIOMOD object (seasonal)
if(seasonal == 'yes'){
biomod_obj_seasons = list()
for(i in 1:length(pts_env_seasons)){
  temp = pts_env_seasons[[i]]
  temp = na.omit(temp) # remove all NAs
  temp = temp %>% # convert 0 to NAs (allows them to be seen as backgorund and not true absence)
    mutate(pa = ifelse(pa==0,NA,pa))
  pa = temp$pa # presence absence column
  pa_xy = temp[,c(2,3)] # presence absence coordinates
  exp =  temp[,-c(1:3)] # environmental variables
  biomod_obj_seasons[[i]] =  BIOMOD_FormatingData(resp.var = pa, # presence/absence data
                                         expl.var = exp, # environmental variables
                                         resp.xy = pa_xy,
                                         resp.name = target, # species name
                                         # random background cells at 20% area of EEZ (5km2 res): 8500
                                         PA.nb.absences = n_bckg_pts,
                                         PA.nb.rep = 1,
                                         # for high specificity pseudo-absences should be randomly selected 
                                         # OR they can also be chosen at a minimal and maximum distance from presence points
                                         PA.strategy = 'random') 
  }} 

