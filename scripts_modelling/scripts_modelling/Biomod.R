# ---------------------------------------------------------------------------------
# AUTHOR: Nina Faure Beaulieu (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script packages the data in a biomod object
# this allows it to run using the biomod2 package
# ---------------------------------


# ---------------------------------
# PACKAGES
# ---------------------------------
# biomod2 is loaded in seperately as it interacts with functions from other packages
detach(package:dismo,unload=TRUE)
library(biomod2)
# ---------------------------------


# ---------------------------------
# FORMATTING
# ---------------------------------

# this allows you to see the default model parameters used by biomod
Print_Default_ModelingOptions() # these are the default parameters used for each model

# default parameters can be altered using BIOMOD_ModelingOptions()
# in our case we will be using MAXENT so i need to specify the path to my downloaded MAXENT file
# i also want to specify the use of a polynomial GLM
mxtPh = BIOMOD_ModelingOptions(MAXENT = list(path_to_maxent.jar = paste0(path,"Dropbox/6-WILDOCEANS/Modelling/maxent")),
                               GLM = list(type = 'polynomial'))

# remove all NAs
pts_env = na.omit(pts_env)

# convert 0 to NAs
# this is important as it means they ar eseen as background points and not true absences
pts_env = pts_env %>% mutate(pa=ifelse(pa==0,NA,pa))

# isolate presence/background points column of 1s and NAs
pa = pts_env$pa 

# isolate presence/background points coordinates
pa_xy = pts_env[,c(2,3)] 

# isolate environmental variables columns (from column 4 onwards)
exp =  pts_env[,-c(1:3)] 

# from all your generated background points in the pseudo-absence script
# pick the same number as your number of presence points
# this is because for each model run we chose to have the same number of presences as number of background points
# and we run each model with 2 sets of background points (specified by PA.nb.rep = 2)
pseudoabsences = length(which(pa==1))

# make sure substrate is a factor
exp$substrate_simplified = as.factor(exp$substrate_simplified)

# biomod object
biomod_obj =  BIOMOD_FormatingData(resp.var = pa, # presence/background data
                                                expl.var = exp, # environmental variables
                                                resp.xy = pa_xy, # response variable coordinates
                                                resp.name = target, # species name
                                   # this will pick the pseudo-absences from your NAs in pa
                                   PA.nb.absences = pseudoabsences,
                                   # two sets of pseudoabsences to be chosen
                                   PA.nb.rep = 2,
                                   # for high specificity pseudo-absences should be randomly selected 
                                   PA.strategy = 'random') 

# Formatting the seasonal data as a BIOMOD object
# the following code follows the exact same procedure as above
# it simply does it for summer and winter points seperately
if(seasonal == 'yes' & !is.na(seasonal)){
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
                                         PA.nb.absences = pseudoabsences,
                                         PA.nb.rep = 2,
                                         # for high specificity pseudo-absences should be randomly selected 
                                         # OR they can also be chosen at a minimal and maximum distance from presence points
                                         PA.strategy = 'random') 
  }} 
# ---------------------------------

