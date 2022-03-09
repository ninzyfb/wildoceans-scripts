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
# biomod2 is loaded in separately as it interacts with functions from other packages
detach(package:dismo,unload=TRUE)
library(biomod2)
# ---------------------------------


# ---------------------------------
# FORMATTING
# ---------------------------------

# this allows you to see the default model parameters used by biomod
bm_DefaultModelingOptions() # these are the default parameters used for each model

# default parameters can be altered using BIOMOD_ModelingOptions()
# in our case we will be using MAXENT so i need to specify the path to my downloaded MAXENT file
# i also want to specify the use of a polynomial GLM
mxtPh = BIOMOD_ModelingOptions(MAXENT = list(path_to_maxent.jar = paste0(my.directory,"/maxent"),
                                             maximumiterations = 400),
                               GLM = list(type = 'polynomial'),
                               GAM = list(control = list(nlm = list(iterlim = 400))))

# remove NAs
pts_env = na.omit(pts_env)

# isolate presence/background points column of 1s and 0s
pa = pts_env$pa 

# convert 0 to NAs
# this is important as it means they ar eseen as background points and not true absences
pa[which(pa == 0)] = NA

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

# keep or remove substrate layer
if(!is.na(substrate) & substrate == "no"){
  stack_model = dropLayer(stack_subset,"substrate_simplified")
  exp$substrate_simplified = NULL}else{stack_model = stack_subset}

# reduce number of environmental variables if prevalence is less than 100 grid cells
if(length(which(pa==1))<100){
  reducedvar = read.csv(list.files(pattern = "reducedvariables.csv", recursive=TRUE, full.names = TRUE))
  stack_model = dropLayer(stack_model,reducedvar$x)
  exp = exp[,which(!(colnames(exp) %in% reducedvar$x))]
  rm(reducedvar)}


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
  if(i == 1){
  temp = pts_env_seasons[[i]]
  temp = na.omit(temp)
  pa1 = temp$pa
  pa1[which(pa1 == 0)] = NA
  pseudoabsences1 = length(which(pa==1))
  pa1xy = temp[,c(2,3)]
  exp1 = temp[,-c(1:3)]
  exp1$substrate_simplified = as.factor(exp1$substrate_simplified)
  # keep or remove substrate layer
  if(!is.na(substrate) & substrate == "no"){exp1$substrate_simplified = NULL}
  
  biomod_obj_seasons[[i]] =  BIOMOD_FormatingData(resp.var = pa1, # presence/absence data
                                         expl.var = exp1, # environmental variables
                                         resp.xy = pa1xy,
                                         resp.name = target, # species name
                                         # random background cells at 20% area of EEZ (5km2 res): 8500
                                         PA.nb.absences = pseudoabsences1,
                                         PA.nb.rep = 2,
                                         # for high specificity pseudo-absences should be randomly selected 
                                         # OR they can also be chosen at a minimal and maximum distance from presence points
                                         PA.strategy = 'random')} 
  if(i == 2){
    temp = pts_env_seasons[[i]]
    temp = na.omit(temp)
    pa2 = temp$pa
    pa2[which(pa2 == 0)] = NA
    pseudoabsences2 = length(which(pa==1))
    pa2xy = temp[,c(2,3)]
    exp2 = temp[,-c(1:3)]
    exp2$substrate_simplified = as.factor(exp2$substrate_simplified)
    if(!is.na(substrate) & substrate == "no"){exp2$substrate_simplified = NULL}
    biomod_obj_seasons[[i]] =  BIOMOD_FormatingData(resp.var = pa2, # presence/absence data
                                                    expl.var = exp2, # environmental variables
                                                    resp.xy = pa2xy,
                                                    resp.name = target, # species name
                                                    # random background cells at 20% area of EEZ (5km2 res): 8500
                                                    PA.nb.absences = pseudoabsences2,
                                                    PA.nb.rep = 2,
                                                    # for high specificity pseudo-absences should be randomly selected 
                                                    # OR they can also be chosen at a minimal and maximum distance from presence points
                                                    PA.strategy = 'random')}}} 
# ---------------------------------