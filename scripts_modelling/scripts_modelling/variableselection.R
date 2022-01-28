# ---------------------------------------------------------------------------------
# AUTHOR: Nina Faure Beaulieu (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script identifies collinearity between predictor variables and only keeps a subset
# ---------------------------------


# ---------------------------------
# FORMATTING
# ---------------------------------
# tidyr package seems to mess with other packages so best to only load when needed
library(tidyr) 
# create temporary duplicate of presence absence data without NAs
pts_env_temporary = pts_env %>% 
  drop_na()
# detach tidyr package 
detach(package:tidyr,unload=TRUE)

# remove substrate which is categorical variable 
# categorical variables cannot be measured against continuous variables for colinearity
pts_env_temporary$substrate_simplified = NULL

# Variable selection function from mecofun package
# function selects weakly correlated variables based on univariate importance
# univariate variable importance is based on AIC
# based on (Dormann et al. 2013)
# see ?select07() for further descriptions of how it works
variable_selection = select07(
  # predictor variables (environmental variables)
  x = pts_env_temporary[,-c(1:3)], 
  # response variable (1 for presence, 0 for background)
  y = pts_env_temporary$pa,
  # any correlations above this threshold are judged problematic
  threshold=0.7)

rm(pts_env_temporary) 

# filter presence/backgrounds points to only keep chosen variables
# keep is the pa,lat,long and substrate variable
keep = pts_env[,c(1:3,length(pts_env))] # substrate simplified is always the last variable due to alphabetical sorting
pts_env = pts_env[,names(pts_env) %in% variable_selection$pred_sel] # filter df to only keep chosen variables
pts_env = cbind(keep,pts_env)
pts_env$substrate_simplified = as.factor(pts_env$substrate_simplified)
rm(keep)

# filter stack to only keep chosen variables
stack_subset = subset(stack,names(pts_env)[-c(1:3)])
stack_subset$substrate_simplified = as.factor(stack_subset$substrate_simplified )

# run same code for seasonal data
if(exists("pts_env_seasons")){
# filter dfs to only keep chosen variables (seasonal)
for(i in 1:length(pts_env_seasons)){
  temp =  pts_env_seasons[[i]] # extract df
  keep = temp[,c(1:3,length(temp))] # keep first 3 columns + substrate column
  temp = temp[,names(temp) %in% variable_selection$pred_sel] # filter df to only keep chosen variables
  temp = cbind(keep,temp)
  temp$substrate_simplified = as.factor(temp$substrate_simplified)
  pts_env_seasons[[i]] = temp
  rm(temp,keep)} # remove unnecessary variables
rm(i)} # remove unnecessary variables

# remove substrate layer if necessary
if(substrate == "no"){
  pts_env$substrate_simplified = NULL
  stack_subset = dropLayer(stack_subset,1)
  if(exists("pts_env_seasons")){
  pts_env_seasons[[1]]$substrate_simplified = NULL
  pts_env_seasons[[2]]$substrate_simplified = NULL}}
rm(substrate) 
# ---------------------------------


# ---------------------------------
# SAVING RESULTS
# ---------------------------------
# save chosen variables to a folder 
# file name will specify resolution and species name
write.csv(variable_selection$pred_sel,paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs/selectedvariables/",target,"_","res",res,"_variables.csv"))
rm(variable_selection)
# ---------------------------------

