# ---------------------------------------------------------------------------------
######### Shark and ray species distribution models (SDMs) - variable selection script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: identifies collinearity between predictor variables and only keeps a subset

# ---------------------------------
# FORMATTING
# ---------------------------------
# Run select07() function
library(tidyr) # this package seems to mess with other packages so best to only load when needed
pts_env_temporary = pts_env %>% # create temporary duplicate of presence absence data without NAs
  drop_na()
# detach tidyre package 
detach(package:tidyr,unload=TRUE)

# remove substrate which is categorical variable and doesn't work with function
pts_env_temporary$substrate_simplified = NULL

# run variable selection function
variable_selection = select07(X=pts_env_temporary[,-c(1:3)], 
                   y=pts_env_temporary$pa, 
                   threshold=0.7)
rm(pts_env_temporary) # remove unnecessary variable

# filter df to only keep chosen variables (static)
# keep is the pa,lat,long and substrate variable
keep = pts_env[,c(1:3,length(pts_env))] # substrate simplified is always the last variable due to alphabetical sorting
pts_env = pts_env[,names(pts_env) %in% variable_selection$pred_sel] # filter df to only keep chosen variables
pts_env = cbind(keep,pts_env)
pts_env$substrate_simplified = as.factor(pts_env$substrate_simplified)
rm(keep) # remove unnecessary variables

# filter stack to only keep chosen variables
stack_subset = subset(stack,names(pts_env))
stack_subset$substrate_simplified = as.factor(stack_subset$substrate_simplified )
rm(stack) # remove unnecessary variables

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
  if(exists("pts_env_seasons")){
  pts_env_seasons[[1]]$substrate_simplified = NULL
  pts_env_seasons[[2]]$substrate_simplified = NULL}
  names(stack_subset)
  stack_subset = dropLayer(stack_subset,1)}
rm(substrate) # no longer needed

# ---------------------------------
# WRITING DATA
# ---------------------------------

# save chosen variables for species in question 
write.csv(variable_selection$pred_sel,paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs/selectedvariables/",target,"_variables.csv"))
rm(variable_selection) # remove unnecessary variables
