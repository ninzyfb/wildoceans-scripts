
# filter to only keep models of choice and associated targets for each species
aseasonal_features = featurenames %>%
    filter(MODELTYPE == "Aseasonal")
summer_features = featurenames %>%
  filter(MODELTYPE %in% c("summer"))
winter_features = featurenames %>%
  filter(MODELTYPE %in% c("winter"))

# create list with targets
list_targets = list(aseasonal_features,summer_features,winter_features)
rm(aseasonal_features,summer_features,winter_features) # remove individual dfs

# filter featurestack as well
# this creates a seperate rasterstack for each problem run
keep = str_detect(names(feature_stack),"Aseasonal")  # identify Aseasonal layers in stack
idx = which(keep == FALSE) # find out layer number with feature to omit
feature_stack_aseasonal = dropLayer(feature_stack,idx)
rm(idx,keep) # remove

keep = str_detect(names(feature_stack),c("summer")) 
idx = which(keep == FALSE) # find out layer number with feature to omit
feature_stack_summer = dropLayer(feature_stack,idx)
rm(idx,keep) # remove

keep = str_detect(names(feature_stack),c("winter")) 
idx = which(keep == FALSE) # find out layer number with feature to omit
feature_stack_winter = dropLayer(feature_stack,idx)
rm(idx,keep) # remove

# combine each stack in a list
list_stacks = list(feature_stack_aseasonal,feature_stack_summer,feature_stack_winter)
rm(feature_stack_aseasonal,feature_stack_summer,feature_stack_winter)

# check that species targets and names in stack are in the same order
for(i in 1:length(list_stacks)){
check = names(list_stacks[[i]]) == list_targets[[i]][["FEATURENAME"]]
print(unique(check) == TRUE)}
rm(check,i)

# run each problems based on the three subscenarios (of increasing target %)
# each season (aseasonal, summer, winter) is run three times (low, medium, high targets)

penalty = 0 # set penalty for this sequence of problems
list_problems = list() # empty list for holding the problems
count = 1 # create a counter for list of problems

for(i in 1:length(list_stacks)){
  temp_feature = list_stacks[[i]] # for every season type (aseasonal, summer, winter)
  temp_target = list_targets[[i]] # get the appropriate targets for those layers
  temp_target = temp_target[,c(5:7)] # isolate target columns
  for(j in 1:3){ # run problem for that one stack once per target column (3 in our case)
    list_problems[[count]] = problem(pu, temp_feature) %>%
      add_min_set_objective() %>%
      add_relative_targets(temp_target[,j]) %>%
      add_boundary_penalties(penalty = penalty) %>% # add penalty
      add_binary_decisions() %>%
      add_gurobi_solver(verbose = FALSE) # gap default value is 10% from optimality
    count = count +1
  }
}

rm(i,j,count,temp_feature,temp_target) # remove


