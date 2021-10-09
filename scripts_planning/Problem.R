
# filter to only keep models of choice and associated targets for each species
aseasonal_features = featurenames %>%
    filter(modeltype == "Aseasonal")
summer_features = featurenames %>%
  filter(modeltype %in% c("summer"))
winter_features = featurenames %>%
  filter(modeltype %in% c("winter"))

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
check = names(list_stacks[[i]]) == list_targets[[i]][["featurename"]]
print(unique(check) == TRUE)}
rm(check,i)

# run each problems based on the three subscenarios (of increasing target %)
# each season (aseasonal, summer, winter) is run three times (low, medium, high targets)
p = 0 # problem number
list_problems = list()

for(i in 1:length(list_stacks)){
  p = p+1
list_problems[[p]] = problem(pu, list_stacks[[i]]) %>%
  add_min_set_objective() %>%
  add_relative_targets(list_targets[[i]]$targetsa) %>%
  add_boundary_penalties(penalty = penalty) %>% # add penalty
  add_binary_decisions() %>%
  add_gurobi_solver(verbose = FALSE) # gap default value is 10% from optimality
p = p+1
list_problems[[p]] = problem(pu, list_stacks[[i]]) %>%
  add_min_set_objective() %>%
  add_relative_targets(list_targets[[i]]$targetsb) %>%
  add_boundary_penalties(penalty = penalty) %>% # add penalty
  add_binary_decisions() %>%
  add_gurobi_solver(verbose = FALSE) # gap default value is 10% from optimality
p = p+1
list_problems[[p]] = problem(pu, list_stacks[[i]]) %>%
  add_min_set_objective() %>%
  add_relative_targets(list_targets[[i]]$targetsc) %>%
  add_boundary_penalties(penalty = penalty) %>% # add penalty
  add_binary_decisions() %>%
  add_gurobi_solver(verbose = FALSE) # gap default value is 10% from optimality
}

rm(i)
