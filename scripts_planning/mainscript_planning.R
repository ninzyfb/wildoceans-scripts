# ---------------------------------------------------------------------------------
# AUTHOR: Nina Faure Beaulieu (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script is the parent spatial planning script, it calls all sub-scripts
# IMPORTANT: Run each subscript one at a time as running the whole parent script at once seems to cause some issues
# ---------------------------------


# ---------------------------------
# PACKAGES
# ---------------------------------
# list of required packages
requiredpackages = c("rgeos","sf","dplyr","tidyr","prioritizr","gurobi","stringr","rasterVis","viridis","raster","scales","readxl","fasterize","sdmvspecies","RColorBrewer")
# load packages
lapply(requiredpackages,require, character.only = TRUE)
rm(requiredpackages)
# ---------------------------------


# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------
# set directory to same parent folder where sub-scripts are found
# the subs-scripts can be in folders within this directory as the code will look through all the folders
path =  "/Users/nfb/" # path for mac
my.directory = paste0(path,"Dropbox/6-WILDOCEANS")
# set directory
setwd(my.directory) 
# ---------------------------------


# ---------------------------------
# PLANNING UNITS
# ---------------------------------
# Load the planning unit grid at 10 x 10 km or 5 x 5 km resolution
# Each grid cell has a value of 1 which represents the cost of that grid cell
pu = raster(list.files(pattern = "template_10km.tif",full.names = TRUE,recursive = TRUE))
# ---------------------------------


# ---------------------------------
# CONSERVATION FEATURES
# ---------------------------------
source(list.files(pattern = "Conservationfeatures.R", recursive = TRUE)) 
# ---------------------------------


# ---------------------------------
# COSTS
# ---------------------------------
source(list.files(pattern = "costs_2018NBA.R", recursive = TRUE))
# ---------------------------------


# ---------------------------------
# LOCKED IN AREAS
# ---------------------------------
source(list.files(pattern = "Lockedin.R", recursive = TRUE))
# ---------------------------------


# ---------------------------------
# CONSERVATION TARGETS
# ---------------------------------
# load in spreadsheet with species and their targets
targets = read_xlsx(list.files(pattern = "species_targets.xlsx",recursive = TRUE), sheet = 1)
# only keep species names and targets
targets = targets %>%
  dplyr::select(SPECIES_SCIENTIFIC,Score)
# turn colnames to upper case
colnames(targets) = toupper(colnames(targets))
# join these targets to featurenames dataframe
colnames(featurenames) = toupper(colnames(featurenames))
featurenames = left_join(featurenames,targets)
rm(targets) # remove

# load data summary sheet
master = read_xlsx(path=paste0(path,"Dropbox/6-WILDOCEANS/data_summary_master.xlsx"),sheet = 1)
master  = master %>%
  dplyr::select(SPECIES_SCIENTIFIC,target_species)
featurenames = left_join(featurenames,master)

# weights, give higher weighting to target species
# i.e. species with a high endemism, high threat and restricted range
featurenames = featurenames %>%
  mutate(SCORE = ifelse(target_species == "yes",SCORE+1,SCORE))
# ---------------------------------


# ---------------------------------
# BUILDING AND SOLVING A CONSERVATION PROBLEM
# ---------------------------------
# turn off scientific numbering
options(scipen = 100) 

# scenarios
scenario_sheet = read_xlsx(path=paste0(path,"Dropbox/6-WILDOCEANS/Planning/scenarios.xlsx"),sheet = 2)

# first decide on which scenario you are running (row number of spreadsheet)
for(i in 1:9){
  n = i # problem number
  scenario = scenario_sheet$scenario[n] # scenario name
  season = scenario_sheet$season[n] # season
  features = scenario_sheet$features[n] # features to use
  format = scenario_sheet$format[n] # format of features i.e. continuous or seasonal
  locked_in = scenario_sheet$lockedin[n] # sets areas to be locked-in
  costs = scenario_sheet$costs[n] # sets costs
  target = scenario_sheet$targets[n] # sets targets i.e. uniform or tailored
  tailoredtargets = scenario_sheet$tailored_targets[n] # set level of tailored targets
  boundary_penalty = as.numeric(scenario_sheet$boundary_penalty[n]) # sets boundary penalties
  objective = scenario_sheet$objective_budget[n]
  feat = get(features) # features
  
  # weights
  weights = scenario_sheet$weights[n]
  if(weights == "yes"){w = featurenames$SCORE}
  
  # targets
  if(target == "tailored"){
    t = featurenames # dataframe with tailored targets
    # further refine by season
    if(season == "aseasonal"){t = t %>%filter(MODELTYPE == "Aseasonal")
      # specifies which targets to pick
    if(tailoredtargets == "low"){t = t$low}
    if(tailoredtargets == "medium"){t = t$medium}
    if(tailoredtargets == "high"){t = t$high}}
    # if target is numeric just take that value
  }else{t = as.numeric(target)}
  
  # build your basic problem
  problem_base= problem(pu,feat)%>%
    add_gap_portfolio(number_solutions=5)
  
  # additional parameters to add to your problem based on specific scenario
  if(weights == "yes"){
    problem_base = problem_base %>%
      add_feature_weights(w)}
  if(locked_in != "none"){
    problem_base = problem_base %>%
      add_locked_in_constraints(lockedin[[locked_in]])}
  if(costs == "fp_threshold"){
    problem_base = problem_base %>%
    add_linear_constraints(threshold = fp_threshold, sense = "<=", data = costs_all)}
  if(costs == "fp_binary"){
    problem_base = problem_base %>%
    add_locked_out_constraints(fp_binary)}
  
  # run conservation problem with selected solving objective
  if(objective == "add_max_features_objective"){ 
    problem_single = problem_base %>% # costs and features
      add_max_features_objective(budget = 1080)%>%
      add_relative_targets(t) %>%
      add_proportion_decisions() %>%
      add_gurobi_solver(gap=0.1)}
  
  if(objective == "add_min_set_objective"){ 
    problem_single = problem_base %>% # costs and features
      add_min_set_objective()%>%
      add_relative_targets(t) %>%
      add_proportion_decisions() %>%
      add_gurobi_solver(gap=0.1)}
  
  if(objective == "add_min_largest_shortfall_objective"){ 
    problem_single = problem_base %>% # costs and features
      add_min_largest_shortfall_objective(budget = 1080)%>%
      add_relative_targets(t) %>%
      add_proportion_decisions() %>%
      add_gurobi_solver(gap=0.1)}
  
  if(objective == "add_min_shortfall_objective"){ 
    problem_single = problem_base %>% # costs and features
      add_min_shortfall_objective(budget = 1080)%>%
      add_relative_targets(t) %>%
      add_binary_decisions() %>%
      add_gurobi_solver(gap=0.1)}
  
  # solve problem
  solution_single = solve(problem_single)

  # create solution frequency raster
  solution_sum= calc(stack(unlist(solution_single)),sum)
  
  # save solution as raster
  writeRaster(solution_sum,paste0("Planning/Outputs/solutions/rasters_rawsolutions/","p",str_pad(n,3,pad = "0"),"_",scenario,"scenario.tiff"),overwrite = TRUE)
  
  # evaluate performance of solution
  performances = data.frame()
  performances$prop_eez = NULL
  for(i in 1:length(solution_single)){
  # number of planning units selected within a solution.
  pus = eval_n_summary(problem_single, solution_single[[i]])
  performances[i,1] = pus[1,2]
  # calculate % of EEZ represented (there are 42053 cells in the EEZ)
  performances$prop_eez[i] = (performances$cost/10809)*100}
  
  # get an average proportion of the EEZ to add onto plots
  prop_eez = round(mean(performances$prop_eez), 1)
  
  # create coverage summary
  coverage_summary = data.frame()
  for(i in 1:length(solution_single)){
  temp = eval_target_coverage_summary(problem_single,solution_single[[i]])
  temp = cbind(temp,featurenames)
  temp = temp %>%
    filter(met == FALSE)
  temp$absolute_shortfall = round(temp$absolute_shortfall, 2)
  temp$relative_held = round(temp$relative_held, 2)
  temp$relative_shortfall = round(temp$relative_shortfall, 2)
  temp$km_shortfall = temp$absolute_shortfall*10
  if(nrow(temp)>0){
    temp$solution = i
    temp$feature = NULL
    temp$FEATURENAME_BINARY = NULL
    temp$FEATURENAME = NULL
    temp$numbershortfallspp = nrow(temp)}
  # save coverage summary
  coverage_summary = rbind(coverage_summary,temp)}
  
  if(nrow(coverage_summary)>0){
  # get some numbers that are easier to interpret
  coverage_summary = coverage_summary %>%
    group_by(SPECIES_SCIENTIFIC,target_species,SCORE) %>%
    summarise(km_shortfall_avg = mean(km_shortfall),
              km_shortfall_sd = sd(km_shortfall),
              target = mean(relative_target),
              target_achieved = mean(relative_held),
              number_of_solutions = n_distinct(solution))
  coverage_summary$target = as.numeric(paste0(round(coverage_summary$target , 3), "0"))
  coverage_summary = coverage_summary %>%
    filter(target != target_achieved)
  write.csv(coverage_summary,paste0(path,"Dropbox/6-WILDOCEANS/Planning/Outputs/performances/","p",str_pad(n,3,pad = "0"),"_performance.csv"), row.names = FALSE)}
  }
  
  # ferrier score for single problem
  ferrierscore_sum = stack()
  for(i in 1:length(solution_single)){
  ferrierscore_single = eval_ferrier_importance(problem_single, solution_single[[i]])[["total"]]
  ferrierscore_single = rescale(ferrierscore_single)
  ferrierscore_sum = addLayer(ferrierscore_sum,ferrierscore_single)}
  # calculate sum
  ferrierscore_sum= calc(ferrierscore_sum,sum)
  
  # save as raw raster file
  writeRaster(ferrierscore_sum,paste0("Planning/Outputs/solutions/rasters_rawsolutions/","p",str_pad(n,3,pad = "0"),"_",scenario,"scenario_FS.tiff"), overwrite = TRUE)
  
  # leave solution_single in environment to facilitate plotting tests
  rm(solution_single,solution_sum,ferrierscore_single,ferrierscore_sum,f,c,t,boundary_penalty,season,scenario,target,locked_in,costs,features,format,problem_single,performances,pus,w,weights,tailoredweights,tailoredtargets,n,objective,temp,feat,cost)
}
