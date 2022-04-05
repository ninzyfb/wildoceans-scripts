# ---------------------------------------------------------------------------------
# AUTHORS: Nina Faure Beaulieu, Dr. Victoria Goodall (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST
# CONTACTs: ninab@wildtrust.co.za; victoria.goodall@mandela.ac.za 
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
# check which packages you need to install
requiredpackages = requiredpackages[which(!(requiredpackages %in% installed.packages()))]
# install packages
install.packages(requiredpackages)
# load packages
requiredpackages = c("rgeos","sf","dplyr","tidyr","prioritizr","gurobi","stringr","rasterVis","viridis","raster","scales","readxl","fasterize","sdmvspecies","RColorBrewer")
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
# SPECIES INFO
# ---------------------------------
# load data summary sheet
master = read_xlsx(list.files(pattern = "data_summary_master.xlsx", recursive = TRUE,full.names = TRUE),sheet = 1)
# ---------------------------------


# ---------------------------------
# CONSERVATION FEATURES
# ---------------------------------
source(list.files(pattern = "Conservationfeatures.R", recursive = TRUE)) 
# ---------------------------------


# ---------------------------------
# IUCN FEATURES
# ---------------------------------
source(list.files(pattern = "iucnmaps.R", recursive = TRUE)) 
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
# TARGETS
# ---------------------------------
targets = read_xlsx(list.files(pattern = "perc_targets", recursive = TRUE,full.names = TRUE))
targets = targets %>%
  pivot_longer(!STATUS,names_to = "ENDEMIC.STATUS",values_to = "target")

featurenames = left_join(featurenames,targets)

# extract target species
# that means endangered, critical and endemics (SA)
special_species_1 = featurenames %>%
  filter(ENDEMIC.STATUS %in% c("1") | STATUS %in% c("CR","EN"))
idx = which(names(feature_stack_aseasonal_thresholds) %in% special_species_1$FEATURENAME)
feature_stack_specialspp1 = subset(feature_stack_aseasonal_thresholds,idx)

# that means endangered, critical, vulnerable and endemics (SA and Southern Africa)
special_species_2 = featurenames %>%
  filter(ENDEMIC.STATUS %in% c("1","2") | STATUS %in% c("CR","EN","VU"))
idx = which(names(feature_stack_aseasonal_thresholds) %in% special_species_2$FEATURENAME)
feature_stack_specialspp2 = subset(feature_stack_aseasonal_thresholds,idx)
# ---------------------------------


# ---------------------------------
# BUILDING AND SOLVING A CONSERVATION PROBLEM
# ---------------------------------

# parent folder to save all solution outputs
solutionsfolder = "Planning/Outputs/solutions/rasters_rawsolutions/"

# parent folder to save all performance outputs
performancefolder = "Planning/Outputs/performances/"

# turn off scientific numbering
options(scipen = 100) 

# scenarios
scenario_sheet = read_xlsx(path=paste0(path,"Dropbox/6-WILDOCEANS/Planning/scenarios.xlsx"),sheet = 1)

# start counter
problem_number = 4

# Building and solving conservation problems
# these are all outlined in the scenario sheet
# the following loop goes through each row of the scenario sheet and outputs a solution
for(i in 5:nrow(scenario_sheet)){
  
  # scenario name (Control, MPA, Fishing)
  scenario = scenario_sheet$scenario[i]
  
  # features
  features = get(scenario_sheet$features[i])
  
  # season
  season = toupper(scenario_sheet$season[i])
  featurenames_temp = featurenames[featurenames$FEATURENAME %in% names(features),]
  
  # weights
  weights = scenario_sheet$weights[i]
  if(weights == "yes"){w = featurenames_temp$SCORE}
  
  # budget
  objective = scenario_sheet$objective_budget[i]
  
  # areas to be locked-in
  locked_in = scenario_sheet$lockedin[i] 
  
  # costs
  costs = scenario_sheet$costs[i]
  
  # boundary penalty
  boundary_penalty = as.numeric(scenario_sheet$boundary_penalty[i]) 
  
  # target
  if(scenario_sheet$targets[i] == "t"){t = featurenames_temp$target}else{t = as.numeric(scenario_sheet$targets[i])}

    # problem number
    problem_number=problem_number+1
    
    # BASIC CONSERVATION PROBLEM
    problem_single= problem(pu,features)%>%
      # protection targets (will apply to each feature)
      add_relative_targets(t) %>%
      # budget representing 10% of EEZ
      add_min_set_objective()%>%
      # solutions needs to be within 10% of optimality
      add_gurobi_solver(gap=0.1) %>%
      # generate 10 solutions per problem
      add_gap_portfolio(number_solutions=10, pool_gap = 0.1)%>%
      # binary decisions
      add_binary_decisions()
    
    # ADDITIONAL PARAMETERS
    # these will increase the complexity of the conservation problem
    
    # weights
    if(weights == "yes"){problem_single = problem_single %>% add_feature_weights(w)}
  
    # locked in areas
    if(locked_in != "none"){problem_single = problem_single %>% add_locked_in_constraints(lockedin[[locked_in]])}
  
    # costs (fishing - threshold)
    if(costs == "fp_threshold"){problem_single = problem_single %>% add_linear_constraints(threshold = fp_threshold, sense = "<=", data = costs_all)}
  
    # costs (fishing - binary)
    if(costs == "fp_binary"){problem_single = problem_single %>% add_locked_out_constraints(fp_binary)}
  
    # solve problem
    solution_single = solve(problem_single)
    
    # create raster stack from solutions
    solution_single = stack(unlist(solution_single))
  
    # create solution frequency raster
    # this sums all the solutions together
    # it outlines the most frequently chosen areas for the given conservation problem
    solution_sum= calc(solution_single,sum)
  
    # save solution as raster
    writeRaster(solution_sum,paste0(solutionsfolder,"p",str_pad(problem_number,3,pad = "0"),"_",scenario,"_scenario.tiff"),overwrite = TRUE)
  
  # create coverage summary
  coverage_summary = data.frame()
  for(a in 1:nlayers(solution_single)){
  temp = eval_target_coverage_summary(problem_single,solution_single[[a]])
  temp = cbind(temp,featurenames_temp)
  temp$absolute_shortfall = round(temp$absolute_shortfall, 2)
  temp$relative_held = round(temp$relative_held, 2)
  temp$relative_shortfall = round(temp$relative_shortfall, 2)
  temp$km_shortfall = temp$absolute_shortfall*10
  if(nrow(temp)>0){
    temp$solution = a
    temp$feature = NULL
    temp$FEATURENAME_BINARY = NULL
    temp$FEATURENAME = NULL}
   #save coverage summary
  coverage_summary = rbind(coverage_summary,temp)}
  
  if(nrow(coverage_summary)>0){
  # get some numbers that are easier to interpret
  coverage_summary = coverage_summary %>%
    group_by(SPECIES_SCIENTIFIC,STATUS) %>%
    summarise(total_amount = mean(total_amount),
              absolute_held_avg = mean(absolute_held),
              absolute_target_avg = mean(absolute_target),
              km_shortfall_avg = mean(km_shortfall),
              km_shortfall_sd = sd(km_shortfall),
              target = mean(relative_target),
              target_achieved = mean(relative_held))%>%
    arrange(STATUS)
  coverage_summary$target = as.numeric(paste0(round(coverage_summary$target , 3), "0"))
  write.csv(coverage_summary,paste0(performancefolder,"p",str_pad(problem_number,3,pad = "0"),scenario,"_scenario_performance.csv"), row.names = FALSE)}
  
  # irraplaceability score for single problem
  ir <- eval_replacement_importance(problem_single, solution_single[[1]])
  # save as raw raster file
  writeRaster(ir,paste0(solutionsfolder,"p",str_pad(problem_number,3,pad = "0"),"_",scenario,"_scenario_IR.tiff"), overwrite = TRUE)
  
  rm(problem_single,solution_single,solution_sum,ferrierscore_single,ferrierscore_sum,t)
  rm(boundary_penalty,scenario,locked_in,costs,features,performances,objective,temp)
}
