# ---------------------------------------------------------------------------------
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST 
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
requiredpackages = c("dplyr","tidyr","prioritizr","gurobi","stringr","rasterVis","viridis","raster","scales","readxl","fasterize","sdmvspecies","RColorBrewer")
# load packages
lapply(requiredpackages,require, character.only = TRUE)
rm(requiredpackages)
# ---------------------------------


# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------
# set directory to same parent folder where sub-scripts are found
# the subs-scripts can be in folders within this directory as the code will look through all the folders
path = "/home/nina/" # path for linux
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
writeRaster(feature_stack_aseasonal,"feature_stack_aseasonal.grd", format="raster")
writeRaster(feature_stack_binary,"feature_stack_binary.grd", format="raster")
rm(feature_stack,feature_stack_aseasonal,feature_stack_binary)
feature_stack_aseasonal = stack(list.files(pattern = "feature_stack_aseasonal"))
feature_stack_binary = stack(list.files(pattern = "feature_stack_binary"))
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
# PLOTTING PARAMETERS
# ---------------------------------
source(list.files(pattern = "plottingparameters.R", recursive = TRUE, full.names= TRUE))
# ---------------------------------


# ---------------------------------
# CONSERVATION PROBLEM & SOLVING
# ---------------------------------
# turn off scientific numbering
options(scipen = 100) 

# scenarios
scenario_sheet = read_xlsx(path=paste0(path,"Dropbox/6-WILDOCEANS/Planning/scenarios.xlsx"),sheet = 2)

# colours for plots
cols <- colorRampPalette(c("white","darkgreen"))
cols2 <- colorRampPalette(c("yellow"))

# first decide on which scenario you are running (row number of spreadsheet)
for(i in 46:64){
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
  # features and costs
  feat = get(features)
  cost = get(costs)
# weights
  weights = scenario_sheet$weights[n]
  if(weights == "yes"){w = featurenames$SCORE}
  
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
  
  # base problem
  problem_base= problem(cost,feat)%>%
    add_gap_portfolio(number_solutions=5)
  if(weights == "yes"){
    problem_base = problem_base %>%
      add_feature_weights(w)}
  if(locked_in != "none"){
    problem_base = problem_base %>%
      add_locked_in_constraints(lockedin[[locked_in]])
  }
    # run conservation problem
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
      add_linear_constraints(threshold = 1080,sense = "<=",data = pu)%>%
      add_relative_targets(t) %>%
      add_binary_decisions() %>%
      add_gurobi_solver(gap=0.1)
}
  
  # solve single solution
  #source(list.files(pattern = "Solution", recursive = TRUE)) 
  solution_single = solve(problem_single)

  # create solution frequency raster
  solution_sum= calc(stack(unlist(solution_single)),sum)
  
  # evaluate performance
  performances = data.frame()
  performances$prop_eez = NULL
  for(i in 1:length(solution_single)){
  # number of planning units selected within a solution.
  pus = eval_n_summary(problem_single, solution_single[[i]])
  performances[i,1] = pus[1,2]
  # calculate % of EEZ represented (there are 42053 cells in the EEZ)
  performances$prop_eez[i] = (performances$cost/10809)*100}
  
  # also save dataframe evaluating if targets are met
  coverage_summary = data.frame()
  for(i in 1:length(solution_single)){
  temp = eval_target_coverage_summary(problem_single,solution_single[[i]])
  temp = temp %>%
    filter(met == FALSE)
  temp = temp[,c(1,2,6,7,8,9)]
  temp$absolute_shortfall = round(temp$absolute_shortfall, 2)
  temp$relative_held = round(temp$relative_held, 2)
  temp$relative_shortfall = round(temp$relative_shortfall, 2)
  temp$km_shortfall = temp$absolute_shortfall*10
  temp$solution = i
  temp$numbershortfallspp = nrow(temp)
  coverage_summary = rbind(coverage_summary,temp)}
  write.csv(coverage_summary,paste0(path,"Dropbox/6-WILDOCEANS/Planning/Outputs/performances/","p",str_pad(n,3,pad = "0"),"_performance.csv"), row.names = FALSE)
  
  # save as raw raster file
  writeRaster(solution_sum,paste0("Planning/Outputs/solutions/rasters_rawsolutions/","p",str_pad(n,3,pad = "0"),"_",scenario,"scenario.tiff"),overwrite = TRUE)
  
  # plot single solution
  plot=levelplot(solution_sum,
            main = paste(scenario,"scenario","\nTargets:",target,"| Target category:",tailoredtargets,"\nPenalty:",boundary_penalty),
            sub = paste(objective),
            col.regions = cols,
            margin = FALSE,
            colorkey=FALSE)+
    # mpa filled no take only
    #levelplot(mpas,col.regions = cols2, alpha.regions=0.6)+
    # mpa outline
    latticeExtra::layer(sp.polygons(mpas,col = "black",lwd = 1))+
    # eez
    latticeExtra::layer(sp.polygons(eez,col = "black",lwd = 1))+
    # sa coast
    latticeExtra::layer(sp.polygons(sa_coast,col = "black",lwd= 1, fill = "white"))+
    # points for main cities
    latticeExtra::layer(sp.points(places[c(1:3,5,6,18,20:22,10,14),],col = "black",pch = 20))+
    # coordinates and city names
    # done in three lines as a "pretty" position varies based on their place on the map
    latticeExtra::layer(sp.text(coordinates(places)[c(1:3,5,6),],places$Location[c(1:3,5,6)],col = "black",pch = 20,pos=4,cex = 0.5))+
    latticeExtra::layer(sp.text(coordinates(places)[c(18,20,21,22),],places$Location[c(18,20,21,22)],col = "black",pch = 20,pos=2,cex = 0.5))+
    latticeExtra::layer(sp.text(adjustedcoords,places$Location[c(10,14)],col = "black",pch = 20, pos=2,cex = 0.5))
  png(file=paste0("Planning/Outputs/solutions/national/","p",str_pad(n,3,pad = "0"),"_",scenario,"scenario.png"),width=3000, height=2000, res=300)
  print(plot)
  dev.off()
  
  # plot single solution per ebert range
  for(j in 1:length(range)){
    # subset range
    subset = regions[regions$Region%in%range[j],]
    cropped = crop(solution_sum,subset)
    plot = levelplot(cropped, 
              main = paste(scenario,"scenario","\nTargets:",target,"| Target category:",tailoredtargets,"\nPenalty:",boundary_penalty),
              sub = paste("Objective:",objective,"Features:",season,format,"\nPercentage of EEZ = ",round(performances$prop_eez,0),"%"),
              margin = FALSE,
              colorkey=FALSE,
              col.regions = cols)+
      # mpa filled no take only
      #levelplot(mpa_layer,col.regions = cols2, alpha.regions=0.6)+
      # mpa outline
      latticeExtra::layer(sp.polygons(mpas,col = "black",lwd = 1))+
      # eez
      latticeExtra::layer(sp.polygons(eez,col = "black",lwd = 1))+
      # sa coast
      latticeExtra::layer(sp.polygons(sa_coast,col = "black",lwd= 1, fill = "white"))+
      # points for main cities
      latticeExtra::layer(sp.points(places[c(1:3,5,6,18,20:22,10,14),],col = "black",pch = 20))+
      # coordinates and city names
      # done in three lines as a "pretty" position varies based on their place on the map
      latticeExtra::layer(sp.text(coordinates(places)[c(1:3,5,6),],places$Location[c(1:3,5,6)],col = "black",pch = 20,pos=4,cex = 0.5))+
      latticeExtra::layer(sp.text(coordinates(places)[c(18,20,21,22),],places$Location[c(18,20,21,22)],col = "black",pch = 20,pos=2,cex = 0.5))+
      latticeExtra::layer(sp.text(adjustedcoords,places$Location[c(10,14)],col = "black",pch = 20, pos=2,cex = 0.5))
    png(file=paste0("Planning/Outputs/solutions/regional/","p",str_pad(n,3,pad = "0"),"_",range[j],"_",scenario,"scenario.png"),width=3000, height=2000, res=300)
    print(plot)
    dev.off()
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
  
  # plot ferrier score plot
  plot = levelplot(ferrierscore_sum, 
                   main = paste(scenario,"scenario","\nTargets:",target,"| Target category:",tailoredtargets,"\nPenalty:",boundary_penalty),
                   sub = paste("Objective",objective,"Features:",season,format,"\nPercentage of EEZ = ",round(performances$prop_eez,0),"%"),
                   margin = FALSE,
                   colorkey=FALSE,
                   col.regions = cols)+
    # mpa filled no take only
    #levelplot(mpa_layer,col.regions = cols2, alpha.regions=0.6)+
    # mpa outline
    latticeExtra::layer(sp.polygons(mpas,col = "black",lwd = 1))+
    # eez
    latticeExtra::layer(sp.polygons(eez,col = "black",lwd = 1))+
    # sa coast
    latticeExtra::layer(sp.polygons(sa_coast,col = "black",lwd= 1, fill = "white"))+
    # points for main cities
    latticeExtra::layer(sp.points(places[c(1:3,5,6,18,20:22,10,14),],col = "black",pch = 20))+
    # coordinates and city names
    # done in three lines as a "pretty" position varies based on their place on the map
    latticeExtra::layer(sp.text(coordinates(places)[c(1:3,5,6),],places$Location[c(1:3,5,6)],col = "black",pch = 20,pos=4,cex = 0.5))+
    latticeExtra::layer(sp.text(coordinates(places)[c(18,20,21,22),],places$Location[c(18,20,21,22)],col = "black",pch = 20,pos=2,cex = 0.5))+
    latticeExtra::layer(sp.text(adjustedcoords,places$Location[c(10,14)],col = "black",pch = 20, pos=2,cex = 0.5))
  png(file=paste0("Planning/Outputs/solutions/ferrierscores/","p",str_pad(n,3,pad = "0"),"_",scenario,"scenario","_ferrierscore.png"),width=3000, height=2000, res=300)
  print(plot)
  dev.off()
  # leave solution_single in environment to facilitate plotting tests
  rm(solution_single,solution_sum,ferrierscore_single,ferrierscore_sum,f,c,t,boundary_penalty,season,scenario,target,locked_in,costs,features,format,problem_single,performances,pus,w,weights,tailoredweights,tailoredtargets,n,objective,temp,feat,cost)
}


# END OF SCRIPT
