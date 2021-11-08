# ---------------------------------------------------------------------------------
######### Shark and ray species conservation planning using prioritizr - Parent script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
#!Run each script one at a time as running the whole at once seems to cause some bugs
#the output line describes what each script produces
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: This is the parent script which calls all of the other scripts
####

# ---------------------------------
# PACKAGES
# ---------------------------------
library(dplyr)
library(tidyr)
library(prioritizr)
library(gurobi)
library(stringr)
library(rasterVis)
library(viridis)
library(rgeos)
library(raster)
library(scales)
library(readxl)
library(fasterize)
library(sdmvspecies)

# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------

# all the subscripts are in the scripts folder
# all the data is in the planning folder
# make sure you choose the coorect patg and set the directory to the overall WILDOCEANS folder so you can find both
#path = "/home/nina/Documents/" # linux path
path = "/Users/nfb/" # mac path
setwd(paste0(path,"Dropbox/6-WILDOCEANS"))

# ---------------------------------
# 1 - DEFINE PLANNING SCENARIO 
# these parameters define what happens in the subscripts
# these parameters need to be entered manually depending on which species you want to model
# ---------------------------------

# load scenario sheet
#scenario_sheet = read_xlsx(path=paste0(path,"Dropbox/6-WILDOCEANS/Planning/scenarios.xlsx"),sheet = 1)

# load data summary sheet
#master_sheet = read_xlsx(path=paste0(path,"Dropbox/6-WILDOCEANS/data_summary_master.xlsx"),sheet = 1)

# filter to only keep modeled species
#master_sheet = master_sheet %>%
#  filter(Round3_static == "yes")

# ---------------------------------
# 2 - PLANNING UNIT (pu)
# output: planning unit grid (EEZ with uniform value of 1)
# ---------------------------------
# grid cell value = cost of grid cell
# this is the basic planning unit, every 5km2 grid cell has a value of 1
pu = raster(list.files(pattern = "template.tif",full.names = TRUE,recursive = TRUE))

# ---------------------------------
# 3 - CONSERVATION FEATURES
# output: 
# ---------------------------------
source(list.files(pattern = "Conservationfeatures.R", recursive = TRUE)) 

# ---------------------------------
# 4 - COSTS
# output: 
# ---------------------------------
source(list.files(pattern = "costs_2018NBA.R", recursive = TRUE))

# ---------------------------------
# 5 - LOCKED IN AREAS
# output: 
# ---------------------------------

# at the moment i am only locking in mpas (no take ones)
mpa_layer = raster(list.files(pattern = "mpa_lockedin.tif",recursive=TRUE,full.names = TRUE))
#source(list.files(pattern = "Lockedin.R", recursive = TRUE))

# ---------------------------------
# 6 - CONSERVATION TARGETS
# output: this adds targets to featurenames dataframe
# ---------------------------------
source(list.files(pattern = "Speciestargets.R", recursive = TRUE)) 

# ---------------------------------
# 7 - PLOTTING PARAMETERS
# ---------------------------------
# load mpas for solution plotting
mpas = shapefile(list.files(pattern ="SAMPAZ_OR_2020_Q3.shp" ,recursive = TRUE, full.names = TRUE))
mpas = gSimplify(mpas,tol = 0.01)
# three marine regions as defined by Ebert et al.
regions = shapefile(list.files(pattern = "ebert_regions.shp", recursive = TRUE,full.names = TRUE))
# turn region names to upper case
regions$Region = toupper(regions$Region)
# subset by east south and west
range = c("WEST","SOUTH","EAST")

# ---------------------------------
# 8 - CONSERVATION PROBLEM & SOLVING
# output: prioritizr problem object
# ---------------------------------
# running from the mainscript for now 
# seems more practical as i understand more about the package
#source(list.files(pattern = "Problem.R", recursive = TRUE))

# load in scenarios
scenario_sheet = read_xlsx(path=paste0(path,"Dropbox/6-WILDOCEANS/Planning/scenarios.xlsx"),sheet = 2)

# decide on your parameters
options(scipen = 100) # turns of scientific numbering

# adjusted coordinates for plotting
adjustedcoords = coordinates(places)[c(10,14),]
adjustedcoords[,2] = adjustedcoords[,2]+0.2

# first decide on which scenario you are runnin (row number of spreadsheet)
for(i in 1:12){
  n = i # problem number
  scenario = scenario_sheet$scenario[n] # scenario name
  season = scenario_sheet$season[n] # season
  features = scenario_sheet$features[n] # features to use
  format = scenario_sheet$format[n] # format of features i.e. continuous or seasonal
  locked_in = scenario_sheet$lockedin[n] # sets areas to be locked-in
  costs = scenario_sheet$costs[n] # sets costs
  target = scenario_sheet$targets[n] # sets targets i.e. uniform or tailored
  penalty = scenario_sheet$penalty[n] # sets boundary penalties
  
  # retrieve correct objects based on problem number
  f = get(features)
  c = get(costs)
  if(target == "tailored"){
    t = featurenames # dataframe with tailored targets
    # further refine by season
    if(season == "aseasonal"){t = t %>% filter(MODELTYPE == "Aseasonal") %>% dplyr::select(targetsa)
    t = t$targetsa}
    if(season == "summer"){t = t %>% filter(MODELTYPE == "summer")%>% dplyr::select(targetsa)
    t = t$targetsa}
    if(season == "winter"){t = t %>% filter(MODELTYPE == "winter")%>% dplyr::select(targetsa)
    t = t$targetsa}
    # if target is numeric just take that value
  }else{t = as.numeric(target)}
  
  # run conservation problem with or without locked in constraints
  if(locked_in == "none"){ 
    problem_single = problem(c,f) %>% # costs and features
      add_min_set_objective() %>%
      add_relative_targets(t) %>%
      add_boundary_penalties(penalty) %>% # add penalty
      add_binary_decisions() %>%
      add_gurobi_solver(verbose = FALSE)}else{
        problem_single = problem(c,f) %>%
          add_min_set_objective() %>%
          add_relative_targets(t) %>%
          add_boundary_penalties(penalty) %>% # add penalty
          add_binary_decisions() %>%
          add_locked_in_constraints(mpa_layer) %>%
          add_gurobi_solver(verbose = FALSE)}
  
  # solve single solution
  #source(list.files(pattern = "Solution", recursive = TRUE)) 
  solution_single = solve(problem_single)
  
  #source(list.files(pattern = "Performances", recursive = TRUE)) 
  # evaluate performance
  performances = data.frame()
  # number of planning units selected within a solution.
  pus = eval_n_summary(problem_single, solution_single)
  performances[1,1] = pus[1,2]
  # calculate % of EEZ represented (there are 42053 cells in the EEZ)
  performances$prop_eez = (performances$cost/42053)*100
  
  # save as raw raster file
  writeRaster(solution_single,paste0("Planning/Outputs/solutions/rasters_rawsolutions/","p",n,"_",scenario,"scenario.tiff"))
  
  # plot single solution
  png(file=paste0("Planning/Outputs/solutions/national/","p",n,"_",scenario,"scenario.png"),width=3000, height=2000, res=300)
  plot(solution_single, col = c("grey90", "darkgreen"),
      # main title
       main = paste(scenario,"scenario","\nTargets:",target,"Penalty:",penalty),
       # sub title
       sub = paste("Features:",season,format,"\nPercentage of EEZ = ",round(performances$prop_eez,0),"%"),
       legend = FALSE)
  plot(mpas,add = TRUE)
  plot(eez, add = TRUE)
  plot(places[c(1:3,5,6,18,20:22,10,14),],col = "black",pch=20,cex=0.6, add = TRUE)
  text(coordinates(places)[c(1:3,5,6),], places$Location[c(1:3,5,6)], col = "black", pos=4, cex = 0.5)
  text(coordinates(places)[c(18,20,21,22),], places$Location[c(18,20,21,22)], col = "black", pos=2, cex = 0.5)
  text(adjustedcoords, places$Location[c(10,14)], col = "black", adj=0.5, pos=2, cex = 0.5)
  dev.off()
  
  # plot single solution per ebert range
  for(j in 1:length(range)){
    # subset range
    subset = regions[regions$Region%in%range[j],]
    png(file=paste0("Planning/Outputs/solutions/regional/","p",n,"_",range[j],"_",scenario,"scenario.png"),width=3000, height=2000, res=300)
    plot(crop(solution_single,subset), col = c("grey90", "darkgreen"),
         # main title
         main = paste(scenario,"scenario","\nTargets:",target,"Penalty:",penalty),
         # sub-title
         sub = paste("Features:",season,format,"\nPercentage of EEZ = ",round(performances$prop_eez,0),"%"),
         legend = FALSE)
    plot(crop(mpas,subset),add = TRUE)
    plot(eez, add = TRUE)
    plot(places[c(1:3,5,6,18,20:22,10,14),],col = "black",pch=20,cex=0.6, add = TRUE)
    text(coordinates(places)[c(1:3,5,6),], places$Location[c(1:3,5,6)], col = "black", pos=4, cex = 0.5)
    text(coordinates(places)[c(18,20,21,22),], places$Location[c(18,20,21,22)], col = "black", pos=2, cex = 0.5)
    text(adjustedcoords, places$Location[c(10,14)], col = "black", adj=0.5, pos=2, cex = 0.5)
    dev.off()
  }
  
  # ferrier score for single problem
  ferrierscore_single = eval_ferrier_importance(problem_single, solution_single)[["total"]]
  
  # save as raw raster file
  writeRaster(ferrierscore_single,paste0("Planning/Outputs/solutions/rasters_rawsolutions/","p",n,"_",scenario,"scenario_FS.tiff"))
  
  # plot ferrier score plot
  png(file=paste0("Planning/Outputs/solutions/ferrierscores/","p",n,"_",scenario,"scenario","_ferrierscore.png"),width=3000, height=2000, res=300)
  plot(ferrierscore_single,
       # main title
       main = paste("Ferrier score",scenario,"scenario","\nTargets:",target,"Penalty:",penalty),
       # sub title
       sub = paste("Features:",season,format,"\nPercentage of EEZ = ",round(performances$prop_eez,0),"%"))
  plot(mpas,add = TRUE)
  plot(eez, add = TRUE)
  plot(places[c(1:3,5,6,18,20:22,10,14),],col = "black",pch=20,cex=0.6, add = TRUE)
  text(coordinates(places)[c(1:3,5,6),], places$Location[c(1:3,5,6)], col = "black", pos=4, cex = 0.5)
  text(coordinates(places)[c(18,20,21,22),], places$Location[c(18,20,21,22)], col = "black", pos=2, cex = 0.5)
  text(adjustedcoords, places$Location[c(10,14)], col = "black", adj=0.5, pos=2, cex = 0.5)
  
  dev.off()
  rm(f,c,t,penalty,season,scenario,target,locked_in,costs,features,format,solution_single,problem_single,performances,pus)
}

# ---------------------------------
# 10 - PLOTTING
# ---------------------------------
# plot single solution
png(file=paste0("Planning/Outputs/solutions/national/","p",n,"_",scenario,"scenario.png"),width=3000, height=2000, res=300)
plot(solution_single, col = c("grey90", "darkgreen"),
     # add scenario parameters as title
     main = paste(scenario,"scenario","\nTargets:",target,"Penalty:",penalty),
     # add problem number and % of EEZ taken to subtitle
     sub = paste("Features:",season,format,"\nPercentage of EEZ = ",round(performances$prop_eez,0),"%"),
     legend = FALSE)
plot(mpas,add = TRUE)
dev.off()

# plot single solution per ebert range
for(j in 1:length(range)){
  # subset range
  subset = regions[regions$Region%in%range[j],]
  png(file=paste0("Planning/Outputs/solutions/regional/","p",n,"_",scenario,"scenario.png"),width=3000, height=2000, res=300)
  plot(crop(solution_single,subset), col = c("grey90", "darkgreen"), main = paste(scenario,"scenario","\nTargets:",target),legend = FALSE)
  plot(crop(mpas,subset),add = TRUE)
  dev.off()
}

# ferrier score for single problem
ferrierscore_single = eval_ferrier_importance(problem_single, solution_single)[["total"]]

# plot solutions
# plotted with the mpas
png(file=paste0("Planning/Outputs/solutions/ferrierscores/","p",n,"_",scenario,"scenario","_ferrierscore.png"),width=3000, height=2000, res=300)
plot(ferrierscore_single, main = paste("Ferrier score",scenario,"scenario","\nTargets:",target))
plot(mpas,add = TRUE)
dev.off()


