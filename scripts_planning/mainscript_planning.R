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
library(RColorBrewer)

# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------

# all the subscripts are in the scripts folder
# all the data is in the planning folder
# make sure you choose the correct patg and set the directory to the overall WILDOCEANS folder so you can find both
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
pu = raster(list.files(pattern = "template_10km.tif",full.names = TRUE,recursive = TRUE))

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
source(list.files(pattern = "Lockedin.R", recursive = TRUE))

# ---------------------------------
# 6 - CONSERVATION TARGETS
# output: this adds targets to featurenames dataframe
# ---------------------------------
source(list.files(pattern = "Speciestargets.R", recursive = TRUE)) 

# ---------------------------------
# 7 - PLOTTING PARAMETERS
# ---------------------------------
source(list.files(pattern = "plottingparameters.R", recursive = TRUE, full.names= TRUE))

# ---------------------------------
# 8 - CONSERVATION PROBLEM & SOLVING
# output: prioritizr problem object
# ---------------------------------
# running from the mainscript for now 
# seems more practical as I learn more about the package
#source(list.files(pattern = "Problem.R", recursive = TRUE))
options(scipen = 100) # turns of scientific numbering

# load in scenarios
scenario_sheet = read_xlsx(path=paste0(path,"Dropbox/6-WILDOCEANS/Planning/scenarios.xlsx"),sheet = 2)

# colours for plots
cols <- colorRampPalette(c("white","darkgreen"))
cols2 <- colorRampPalette(c("yellow"))

# first decide on which scenario you are running (row number of spreadsheet)
for(i in 1:3){
  n = 2 # problem number
  scenario = scenario_sheet$scenario[n] # scenario name
  season = scenario_sheet$season[n] # season
  features = scenario_sheet$features[n] # features to use
  format = scenario_sheet$format[n] # format of features i.e. continuous or seasonal
  locked_in = scenario_sheet$lockedin[n] # sets areas to be locked-in
  costs = scenario_sheet$costs[n] # sets costs
  target = scenario_sheet$targets[n] # sets targets i.e. uniform or tailored
  tailoredtargets = scenario_sheet$tailored_targets[n] # set level of tailored targets
  boundary_penalty = as.numeric(scenario_sheet$boundary_penalty[n]) # sets boundary penalties
  #max_feature = scenario_sheet$max_feature[n]
  # retrieve correct objects based on problem number
  f = get(features)
  c = get(costs)
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
  
  # run conservation problem with or without locked in constraints
  if(locked_in == "none"){ 
    problem_single = problem(c,f) %>% # costs and features
      add_min_set_objective() %>%
      add_linear_constraints(threshold = 1080,sense = "<=",data = pu)%>%
      #add_absolute_targets(0.2) %>%
      add_relative_targets(t) %>%
      add_boundary_penalties(boundary_penalty) %>% # add penalty
      add_binary_decisions() %>%
      #add_neighbor_constraints(k=2) %>%
      add_gurobi_solver()}else{
    
        problem_single = problem(c,f) %>%
          #add_min_set_objective() %>%
          add_min__shortfall_objective(budget = 1080) %>% # budget representing 10% of planning units total
          add_relative_targets(t) %>%
          add_boundary_penalties(0.00001) %>% # add penalty
          add_binary_decisions() %>%
          add_locked_in_constraints(mpa_layer) %>%
          add_gurobi_solver(time_limit = 3600, gap = 0.2)}
  
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
  performances$prop_eez = (performances$cost/10809)*100
  
  # also save dataframe evaluating if targets are met
  coverage_summary = eval_target_coverage_summary(problem_single,solution_single)
  write.csv(coverage_summary,paste0(path,"Dropbox/6-WILDOCEANS/Planning/Outputs/performances/","p",str_pad(n,3,pad = "0"),"_performance.csv"), row.names = FALSE)
  
  # save as raw raster file
  writeRaster(solution_single,paste0("Planning/Outputs/solutions/rasters_rawsolutions/","p",str_pad(n,3,pad = "0"),"_",scenario,"scenario.tiff"),overwrite = TRUE)
  
  # plot single solution
  plot=levelplot(solution_single,
            main = paste(scenario,"scenario","\nTargets:",target,"| Target category:",tailoredtargets,"\nPenalty:",boundary_penalty),
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
    latticeExtra::layer(sp.polygons(sa_coast,col = "black",lwd= 1))+
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
    cropped = crop(solution_single,subset)
    plot = levelplot(cropped, 
              main = paste(scenario,"scenario","\nTargets:",target,"| Target category:",tailoredtargets,"\nPenalty:",boundary_penalty),
              sub = paste("Features:",season,format,"\nPercentage of EEZ = ",round(performances$prop_eez,0),"%"),
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
      latticeExtra::layer(sp.polygons(sa_coast,col = "black",lwd= 1))+
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
  ferrierscore_single = eval_ferrier_importance(problem_single, solution_single)[["total"]]
  
  # save as raw raster file
  writeRaster(ferrierscore_single,paste0("Planning/Outputs/solutions/rasters_rawsolutions/","p",str_pad(n,3,pad = "0"),"_",scenario,"scenario_FS.tiff"), overwrite = TRUE)
  
  # plot ferrier score plot
  png(file=paste0("Planning/Outputs/solutions/ferrierscores/","p",str_pad(n,3,pad = "0"),"_",scenario,"scenario","_ferrierscore.png"),width=3000, height=2000, res=300)
  plot(ferrierscore_single,
       # main title
       main = paste("Ferrier score",scenario,"scenario","\nTargets:",target,"| Target category:",tailoredtargets,"\nPenalty:",boundary_penalty),
       # sub title
       sub = paste("Features:",season,format,"\nPercentage of EEZ = ",round(performances$prop_eez,0),"%"))
  plot(mpas,add = TRUE,legend = FALSE)
  plot(eez, add = TRUE,legend = FALSE)
  plot(places[c(1:3,5,6,18,20:22,10,14),],col = "black",pch=20,cex=0.6, add = TRUE)
  text(coordinates(places)[c(1:3,5,6),], places$Location[c(1:3,5,6)], col = "black", pos=4, cex = 0.5)
  text(coordinates(places)[c(18,20,21,22),], places$Location[c(18,20,21,22)], col = "black", pos=2, cex = 0.5)
  text(adjustedcoords, places$Location[c(10,14)], col = "black", adj=0.5, pos=2, cex = 0.5)
  
  dev.off()
  # leave solution_single in environment to facilitate plotting tests
  rm(f,c,t,boundary_penalty,season,scenario,target,locked_in,costs,features,format,problem_single,performances,pus)
}

# ---------------------------------
#  - PLOTTING (MORE PLOTTING)
# ---------------------------------
solutions_all = list.files(path = paste0("Planning/Outputs/solutions/rasters_rawsolutions/"),pattern = "Basescenario.tif",full.names = TRUE)
all_scenarios = stack(solutions_all[1:3])
plot(all_scenarios)
all = calc(all_scenarios,sum)
plot(all)
?calc


