# ---------------------------------------------------------------------------------
# AUTHOR: Nina Faure Beaulieu (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script plots all of the conservation planning solutions
# ---------------------------------

# ---------------------------------
# PACKAGES
# ---------------------------------
# list of required packages
requiredpackages = c("gridExtra","rgeos","sf","dplyr","tidyr","stringr","rasterVis","viridis","raster","scales","readxl","fasterize","sdmvspecies","RColorBrewer")
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
# PLOTTING PARAMETERS
# ---------------------------------
source(list.files(pattern = "plottingparameters.R", recursive = TRUE, full.names= TRUE))
# plotting settings
settings = latticeExtra::layer(sp.polygons(mpas,col = "black",lwd = 1))+
  # mpa no-take fill
  latticeExtra::layer(sp.polygons(mpas_notake, fill = "purple",alpha = 0.1))+
  # mpa no-take outline
  latticeExtra::layer(sp.polygons(mpas_notake,col = "black",lwd = 0.7))+
  # eez
  latticeExtra::layer(sp.polygons(eez,col = "black",lwd = 1))+
  # sa coast
  latticeExtra::layer(sp.polygons(sa,col = "black",lwd= 1, fill = "white"))+
  # points for main cities
  latticeExtra::layer(sp.points(places[c(1:3,5,6,18,20:22,10,14),],col = "black",pch = 20))+
  # coordinates and city names
  # done in three lines as a "pretty" position varies based on their place on the map
  latticeExtra::layer(sp.text(coordinates(places)[c(1:3,5,6),],places$Location[c(1:3,5,6)],col = "black",pch = 20,pos=4,cex = 0.5))+
  latticeExtra::layer(sp.text(coordinates(places)[c(18,20,21,22),],places$Location[c(18,20,21,22)],col = "black",pch = 20,pos=2,cex = 0.5))+
  latticeExtra::layer(sp.text(adjustedcoords,places$Location[c(10,14)],col = "black",pch = 20, pos=2,cex = 0.5))+
  latticeExtra::layer(sp.text(coordinates(legend)[1,],paste0("Percentage of EEZ = ",prop_eez,"%"),col = "black",pch = 20, pos=2,cex = 1))+
  latticeExtra::layer(sp.text(coordinates(legend)[2,],paste0("Current MPAs included = ",inclusion),col = "black",pch = 20, pos=2,cex = 1))
# ---------------------------------


# ---------------------------------
# VISUALIZE RESULTS
# ---------------------------------

# scenarios
scenario_sheet = read_xlsx(path=paste0(path,"Dropbox/6-WILDOCEANS/Planning/scenarios.xlsx"),sheet = 1)

# list of all solutions in raster format
general_solutions = list.files(path = paste0(my.directory,"/Planning/Outputs/"),pattern = "scenario.tif",recursive = TRUE, full.names = TRUE)

# list of all ferrier scores in raster format
ferrier_scores = list.files(path = paste0(my.directory,"/Planning/Outputs/"),pattern = "scenario_FS.tif",recursive = TRUE, full.names = TRUE)

# list of all irraplaceability scores in raster format
irraplaceability_scores = list.files(path = paste0(my.directory,"/Planning/Outputs/"),pattern = "scenario_IR.tif",recursive = TRUE, full.names = TRUE)

# list of all solution information
performances = list.files(path = paste0(my.directory,"/Planning/Outputs/"),pattern = "performance.csv",recursive = TRUE, full.names = TRUE)

# the following loop will plot each raster individually
for(i in 1:length(files)){
  
  # stack of raster files
  raster_stack = stack(general_solutions[i],ferrier_scores[i],irraplaceability_scores[i]) 
  # raster name
  name = names(raster_stack[[1]])
  # problem number
  pnumber = str_split(name,"_")[[1]][1]
  # stream
  stream = str_split(name,"_")[[1]][2]
  if(stream == "streamA"){stream = "no"}else{stream = "yes"}
  # scenario
  scenario = str_split(name,"_")[[1]][3]
  # prop_eez
  maxvalue = max(values(temp),na.rm=TRUE)
  prop_eez = round((length(which(values(raster_stack[[1]])==maxvalue))/10809)*100,1)
  # target
  t = as.numeric(scenario_sheet$targets[i])*100
  # mpas included
  inclusion = scenario_sheet$lockedin[i]
  if(inclusion == "mpa_layer_fullyprotected"){inclusion = "no-take zones"}
  if(inclusion == "mpa_layer_all"){inclusion = "all MPAs"}
  if(inclusion == "none"){inclusion = "No MPAs"}
  
  # binary solution plots 
  plot_type = names(raster_stack[[1]])
  plot=levelplot(raster_stack[[1]],
                 xlab = NULL,
                 ylab = NULL,
                 main = paste0(scenario," scenario","\nWeighted: ",stream," | Species protection: ",t,"%"),
                 col.regions = cols,
                 margin = FALSE,
                 colorkey=FALSE)+settings
  png(file=paste0("Planning/Outputs/solutions/maps/",plot_type,".png"),width=3000, height=2000, res=300)
  print(plot)
  dev.off()
  
  # ferrier and irraplaceability solution plots
  for(a in 2:3){
    plot_type = names(raster_stack[[a]]) # plot name
    plot = levelplot(raster_stack[[a]],
                   xlab = NULL,
                   ylab = NULL,
                   colorkey=list(space="bottom", title = "Irraplaceability "),
                   main = paste0(scenario," scenario","\nWeighted: ",stream," | Species protection: ",t,"%"),
                   margin = FALSE,
                   col.regions = rev(heat.colors(32)),at = intervals2)+settings
    png(file=paste0("Planning/Outputs/solutions/maps/",plot_type,".png"),width=3000, height=2000, res=300)
    print(plot)
    dev.off()

  # extract values in all MPA zones
  values = extract(raster_stack[[a]],mpas)
  
  mpas_temp = mpas
  
  for (b in 1:length(values)){
    sum = sum(values[[b]], na.rm = TRUE)
    mpas_temp$importance[b] = sum}
  
    result = mpas_temp@data %>%
      arrange(importance)
    
    colnames(result) = c("MPA_zone","MPA","MPA_type","Irreplaceability")
    
    result = result %>%
      mutate(Irreplaceability = round(Irreplaceability,1))%>%
      arrange(MPA_type,desc(Irreplaceability))
    
    write.csv(result,paste0("Planning/Outputs/solutions/mpa_rankings/",plot_type,"_zoneimportance.csv"), row.names = FALSE)
    
    result = result %>%
      group_by(MPA) %>%
      summarise(Irreplaceability = round(sum(Irreplaceability),1))%>%
      arrange(desc(Irreplaceability))
    
    write.csv(result,paste0("Planning/Outputs/solutions/mpa_rankings/",plot_type,"_mpaimportance.csv"), row.names = FALSE)
  
    mpas_temp1 = st_as_sf(mpas_temp) %>%
      group_by(CUR_NME)%>%
      summarise(importance = round(sum(importance),0))
 
   mpas_temp1 = as(mpas_temp1, Class = "Spatial")
  
   mpas_temp1$categories = as.numeric(cut(mpas_temp1$importance,5))
  
   # Find unique colors from color ramp,based on 'importance' column 
   color.match = manual.col(length(unique(mpas_temp1$categories)))
  
   # Sort the values of interest (in this case, 'Prop')
   lookupTable = sort(unique(mpas_temp1$categories))
  
   # Match colors to sorted unique values in polygon
   # and assign them to a new column in the polygon data
   # so that they plot smallest values as lightest and largest values as darkest
  
   mpas_temp1$color = color.match[match(mpas_temp1$categories, lookupTable)]
 
    # Plot the final product!
  
   plot = levelplot(raster_stack[[a]],
                   xlab = NULL,
                   ylab = NULL,
                   colorkey=list(space="bottom"),
                   main = paste0(scenario," scenario","\nWeighted: ",stream," | Species protection: ",t,"%"),
                   margin = FALSE,
                   col.regions = rev(heat.colors(32)),at = intervals2)+settings+
    latticeExtra::layer(sp.polygons(mpas_temp1,fill=mpas_temp1$color, lwd = 1))
  png(file=paste0("Planning/Outputs/solutions/",plot_type,"_mparanks.png"),width=3000, height=2000, res=300)
  print(plot)
  dev.off()  
  
  # filter to only use no take zones
  mpas_temp2 = mpas_temp[which(mpas_temp$type == "take"),]
  mpas_temp2$categories = as.numeric(cut(mpas_temp2$importance,5))
  # Find unique colors from color ramp,based on 'importance' column 
  color.match = manual.col(length(unique(mpas_temp2$categories)))
  # Sort the values of interest (in this case, 'Prop')
  lookupTable = sort(unique(mpas_temp2$categories))
  # Match colors to sorted unique values in polygon
  # and assign them to a new column in the polygon data
  # so that they plot smallest values as lightest and largest values as darkest
  mpas_temp2$color = color.match[match(mpas_temp2$categories, lookupTable)]
  # Plot the final product!
  plot = levelplot(raster_stack[[a]],
                   xlab = NULL,
                   ylab = NULL,
                   colorkey=list(space="bottom"),
                   main = paste0(scenario," scenario","\nWeighted: ",stream," | Species protection: ",t,"%"),
                   margin = FALSE,
                   col.regions = rev(heat.colors(32)),at = intervals2)+settings+
    latticeExtra::layer(sp.polygons(mpas_temp2,fill=mpas_temp2$color, lwd = 1))
  png(file=paste0("Planning/Outputs/solutions/",plot_type,"zoneranks.png"),width=3000, height=2000, res=300)
  print(plot)
  dev.off()  
  rm(mpas_temp,mpas_temp1, mpas_temp2)
  }}

# END OF SCRIPT