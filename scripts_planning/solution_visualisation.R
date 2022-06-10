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
requiredpackages = c("colorspace","gridExtra","rgeos","sf","dplyr","tidyr","stringr","rasterVis","viridis","raster","scales","readxl","fasterize","sdmvspecies","RColorBrewer")
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
path =  "/home/nina/" # path for pc
my.directory = paste0(path,"Dropbox/6-WILDOCEANS")
# set directory
setwd(my.directory) 
# ---------------------------------


# ---------------------------------
# PLOTTING PARAMETERS
# ---------------------------------
source(list.files(pattern = "plottingparameters.R", recursive = TRUE, full.names= TRUE))
# plotting settings
settings = 
  # mpa outline
  latticeExtra::layer(sp.polygons(mpas,col = "blue",lwd = 0.7))+
  # mpa no-take outline
  latticeExtra::layer(sp.polygons(mpas_notake,col = "red",lwd = 0.7))+
  # mpas no-take fill
  latticeExtra::layer(sp.polygons(mpas_notake,fill = "red",alpha = 0.2))+
  # eez
  latticeExtra::layer(sp.polygons(eez,col = "black",lwd = 1))+
  # sa coast
  latticeExtra::layer(sp.polygons(sa,col = "black",lwd= 1, fill = "grey",alpha=0.3), under = TRUE)+
  # points for main cities
  latticeExtra::layer(sp.points(places[c(1:3,5,6,18,20:22,10,14),],col = "black",pch = 20))+
  # coordinates and city names
  # done in three lines as a "pretty" position varies based on their place on the map
  latticeExtra::layer(sp.text(coordinates(places)[c(1:3,5,6),],places$Location[c(1:3,5,6)],col = "black",pch = 20,pos=4,cex = 0.5))+
  latticeExtra::layer(sp.text(coordinates(places)[c(18,20,21,22),],places$Location[c(18,20,21,22)],col = "black",pch = 20,pos=2,cex = 0.5))+
  latticeExtra::layer(sp.text(adjustedcoords,places$Location[c(10,14)],col = "black",pch = 20, pos=2,cex = 0.5))+
  # box on top left describing scenario
  latticeExtra::layer(sp.polygons(sps, fill = "white"))+
  latticeExtra::layer(sp.text(c(13.4,-27.1),paste0("Scenario: ",pnumber,", ",scenario," scenario"),cex = 0.7, font = 2, adj=0))+
  latticeExtra::layer(sp.text(c(13.4,-27.4),paste0("Protection targets: Tailored"),cex = 0.7,font = 2,adj=0))+
  latticeExtra::layer(sp.text(c(13.4,-27.7),paste0("Biodiversity features: Elasmobranch distributions"),cex = 0.7,font = 2,adj=0))+
  latticeExtra::layer(sp.text(c(13.4,-28),paste0("Species included: ",features),cex = 0.7,font = 2,adj=0))+
  latticeExtra::layer(sp.text(c(13.4,-28.3),paste0("MPAs locked-in: ",inclusion),cex = 0.7,font = 2,adj=0))+
  # legend text
  latticeExtra::layer(sp.text(c(30.7,-36.4),"No-take zones (Restricted, Sanctuary, Wilderness areas)",cex = 0.5, adj=0))+
  latticeExtra::layer(sp.text(c(30.7,-36.7),"Marine Protected Area (MPA) network",cex = 0.5, adj=0))+
  # box on bottom right with scenario stats
  latticeExtra::layer(sp.polygons(sps2, fill = "white"))+
  latticeExtra::layer(sp.text(c(36.5,-37.3),paste0("Percentage of EEZ covered = ",prop_eez,"%"),col = "black",pch = 20, pos=2,cex = 0.7, adj=0))+
  latticeExtra::layer(sp.text(c(36.5,-37.6),paste0("Number of species included = ",n_spp),col = "black",pch = 20, pos=2,cex = 0.7, adj=0))+
  latticeExtra::layer(sp.text(c(36.5,-37.9),paste0("Avg. % range protection per species = ",mean_protection,"(±",sd_protection,")"),col = "black",pch = 20, pos=2,cex = 0.7, adj=0))

# settings 2 (for province plots as need to remove legend)
settings2 = 
  # mpa outline
  latticeExtra::layer(sp.polygons(mpas,col = "blue",lwd = 0.7))+
  # mpa no-take outline
  latticeExtra::layer(sp.polygons(mpas_notake,col = "red",lwd = 0.7))+
  # mpas no-take fill
  latticeExtra::layer(sp.polygons(mpas_notake,fill = "red",alpha = 0.2))+
  # eez
  latticeExtra::layer(sp.polygons(eez,col = "black",lwd = 1))+
  # sa coast
  latticeExtra::layer(sp.polygons(sa,col = "black",lwd= 1, fill = "grey",alpha=0.3), under = TRUE)+
  # points for main cities
  latticeExtra::layer(sp.points(places[c(1:3,5,6,18,20:22,10,14),],col = "black",pch = 20))+
  # coordinates and city names
  # done in three lines as a "pretty" position varies based on their place on the map
  latticeExtra::layer(sp.text(coordinates(places)[c(1:3,5,6),],places$Location[c(1:3,5,6)],col = "black",pch = 20,pos=4,cex = 0.5))+
  latticeExtra::layer(sp.text(coordinates(places)[c(18,20,21,22),],places$Location[c(18,20,21,22)],col = "black",pch = 20,pos=2,cex = 0.5))+
  latticeExtra::layer(sp.text(adjustedcoords,places$Location[c(10,14)],col = "black",pch = 20, pos=2,cex = 0.5))

# legend box (top left)
x_coord <- c(23.5,  13,  13, 23.5, 23.5)
y_coord <- c(-28.5, -28.5, -26, -26, -28.5)
xym <- cbind(x_coord, y_coord)
p = Polygon(xym)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
# stats box (bottom right)
x_coord <- c(36.6,  30,  30, 36.6, 36.6)
y_coord <- c(-38.3, -38.3, -37, -37, -38.3)
xym <- cbind(x_coord, y_coord)
p = Polygon(xym)
ps = Polygons(list(p),1)
sps2 = SpatialPolygons(list(ps))
# ---------------------------------


# ---------------------------------
# FILES
# ---------------------------------
# scenarios
scenario_sheet = read_xlsx(path=paste0(my.directory,"/ConservationPlan/Planning/scenarios.xlsx"),sheet = 1)

# list of all solutions in raster format
general_solutions = list.files(path = paste0(my.directory,"/ConservationPlan/Planning/Outputs/solutions/rasters_rawsolutions_working/"),pattern = "scenario.tif",recursive = TRUE, full.names = TRUE)

# list of all irraplaceability scores in raster format
irraplaceability_scores = list.files(path = paste0(my.directory,"/ConservationPlan/Planning/Outputs/solutions/rasters_rawsolutions_working/"),pattern = "_IR",recursive = TRUE, full.names = TRUE)

# list of all solution information
performances = list.files(path = paste0(my.directory,"/ConservationPlan/Planning/Outputs/solutions/performances_working/"),pattern = "performance.csv",recursive = TRUE, full.names = TRUE)
# ---------------------------------


# ---------------------------------
# PLOTS (basic plots)
# ---------------------------------
# the following loop will plot each binary raster individually
for(i in 1:length(general_solutions)){
  
  # raster file
  temp = raster(general_solutions[i]) 
  # raster name
  name = names(temp)
  # problem number
  pnumber = str_split(name,"_")[[1]][1]
  # scenario
  scenario = str_split(name,"_")[[1]][2]
  # prop_eez
  prop_eez = round((length(which(values(temp)!=0))/10809)*100,1)
  
  # mpas included
  inclusion = scenario_sheet$lockedin[i]
  if(inclusion == "mpa_layer_fullyprotected"){inclusion = "No-take zones"}
  if(inclusion == "mpa_layer_all"){inclusion = "All MPAs"}
  if(inclusion == "none"){inclusion = "None"}
  
  # features included
  features = scenario_sheet$features[i]
  if(features == "sdms_all"){features = "Raw distribution maps"}
  if(features == "sdms_thresholds"){features = "all modelled species"}
  if(features == "sdms_specialspp1"){features = "IUCN (CR,EN) and/or South African endemics"}
  if(features == "sdms_specialspp2"){features = "IUCN (CR,EN,VU) and/or Southern African endemics"}
  
  plot_type = names(temp)
  
  # convert <1 values to NA (so that land goes over raster at border)
  values(temp)[which(values(temp)<1)] = NA

  # get stats for scenario
  stats = read.csv(performances[i])
  n_spp = nrow(stats)
  mean_protection = round(mean(stats$target_achieved),2)*100
  sd_protection = round(sd(stats$target_achieved),2)*100
  
  # turn raster to polygon or else some features of plot do not work
  temp_polygon = rasterToPolygons(temp)
  
  # binary solution plots 
  plot=levelplot(blank_template,
                 xlab = NULL,
                 ylab = NULL,
                 #main = paste0(pnumber," | ",scenario," scenario | Protection targets: ",t,"\n",features),
                 #col.regions = cols,
                 margin = FALSE, 
                 colorkey = FALSE,
                 panel = function(x,y,...){
                   panel.points(x=30.5, y=-36.1, col = "dark green",lwd = 0.7,cex=1, pch = 15)
                   panel.points(x=30.5, y=-36.4, col = "red",lwd = 0.7,cex=1, pch = 0)
                   panel.points(x=30.5, y=-36.4, col = "red",alpha=0.2,pch = 15)
                   panel.points(x=30.5, y=-36.7, col = "blue",lwd = 0.7,cex=1, pch = 0)})+
    # solution
    latticeExtra::layer(sp.polygons(temp_polygon,fill = "dark green",col = "dark green",cex = 0.4,alpha = 0.8, pch = 21))+
    # priority area legend text
    latticeExtra::layer(sp.text(c(30.7,-36.1),"Priority areas",cex = 0.5, adj=0))+
    settings
  
  png(file=paste0(my.directory,"/ConservationPlan/Planning/Outputs/solutions/binary_working/",plot_type,".png"),width=3000, height=2000, res=300)
  print(plot) 
  dev.off()
      }

# the following loop will plot each irraplacability plot
for(i in 1:length(irraplaceability_scores)){
  # stack of raster files
  temp = raster(irraplaceability_scores[i]) 
  # raster name
  name = names(temp)
  # problem number
  pnumber = str_split(name,"_")[[1]][1]
  # scenario
  scenario = str_split(name,"_")[[1]][2]
  # prop_eez
  temp2 = raster(general_solutions[i]) 
  prop_eez = round((length(which(values(temp2)!=0))/10809)*100,1)
  rm(temp2)
 
  # mpas included
  inclusion = scenario_sheet$lockedin[i]
  if(inclusion == "mpa_layer_fullyprotected"){inclusion = "No-take zones"}
  if(inclusion == "mpa_layer_all"){inclusion = "All MPAs"}
  if(inclusion == "none"){inclusion = "None"}
  
  # features included
  features = scenario_sheet$features[i]
  if(features == "sdms_all"){features = "Raw distribution maps"}
  if(features == "sdms_thresholds"){features = "all modelled species"}
  if(features == "sdms_specialspp1"){features = "IUCN (CR,EN) and/or South African endemics"}
  if(features == "sdms_specialspp2"){features = "IUCN (CR,EN,VU) and/or Southern African endemics"}
  
  # irraplaceability solution plots
  plot_type = names(temp) # plot name
  
  # turn 0 values to NA
  values(temp)[values(temp) == 0] = NA
  
  # turn raster to polygon or else some features of plot do not work
  temp_polygon = rasterToPolygons(temp)
  
  # get stats for scenario
  stats = read.csv(performances[i])
  n_spp = nrow(stats)
  mean_protection = round(mean(stats$target_achieved),2)*100
  sd_protection = round(sd(stats$target_achieved),2)*100
  
  # use irreplaceability values for colors
  # look at values
  table(temp_polygon@data)
  # cut irreplaceability values into 5 catgeories
  temp_polygon$categories = as.numeric(cut(temp_polygon@data[,1],4))
  # set 5 colors
  color.match = c("dark green","orange","red","red4")
  # Sort the values
  lookupTable = sort(unique(temp_polygon$categories))
  # Match colors to sorted unique values in polygon
  # and assign them to a new column in the polygon data
  # so that they plot smallest values as lightest and largest values as darkest
  temp_polygon$color = color.match[match(temp_polygon$categories, lookupTable)]
  
  # plot
  plot = levelplot(blank_template,
                   #key = list(space = 'right', text = list(levels(cut(temp_polygon$p001_Base_scenario_IR,4))), 
                   #           points = list(pch = 15, fill = ptsCols)),
                   xlab = NULL,
                   ylab = NULL,
                   colorkey = FALSE,
                   margin = FALSE,
                   panel = function(x,y,...){
                      panel.points(x=30.7, y=-36, col = "dark green",lwd = 0.7,cex=2, pch = 15)
                      panel.points(x=30.7, y=-36, col = "black",lwd = 0.7,cex=2, pch = 0)
                      panel.points(x=30.5, y=-35.8, col = "black",lwd = 4,cex=0.5, pch = "|")
                      panel.points(x=31.1, y=-36, col = "orange",lwd = 0.7,cex=2, pch = 15)
                      panel.points(x=31.1, y=-36, col = "black",lwd = 0.7,cex=2, pch = 0)
                      panel.points(x=30.9, y=-35.9, col = "black",lwd = 0.7,cex=1, pch = "|")
                      panel.points(x=31.5, y=-36, col = "red",lwd = 0.7,cex=2, pch = 15)
                      panel.points(x=31.5, y=-36, col = "black",lwd = 0.7,cex=2, pch = 0)
                      panel.points(x=31.3, y=-35.9, col = "black",lwd = 0.7,cex=1, pch = "|")
                      panel.points(x=31.9, y=-36, col = "red4",lwd = 0.7,cex=2, pch = 15)
                      panel.points(x=31.9, y=-36, col = "black",lwd = 0.7,cex=2, pch = 0)
                      panel.points(x=31.7, y=-35.9, col = "black",lwd = 0.7,cex=1, pch = "|")
                      panel.points(x=32.1, y=-35.9, col = "black",lwd = 0.7,cex=1, pch = "|")
                     panel.points(x=30.5, y=-36.4, col = "red",lwd = 0.7,cex=1, pch = 0)
                     panel.points(x=30.5, y=-36.4, col = "red",alpha=0.2,pch = 15)
                     panel.points(x=30.5, y=-36.7, col = "blue",lwd = 0.7,cex=1, pch = 0)})+
    latticeExtra::layer(sp.polygons(temp_polygon,col=temp_polygon$color,fill=temp_polygon$color,cex = 0.4,alpha = 0.8, pch = 21))+
    settings+
    latticeExtra::layer(sp.text(c(32.2,-36),"Irreplaceability score",cex = 0.5, adj=0))+
    latticeExtra::layer(sp.text(c(30.4,-35.7),"0",cex = 0.5, adj=0,font=2))+
    #latticeExtra::layer(sp.text(c(30.7,-35.7),"0.25",cex = 0.5, adj=0,font=2))+
    latticeExtra::layer(sp.text(c(31.1,-35.7),"0.5",cex = 0.5, adj=0,font=2))+
    #latticeExtra::layer(sp.text(c(31.5,-35.7),"0.75",cex = 0.5, adj=0,font=2))+
    latticeExtra::layer(sp.text(c(32.1,-35.7),"1",cex = 0.5, adj=0,font=2))
  # save plot
    png(file=paste0(my.directory,"/ConservationPlan/Planning/Outputs/solutions/irraplaceability_working/",plot_type,".png"),width=3000, height=2000, res=300)
    print(plot)
    dev.off()
    
      # province plots
    for(x in 1:4){
      subname = names(bboxes)[x]
      # plot dimensions (height)
      dims = c(3000,3000,2000,3000)
      # plot
      plot = levelplot(crop(blank_template,bboxes[[x]]),
                       xlab = NULL,
                       ylab = NULL,
                       colorkey = FALSE,
                       margin = FALSE,
                       panel = function(x,y,...){
                         panel.points(x=30.5, y=-36.1, col = "dark green",lwd = 0.7,cex=1, pch = 15)
                         panel.points(x=30.5, y=-36.4, col = "red",lwd = 0.7,cex=1, pch = 0)
                         panel.points(x=30.5, y=-36.4, col = "red",alpha=0.2,pch = 15)
                         panel.points(x=30.5, y=-36.7, col = "blue",lwd = 0.7,cex=1, pch = 0)})+
      latticeExtra::layer(sp.polygons(temp_polygon,col=temp_polygon$color,fill=temp_polygon$color,cex = 0.4,alpha = 0.8, pch = 21))+
        settings2
      png(file=paste0(my.directory,"/ConservationPlan/Planning/Outputs/solutions/irraplaceability_working/provinceplots/",plot_type,"_",subname,".png"),width=3000, height=dims[x], res=300)
      print(plot)
      dev.off()
    }
    }
# ---------------------------------


# ---------------------------------
# PLOTS (more complex plots)
# ---------------------------------
# the following code will sum different solutions
# raster file
temp = stack(irraplaceability_scores[c(2,5,8)]) 
# summ all solutions
temp_sum = calc(temp,sum,na.rm=TRUE)
# plot
values(temp_sum)[values(temp_sum) == 0] = NA
plot = levelplot(temp_sum,
                 xlab = NULL,
                 ylab = NULL,
                 colorkey=list(space="right"),
                 #main = paste0(pnumber," | ",scenario," scenario | Protection targets: ",t,"\n",features),
                 margin = FALSE,
                 col.regions = rev(sequential_hcl(n=3)),
                 at = seq(0,3,1))+
  latticeExtra::layer(sp.polygons(mpas,col = "black",lwd = 1))+
  latticeExtra::layer(sp.polygons(cba_sp[1,],fill = "transparent",col = "dark green",lwd = 0.7))+
  latticeExtra::layer(sp.polygons(cba_sp[2,],fill = "transparent",col = "light green",lwd = 0.7))+
  # mpa no-take fill
  #latticeExtra::layer(sp.polygons(mpas_notake, fill = "purple",alpha = 0.1))+
  # mpa no-take outline
  latticeExtra::layer(sp.polygons(mpas_notake,col = "red",lwd = 0.7))+
  # eez
  latticeExtra::layer(sp.polygons(eez,col = "black",lwd = 1))+
  # sa coast
  latticeExtra::layer(sp.polygons(sa,col = "black",lwd= 1, fill = NA))+
  # points for main cities
  latticeExtra::layer(sp.points(places[c(1:3,5,6,18,20:22,10,14),],col = "black",pch = 20))+
  # coordinates and city names
  # done in three lines as a "pretty" position varies based on their place on the map
  latticeExtra::layer(sp.text(coordinates(places)[c(1:3,5,6),],places$Location[c(1:3,5,6)],col = "black",pch = 20,pos=4,cex = 0.5))+
  latticeExtra::layer(sp.text(coordinates(places)[c(18,20,21,22),],places$Location[c(18,20,21,22)],col = "black",pch = 20,pos=2,cex = 0.5))+
  latticeExtra::layer(sp.text(adjustedcoords,places$Location[c(10,14)],col = "black",pch = 20, pos=2,cex = 0.5))

png(file=paste0(my.directory,"/ConservationPlan/Planning/Outputs/solutions/irraplaceability_working/","ENDEMICSSUMMED.png"),width=3000, height=2000, res=300)
print(plot)
dev.off()
# ---------------------------------
