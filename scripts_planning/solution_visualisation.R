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
library(gridExtra)
# ---------------------------------


# ---------------------------------
# PLOTTING PARAMETERS
# ---------------------------------
source(list.files(pattern = "plottingparameters.R", recursive = TRUE, full.names= TRUE))
# ---------------------------------


# ---------------------------------
# VISUALIZE RESULTS
# ---------------------------------

# list of all solutions in raster format
files = list.files(path = paste0(my.directory,"/Planning/Outputs/"),pattern = "scenario.tif",recursive = TRUE, full.names = TRUE)

# the following loop will plot each raster individually
# 4 plots are produce for each raster: the entire EEZ, and then the west, south and east coast seperately
for(i in 1:length(files)){
  
  # temporary raster
  temp = raster(files[i])
  
  # plot of entire EEZ
  plot=levelplot(temp,
                 main = paste(scenario,"scenario","\nTargets:",target,"| Target category:",tailoredtargets,"\nPenalty:",boundary_penalty),
                 sub = paste(objective,"percentage of EEZ held:",prop_eez,"%"),
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
  
  # plot of each region seperately
  # plot single solution per ebert range
  for(j in 1:length(range)){
    # subset range
    subset = regions[regions$Region%in%range[j],]
    cropped = crop(temp,subset)
    plot = levelplot(cropped, 
                     main = paste(scenario,"scenario","\nTargets:",target,"| Target category:",tailoredtargets,"\nPenalty:",boundary_penalty),
                     sub = paste("Objective:",objective,"Features:",season,format,"\nPercentage of EEZ = ",prop_eez,"%"),
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
}}

# the following loop will plot rasters in groups of 4 and save each plot
for(i in seq(1,length(files),4)){
  # isolate single raster
  temp = stack( raster(files[i]),  raster(files[i+1]), raster(files[i+2]), raster(files[i+3]))
  # create plot
  temp_plot = rasterVis::levelplot(temp,
                                   names.attr = c("10%","20%","30%","40%"),
                                   margin = FALSE,
                                   colorkey = FALSE,
                                   xlab = "Species area percentage protection - all MPAs included",
                                   ylab = NULL,
                                   col.regions = cols,
                                   # reduces size tick marks
                                   scales = list(tck = c(0.5,0.5)),
                                   # reduces space at top and bottom of plot
                                   par.settings = list(layout.heights=list(top.padding=-2, bottom.padding = -1),
                                                       axis.line = list(col = "transparent"), 
                                                       strip.background = list(col = 'transparent')))+
    # mpa outline
    latticeExtra::layer(sp.polygons(mpas,col = "black",lwd = 1))+
    # eez
    latticeExtra::layer(sp.polygons(eez,col = "black",lwd = 1))+
    # sa coast
    latticeExtra::layer(sp.polygons(sa_coast,col = "black",lwd= 1, fill = "white"))+
    # points for main cities
    latticeExtra::layer(sp.points(places[c(1:3,5,6,18,20:22,10,14),],col = "black",pch = 20))+
    # mpa filled no take only
    #levelplot(mpas,col.regions = cols2, alpha.regions=0.6)+
    # coordinates and city names
    # done in three lines as a "pretty" position varies based on their place on the map
    latticeExtra::layer(sp.text(coordinates(places)[c(1:3,5,6),],places$Location[c(1:3,5,6)],col = "black",pch = 20,pos=4,cex = 0.5))+
    latticeExtra::layer(sp.text(coordinates(places)[c(18,20,21,22),],places$Location[c(18,20,21,22)],col = "black",pch = 20,pos=2,cex = 0.5))+
    latticeExtra::layer(sp.text(adjustedcoords,places$Location[c(10,14)],col = "black",pch = 20, pos=2,cex = 0.5))
  # save plot
  str_split(files[i],"/Users/nfb/Dropbox/6-WILDOCEANS/Planning/Outputs//solutions/rasters_rawsolutions/")[[1]][2]
  png(paste0(".png"),width=3000, height=2000, res=300)
  temp_plot
  dev.off()
}

# the following loop will plot each ferrier score individually

# list of all ferrier scores in raster format
files = list.files(path = paste0(my.directory,"/Planning/Outputs/"),pattern = "scenario_FS.tif",recursive = TRUE, full.names = TRUE)

for(i in 1:length(files)){
  temp = raster(files[i])
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
}
