# ---------------------------------------------------------------------------------
# AUTHOR: Nina Faure Beaulieu (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script plots the models
# ---------------------------------


# ---------------------------------
# PACKAGES
# ---------------------------------
# list of required packages
requiredpackages = c("rgeos","viridis","rasterVis","ggplot2","raster","stringr", "raster", "sp", "dplyr", "lubridate")
# load packages
lapply(requiredpackages,require, character.only = TRUE)
rm(requiredpackages)
# ---------------------------------


# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------
# set directory to same parent folder where sub-scripts are found
# the subs-scripts can be in folders within this directory as the code will look through all the folders
my.directory = getwd()
# set directory
setwd(my.directory) 
# ---------------------------------


# ---------------------------------
#  PLOTTING LAYERS
# this subscript prepares layers required for plotting and sets plotting parameters i.e. colour etc..
# ---------------------------------
source(list.files(pattern = "plottingparameters.R", recursive = TRUE, full.names= TRUE))
# ---------------------------------


# ---------------------------------
# DATA FILES
# ---------------------------------
sdms_rasters = list.files(path = "wildoceans-scripts/Outputs/modelling/rasters/",pattern = "Aseasonal_res10_ensemblemean.tif", recursive = TRUE, full.names =TRUE)
# list of species
master = read_xlsx(list.files(pattern = "data_summary_master.xlsx", recursive = TRUE,full.names = TRUE))
# ---------------------------------


# ---------------------------------
# PLOTTING
# ---------------------------------
# plotfolder
plotfolder = paste0(getwd(),"/wildoceans-scripts/Outputs/modelling/prettyplots/")

for(i in 19:length(sdms_rasters)){
  target = str_split(sdms_rasters[i],"/")[[1]][6]
  target = str_split(target,"_")[[1]][1]
  model_type = str_split(sdms_rasters[i],"_")[[1]][2]
  temp = raster(sdms_rasters[i])
  values(temp) = values(temp)/1000
  res = str_split(sdms_rasters[i],"_")[[1]][3]
  
  # turn values of 0 to NA
  values(temp)[values(temp)==0] = NA
  
  # species info
  spp_info = master %>%
    filter(SPECIES_SCIENTIFIC == target)
  
  # IUCN data
  exists3 = file.info(list.files(pattern = paste(target,".gpkg",sep=""), recursive = TRUE, ignore.case = TRUE))
  if(nrow(exists3 !=0)){
    iucn_extent = st_read(list.files(pattern = paste(target,".gpkg",sep=""), recursive = TRUE, ignore.case = TRUE))
    iucn_extent = as(iucn_extent, Class = "Spatial")}else{rm(iucn_extent)}
  rm(exists3)
  
  # convert target to sentence case for plotting
  target = str_to_sentence(target)
  
  plot = levelplot(temp,
                   main = bquote(italic(.(target))~","~.(spp_info$Species_common)),
                   names.attr = c("Ensemble model"),
                   par.settings = rasterTheme(viridis_pal(option="D")(10)),
                   at = intervals/1000,
                   margin = FALSE,
                   xlab = NULL,
                   ylab=NULL)+
    # model type
    latticeExtra::layer(sp.text(c(16,-27.5),paste0(model_type," model"),cex = 1.5))+
    # eez
    latticeExtra::layer(sp.polygons(eez,col = "black",lwd = 1))+
    # 250m isobath
    latticeExtra::layer(sp.polygons(contours, col = "black", lwd = 1))+
    # sa coast
    latticeExtra::layer(sp.polygons(sa,col = "black",fill = "white",lwd= 1))+
    # points for main cities
    latticeExtra::layer(sp.points(places[c(1:3,5,6,18,20:22,10,14),],col = "black",pch = 20))+
    # coordinates and city names
    # done in three lines as a "pretty" position varies based on their place on the map
    latticeExtra::layer(sp.text(coordinates(places)[c(1:3,5,6),],places$Location[c(1:3,5,6)],col = "black",pch = 20,pos=4,cex = 0.5))+
    latticeExtra::layer(sp.text(coordinates(places)[c(18,20,21,22),],places$Location[c(18,20,21,22)],col = "black",pch = 20,pos=2,cex = 0.5))+
    latticeExtra::layer(sp.text(adjustedcoords,places$Location[c(10,14)],col = "black",pch = 20, pos=2,cex = 0.5))+
    # iucn extent
    if(exists("iucn_extent")){latticeExtra::layer(sp.polygons(iucn_extent,col = "red",lwd = 1.5))}
    
  # this saves the plot to a folder
  png(file=paste0(plotfolder,target,"_",model_type,"_res",res,"_continuous_ensemble.png"),width=3000, height=2000, res=300)
  print(plot)
  dev.off()
  rm(temp,plot) # remove unnecessary variables
  }

# ---------------------------------



