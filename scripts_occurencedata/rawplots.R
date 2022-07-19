# ---------------------------------------------------------------------------------
# AUTHOR: Nina Faure Beaulieu (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script plots the raw data per species
# along with the IUCN range and expert ranges from expert mapping workshops
# All SA sharks and rays distributions were downloaded from IUCN, saved as sharks_saonly_updated.shp
# link on how to split IUCN shapefile of all sharks and rays into individual files
# https://gis.stackexchange.com/questions/25709/splitting-shapefile-into-separate-files-for-each-feature-using-qgis-or-saga
# ---------------------------------


# ---------------------------------
# PACKAGES
# ---------------------------------
# list of required packages
requiredpackages = c("lubridate","sf","dplyr","raster","stringr","spatial","readxl","rgeos","gridExtra","latticeExtra","rasterVis")
# load packages
lapply(requiredpackages,require, character.only = TRUE)
rm(requiredpackages)
# ---------------------------------


# ---------------------------------
# DIRECTORY
# ---------------------------------
path =  "/Users/nfb/"
my.directory = paste0(path,"Dropbox/6-WILDOCEANS")
setwd(my.directory)
# ---------------------------------


# ---------------------------------
# DATA
# ---------------------------------
# list of species
master = read_xlsx(list.files(pattern = "data_summary_master.xlsx", recursive = TRUE,full.names = TRUE)[1])
# Occurrence file names
files = list.files(pattern = ".rds", recursive = TRUE, path = paste0(path,"Dropbox/6-WILDOCEANS/ConservationPlan/Modelling/speciesdata/"),full.names = TRUE)
# ---------------------------------


# ---------------------------------
#  - ENVIRONMENTAL VARIABLES 
# ---------------------------------
# specify resolution (5 or 10 km2)
# not actually important for this script, but needed to load environmental variable stack
res = 10
source(list.files(pattern = "envnt_variable_stack.R", recursive = TRUE, full.names = TRUE))
# ---------------------------------


# ---------------------------------
# PLOTTING PARAMETERS
# ---------------------------------
source(list.files(pattern = "plottingparameters.R", recursive = TRUE, full.names= TRUE))

# Title box (top centre)
x_coord <- c(31,  19,  19, 31, 31)
y_coord <- c(-27, -27, -28, -28, -27)
xym <- cbind(x_coord, y_coord)
p = Polygon(xym)
ps = Polygons(list(p),1)
sps3 = SpatialPolygons(list(ps))

# Stats box (bottom right)
x_coord <- c(36.6,  30,  30, 36.6, 36.6)
y_coord <- c(-38.3, -38.3, -37, -37, -38.3)
xym <- cbind(x_coord, y_coord)
p = Polygon(xym)
ps = Polygons(list(p),1)
sps2 = SpatialPolygons(list(ps))
# ---------------------------------


# ---------------------------------
# PLOTTING (INDIVIDUAL PLOTS)
# ---------------------------------

# this loops plots a raw map for each species with data
for(i in 1:nrow(master)){
  # species name
  target = master$SPECIES_SCIENTIFIC[i]
  # folder with occurrence data files (all stored as R object)
  folder = "speciesdata/"
  # run script to load species data
  exampledata = "no"
  source(list.files(pattern = "species_data.R", recursive = TRUE)) 
  rm(folder)
  temp_raster = blank_template
  values(temp_raster) = 0
  
  # only continue with loop if there is data for species
  if(exists("obs.data")){
    if(nrow(obs.data)>0){
    values(temp_raster)[raster::extract(temp_raster,obs.data,cell=TRUE)[,1]] = 1
    values(temp_raster)[values(temp_raster)==0]= NA
    temp_polygon = rasterToPolygons(temp_raster)}}
    
  # IUCN data
  exists3 = file.info(list.files(pattern = paste(target,".gpkg",sep=""), recursive = TRUE, ignore.case = TRUE))
  if(nrow(exists3 !=0)){
    iucn_extent = st_read(list.files(pattern = paste(target,".gpkg",sep=""), recursive = TRUE, ignore.case = TRUE))
    iucn_extent = as(iucn_extent, Class = "Spatial")
    }else{rm(iucn_extent)}
  rm(exists3)
  
  # SPECIES INFO
  TARGET = target
  temp = master %>%
    dplyr::filter(SPECIES_SCIENTIFIC == TARGET)
  
  # EXPERT RANGE
  expert = expert_extent[expert_extent$Scientific_name == tolower(target),]
  # if expert range exists then convert to spatial object
  if(nrow(coordinates(expert))>0){expert = SpatialPoints(expert)}else{rm(expert)}
  
  # species info
  spp_info = master %>%
    filter(SPECIES_SCIENTIFIC == TARGET)
  
  # convert target to sentence case for plotting
  target = str_to_sentence(master$SPECIES_SCIENTIFIC[i])
  
  # get endemic status
  if(spp_info$ENDEMIC.STATUS == 1){endemism = "South Africa"}
  if(spp_info$ENDEMIC.STATUS == 0){endemism = "Not endemic"}
  if(spp_info$ENDEMIC.STATUS == 2){endemism = "Southern Africa"}
  
  # plot and save each species as basic map
  # save the map in different folder depending on if this species will be modeled or not
  # that means based on its 10km resolution prevalence score which needs to be >1
  png(paste0(path,"Dropbox/6-WILDOCEANS/ConservationPlan/Modelling/Outputs/maps_raw/",target,".png",sep=""),width=3000, height=2000, res=300)
  plot.new() 
  plot = levelplot(temp_raster,
            margin = FALSE,
            colorkey=FALSE,
            #col.regions = "white",
            xlab = NULL,
            ylab=NULL,
            #main = bquote(italic(.(target))~","~.(spp_info$Species_common)~" - ["~.(spp_info$STATUS)~"] - ["~.(endemism)~"]"),
            #main = paste(str_to_sentence((target)),"-",temp$Species_common),
            # add blue line IUCN legend
            panel = function(x,y,...){
              panel.points(x=15.5, y=-27.5, col = "steelblue",fill = "lightblue",lwd = 1, pch = 21)
              panel.points(x=15.5, y=-27.8, col = "green",lwd = 2,cex=1, pch = 4)
              panel.points(x=15.5, y=-28.1, col = "indianred",fill = "indianred1",lwd = 1, pch = 21)})+
    latticeExtra::layer(sp.text(c(14.5,-27.5),paste0("IUCN range"),cex = 0.6))+
    latticeExtra::layer(sp.text(c(14.5,-27.8),paste0("Expert range"),cex = 0.6))+
    latticeExtra::layer(sp.text(c(14.5,-28.1),paste0("Data"),cex = 0.6))+
    # iucn extent
    if(exists("iucn_extent")){
        # IUCN range
        latticeExtra::layer(sp.polygons(iucn_extent,col = "steelblue",fill = "lightblue",lwd = 1))+
        # occurrence points
        latticeExtra::layer(sp.polygons(temp_polygon,col = "indianred",fill = "indianred1",cex = 0.4, pch = 21,lwd = 1.5))+
        # mpa outline
        latticeExtra::layer(sp.polygons(mpas,col = "black",lwd = 0.5))+
        # eez
        latticeExtra::layer(sp.polygons(eez,col = "black",lwd = 1))+
        # sa coast
        latticeExtra::layer(sp.polygons(sa,col = "black",lwd= 0.5, fill = "white"))+
        # points for main cities
        latticeExtra::layer(sp.points(places[c(1:3,5,6,18,20:22,10,14),],col = "black",pch = 20))+
        # coordinates and city names
        # done in three lines as a "pretty" position varies based on their place on the map
        latticeExtra::layer(sp.text(coordinates(places)[c(1:3,5,6),],places$Location[c(1:3,5,6)],col = "black",pch = 20,pos=4,cex = 0.5))+
        latticeExtra::layer(sp.text(coordinates(places)[c(18,20,21,22),],places$Location[c(18,20,21,22)],col = "black",pch = 20,pos=2,cex = 0.5))+
        latticeExtra::layer(sp.text(adjustedcoords,places$Location[c(10,14)],col = "black",pch = 20, pos=2,cex = 0.5))+
        # expert range (only plot if it exists) or else just carry on with rest of plot
        if(exists("expert")){
          latticeExtra::layer(sp.points(expert,col = "green",cex = 1, pch = 4,lwd = 2))+
            # box on bottom right with scenario stats
            latticeExtra::layer(sp.polygons(sps2, fill = "white"))+
            latticeExtra::layer(sp.text(c(30.2,-37.3),paste0("Total unique occurrence points = ",temp$abundance),col = "black",pch = 20, cex = 0.7,adj=0))+
            latticeExtra::layer(sp.text(c(30.2,-37.8),paste0("Total 10 x 10km cells = ",temp$cells_10," (",round(temp$prevalence_10*100,2),"%)"),pch = 20,cex = 0.7,adj=0))+
            # box on top for title
            latticeExtra::layer(sp.polygons(sps3, fill = "white"))+latticeExtra::layer(sp.text(c(20,-27.3),bquote(italic(.(target))~","~.(spp_info$Species_common)),col = "black",pch = 20, cex = 1,adj=0))+
            latticeExtra::layer(sp.text(c(20,-27.7),"Red List status:",col = "black",pch = 20,font = 2,cex = 1,adj=0))+
            latticeExtra::layer(sp.text(c(23.2,-27.7),bquote(.(spp_info$STATUS)),col = "black",pch = 20, cex = 1,adj=0))+
            latticeExtra::layer(sp.text(c(23.9,-27.7),"- Endemic status:",col = "black",pch = 20,font = 2,cex = 1,adj=0))+
            latticeExtra::layer(sp.text(c(27.5,-27.7),bquote(.(endemism)),col = "black",pch = 20, cex = 1,adj=0))
          }else{# box on bottom right with scenario stats
            latticeExtra::layer(sp.polygons(sps2, fill = "white"))+
              latticeExtra::layer(sp.text(c(30.2,-37.3),paste0("Total unique occurrence points = ",temp$abundance),col = "black",pch = 20, cex = 0.7,adj=0))+
              latticeExtra::layer(sp.text(c(30.2,-37.8),paste0("Total 10 x 10km cells = ",temp$cells_10," (",round(temp$prevalence_10*100,2),"%)"),pch = 20,cex = 0.7,adj=0))+
              # box on top for title
              latticeExtra::layer(sp.polygons(sps3, fill = "white"))+latticeExtra::layer(sp.text(c(20,-27.3),bquote(italic(.(target))~","~.(spp_info$Species_common)),col = "black",pch = 20, cex = 1,adj=0))+
              latticeExtra::layer(sp.text(c(20,-27.7),"Red List status:",col = "black",pch = 20,font = 2,cex = 1,adj=0))+
              latticeExtra::layer(sp.text(c(23.2,-27.7),bquote(.(spp_info$STATUS)),col = "black",pch = 20, cex = 1,adj=0))+
              latticeExtra::layer(sp.text(c(23.9,-27.7),"- Endemic status:",col = "black",pch = 20,font = 2,cex = 1,adj=0))+
              latticeExtra::layer(sp.text(c(27.5,-27.7),bquote(.(endemism)),col = "black",pch = 20, cex = 1,adj=0))
            
          }}else{
                  # occurrence data
                  latticeExtra::layer(sp.polygons(temp_polygon,col = "indianred",fill = "indianred1",cex = 0.4, pch = 21))+
                    # mpa outline
                    latticeExtra::layer(sp.polygons(mpas,col = "black",lwd = 0.5))+
                    # eez
                    latticeExtra::layer(sp.polygons(eez,col = "black",lwd = 1))+
                    # sa coast
                    latticeExtra::layer(sp.polygons(sa,col = "black",lwd= 0.5, fill = "white"))+
                    # points for main cities
    latticeExtra::layer(sp.points(places[c(1:3,5,6,18,20:22,10,14),],col = "black",pch = 20))+
    # coordinates and city names
    # done in three lines as a "pretty" position varies based on their place on the map
    latticeExtra::layer(sp.text(coordinates(places)[c(1:3,5,6),],places$Location[c(1:3,5,6)],col = "black",pch = 20,pos=4,cex = 0.5))+
    latticeExtra::layer(sp.text(coordinates(places)[c(18,20,21,22),],places$Location[c(18,20,21,22)],col = "black",pch = 20,pos=2,cex = 0.5))+
    latticeExtra::layer(sp.text(adjustedcoords,places$Location[c(10,14)],col = "black",pch = 20, pos=2,cex = 0.5))+
    # expert range (only plot if it exists) or else just carry on with rest of plot
    if(exists("expert")){latticeExtra::layer(sp.points(expert,col = "green",lwd = 2,cex=1,pch = 4))+
        # box on bottom right with scenario stats
        latticeExtra::layer(sp.polygons(sps2, fill = "white"))+
        latticeExtra::layer(sp.text(c(30.2,-37.3),paste0("Total unique occurrence points = ",temp$abundance),col = "black",pch = 20, cex = 0.7,adj=0))+
        latticeExtra::layer(sp.text(c(30.2,-37.8),paste0("Total 10 x 10km cells = ",temp$cells_10," (",round(temp$prevalence_10*100,2),"%)"),pch = 20,cex = 0.7,adj=0))+
        # box on top for title
        latticeExtra::layer(sp.polygons(sps3, fill = "white"))+latticeExtra::layer(sp.text(c(20,-27.3),bquote(italic(.(target))~","~.(spp_info$Species_common)),col = "black",pch = 20, cex = 1,adj=0))+
        latticeExtra::layer(sp.text(c(20,-27.7),"Red List status:",col = "black",pch = 20,font = 2,cex = 1,adj=0))+
        latticeExtra::layer(sp.text(c(23.2,-27.7),bquote(.(spp_info$STATUS)),col = "black",pch = 20, cex = 1,adj=0))+
        latticeExtra::layer(sp.text(c(23.9,-27.7),"- Endemic status:",col = "black",pch = 20,font = 2,cex = 1,adj=0))+
        latticeExtra::layer(sp.text(c(27.5,-27.7),bquote(.(endemism)),col = "black",pch = 20, cex = 1,adj=0))}else{
          # box on bottom right with scenario stats
          latticeExtra::layer(sp.polygons(sps2, fill = "white"))+
            latticeExtra::layer(sp.text(c(30.2,-37.3),paste0("Total unique occurrence points = ",temp$abundance),col = "black",pch = 20, cex = 0.7,adj=0))+
            latticeExtra::layer(sp.text(c(30.2,-37.8),paste0("Total 10 x 10km cells = ",temp$cells_10," (",round(temp$prevalence_10*100,2),"%)"),pch = 20,cex = 0.7,adj=0))+
            # box on top for title
            latticeExtra::layer(sp.polygons(sps3, fill = "white"))+latticeExtra::layer(sp.text(c(20,-27.3),bquote(italic(.(target))~","~.(spp_info$Species_common)),col = "black",pch = 20, cex = 1,adj=0))+
            latticeExtra::layer(sp.text(c(20,-27.7),"Red List status:",col = "black",pch = 20,font = 2,cex = 1,adj=0))+
            latticeExtra::layer(sp.text(c(23.2,-27.7),bquote(.(spp_info$STATUS)),col = "black",pch = 20, cex = 1,adj=0))+
            latticeExtra::layer(sp.text(c(23.9,-27.7),"- Endemic status:",col = "black",pch = 20,font = 2,cex = 1,adj=0))+
            latticeExtra::layer(sp.text(c(27.5,-27.7),bquote(.(endemism)),col = "black",pch = 20, cex = 1,adj=0))
        }}
  print(plot)
  dev.off()
  rm(species,obs.data,expert,iucn_extent,pts)}
# ---------------------------------