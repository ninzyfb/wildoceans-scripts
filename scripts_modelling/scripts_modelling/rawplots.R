# ---------------------------------------------------------------------------------
######### Shark and ray species distribution models (SDMs) - raw plots script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: plots the data per species along with the IUCN range and expert ranges from expert mapping workshops
####

# link on how to split IUCN shapefile of all sharks and rays into individual files
# https://gis.stackexchange.com/questions/25709/splitting-shapefile-into-separate-files-for-each-feature-using-qgis-or-saga
# All SA sharks and rays distributions were downloaded from IUCN, saved as sharks_saonly_updated.shp

# ---------------------------------
# PACKAGES
# ---------------------------------
library(spatial)
library(sf)
library(raster)
library(stringr)
library(dplyr)
library(readxl)

# ---------------------------------
# DIRECTORY
# ---------------------------------
path =  "/Users/nfb/"
setwd(paste0(path,"Dropbox/6-WILDOCEANS"))

# ---------------------------------
# DATA
# ---------------------------------

# list of species
master = read_xlsx(list.files(pattern = "data_summary_master.xlsx", recursive = TRUE,full.names = TRUE))

# Occurrence files
files = list.files(pattern = ".rds", recursive = TRUE, path = paste0(path,"Dropbox/6-WILDOCEANS/Modelling/speciesdata/"),full.names = TRUE)

# EEZ polygon
eez = shapefile(list.files(pattern = "eez.shp", recursive = TRUE, full.names = TRUE))

# Expert extents
load(list.files(pattern = "points.RData", recursive = TRUE, full.names = TRUE))
expert_extent = points
colnames(expert_extent)[1] = "Scientific_name" # column name
expert_extent = as(expert_extent, Class = "Spatial")
expert_extent$Scientific_name = tolower(expert_extent$Scientific_name)
rm(points)

# ---------------------------------
# FORMATTING
# ---------------------------------

# this loops plots a raw map for each species with data
for(i in master$SPECIES_SCIENTIFIC){
  # species data
  target = i # species name
  folder = "speciesdata/" # for now all data is species only, the other folder if "generadata/"
  source(list.files(pattern = "species_data.R", recursive = TRUE)) # finds script in directory
  rm(folder) # no longer needed
  
  if(nrow(obs.data)>0){
  # IUCN data
  exists3 = file.info(list.files(pattern = paste(target,".gpkg",sep=""), recursive = TRUE, ignore.case = TRUE))
  if(nrow(exists3 !=0)){iucn_extent = st_read(list.files(pattern = paste(target,".gpkg",sep=""), recursive = TRUE, ignore.case = TRUE))}
  rm(exists3)
  
  # my binary data
  exists4 = file.info(list.files(path ="Modelling/Outputs/sdms/",pattern = paste(target,"_Aseasonal_res10_ensemblemeanthreshold",sep=""), recursive = TRUE, ignore.case = TRUE,full.names = TRUE))
  if(nrow(exists4 !=0)){binarymap = raster(list.files(path ="Modelling/Outputs/sdms/",pattern = paste(target,"_Aseasonal_res10_ensemblemeanthreshold",sep=""), recursive = TRUE, ignore.case = TRUE,full.names = TRUE))}
  rm(exists4)
  
  # extract data stats for that species
  temp = master %>%
    filter(SPECIES_SCIENTIFIC == i)
  
  # plot and save each species as basic map
  # save the map in different folder depending on if this species will be modeled or not
  # that means based on its 10km resolution prevalence score which needs to be >1
  if(temp$rounded_10>0){png(paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs/maps_raw/",target,".png",sep=""),width=3000, height=2000, res=300)}else{png(paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs/maps_raw_unmodelled/",target,".png",sep=""),width=3000, height=2000, res=300)}
  plot.new() 
  plot(eez, main = paste(target,"\n ",temp$Species_common))
  mtext(paste0("Data points = ",temp$abundance), adj = 0.8, padj = 40)
  mtext(paste0("Cells with presence at 5km resolution = ",temp$cells," (",temp$rounded,"%)"), adj = 0.8, padj = 42)
  mtext(paste0("Cells with presence at 10km resolution = ",temp$cells_10," (",temp$rounded_10,"%)"), adj = 0.8, padj = 44)
  if(exists("binarymap")){plot(binarymap, legend = FALSE)}
  if(exists("iucn_extent")){plot(iucn_extent, add = TRUE, col = "transparent", border= "blue")}
  expert = expert_extent[expert_extent$Scientific_name == tolower(target),]
  if(exists("obs.data")){points(obs.data, cex = 0.5, pch = 16, col = "red")}
  if(exists("expert")){plot(expert, add = TRUE, col = "green", pch = 16)}
  legend("topleft", legend=c("Data","IUCN range","Expert range"),
         cex=1, fill = c("red", "blue",'green'))
  dev.off()
  rm(species,obs.data,expert,iucn_extent,pts,binarymap)
  }}

