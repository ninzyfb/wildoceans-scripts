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
#path =  "C:/Users/Administrator/"
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

for(i in master$SPECIES_SCIENTIFIC){
  # species data
  target = i # species name
  folder = "speciesdata/" # for now all data is species only, the other folder if "generadata/"
  source(list.files(pattern = "species_data.R", recursive = TRUE)) # finds script in directory
  rm(folder) # no longer needed
  
  if(exists("obs.data")){
  # remove duplicates
  dups = duplicated(obs.data[c("LATITUDE","LONGITUDE", "DATE2")]) # verify duplicates (for latitude, longitude, and date)
  obs.data = obs.data[!dups,] # remove duplicates from data
  
  # geo-reference occurrences
  if(nrow(obs.data)>0){
  pts = SpatialPoints(obs.data[,c("LONGITUDE","LATITUDE")])}
  }
  # IUCN data
  exists3 = file.info(list.files(pattern = paste(target,".gpkg",sep=""), recursive = TRUE, ignore.case = TRUE))
  if(nrow(exists3 !=0)){iucn_extent = st_read(list.files(pattern = paste(target,".gpkg",sep=""), recursive = TRUE, ignore.case = TRUE))}
  rm(exists3)
  
  # extract data stats for that species
  temp = master %>%
    filter(SPECIES_SCIENTIFIC == i)
  
  # plot and save each species as basic map
  png(paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs/maps_raw/",target,".png",sep=""),width=3000, height=2000, res=300)
  plot.new() 
  plot(eez, main = paste(target,"\n ",temp$`Common name`))
  mtext(paste0("Data points = ",temp$abundance), adj = 0.8, padj = 40)
  mtext(paste0("Cells with presence at 5km resolution = ",temp$cells," (",temp$rounded,"%)"), adj = 0.8, padj = 42)
  mtext(paste0("Cells with presence at 10km resolution = ",temp$cells_10," (",temp$rounded_10,"%)"), adj = 0.8, padj = 44)
  if(exists("iucn_extent")){plot(iucn_extent, add = TRUE, col = "blue")}
  expert = expert_extent[expert_extent$Scientific_name == tolower(target),]
  if(exists("pts")){points(pts, cex = 0.5, pch = 16, col = "red")}
  if(exists("expert")){plot(expert, add = TRUE, col = "green", pch = 16)}
  legend("topleft", legend=c("Data","IUCN range","Expert range"),
         cex=1, fill = c("red", "blue",'green'))
  dev.off()
  rm(species,obs.data,expert,iucn_extent,pts)
  }

