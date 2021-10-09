# ---------------------------------------------------------------------------------
######### Shark and ray species distribution models (SDMs) - raw plots script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: plots the data per species along with the IUCN range and expert ranges from expert mapping workshops
####

# ---------------------------------
# PACKAGES
# ---------------------------------
library(spatial)
library(sf)
library(raster)
library(stringr)
library(dplyr)

# ---------------------------------
# DIRECTORY
# ---------------------------------
#path =  "C:/Users/Administrator/"
path =  "/Users/nfb/"
setwd(paste0(path,"Dropbox/6-WILDOCEANS/Modelling"))

# ---------------------------------
# DATA
# ---------------------------------

# Occurrence files
files = list.files(pattern = ".rds", recursive = TRUE, path = "speciesdata/")

# EEZ polygon
eez = shapefile(list.files(pattern = "eez.shp", recursive = TRUE))

# Expert extents
load(list.files(pattern = "points.RData", recursive = TRUE))
expert_extent = points
colnames(expert_extent)[1] = "Scientific_name" # column name
expert_extent = as(expert_extent, Class = "Spatial")
expert_extent$Scientific_name = tolower(expert_extent$Scientific_name)
rm(points)


# ---------------------------------
# FORMATTING
# ---------------------------------

for(i in species_keep$species){
  # species data
  target = i # species name
  folder = "speciesdata/" # for now all data is species only, the other folder if "generadata/"
  source(list.files(pattern = "species_data.R", recursive = TRUE)) # finds script in directory
  rm(folder) # no longer needed
  
  # remove duplicates
  dups = duplicated(obs.data[c("LATITUDE","LONGITUDE", "DATE2")]) # verify duplicates (for latitude, longitude, and date)
  obs.data = obs.data[!dups,] # remove duplicates from data
  
  # geo-reference occurrences
  pts = SpatialPoints(obs.data[,c("LONGITUDE","LATITUDE")]) 
  
  # IUCN data
  exists3 = file.info(list.files(pattern = paste(target,".gpkg",sep=""), recursive = TRUE, ignore.case = TRUE))
  if(nrow(exists3 !=0)){iucn_extent = st_read(list.files(pattern = paste(target,".gpkg",sep=""), recursive = TRUE, ignore.case = TRUE))}
  rm(exists3)
  
  # plot and save each species as basic map
  tiff(paste("Outputs/maps_raw/",target,".tiff",sep=""), units="in", width=5, height=5,res = 300)
  plot.new() 
  plot(eez, main = paste(target))
  if(exists("iucn_extent")){plot(iucn_extent, add = TRUE, col = "blue")}
  expert = expert_extent[expert_extent$Scientific_name == tolower(target),]
  if(exists("expert")){plot(expert, add = TRUE, col = "green", pch = 16)}
  points(pts, cex = 0.5, pch = 16, col = "red")
  legend("topleft", legend=c("Data","IUCN range","Expert range"),
         cex=0.8, fill = c("red", "blue",'green'))
  dev.off()
  rm(species,obs.data,expert,iucn_extent,pts)
  }

