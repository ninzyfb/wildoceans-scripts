# set working directory
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling")

# packages
library(sf)
library(readxl)
library(dplyr)
library(rgdal)

# data
places = read.csv(list.files(pattern = "ebert_placenames.csv",recursive = TRUE)) # locations shapefile
species_extents = read_xlsx(list.files(pattern = "species_extents",recursive = TRUE))

# turn all string to upper case to prevent mismatching strings
places$Location = toupper(places$Location)
species_extents$Boundary_west = toupper(species_extents$Boundary_west)
species_extents$Boundary_east = toupper(species_extents$Boundary_east)

# join coordinates from places to species extents
# western boundary
join1 = left_join(species_extents,places, by = c("Boundary_west" = "Location"))
colnames(join1)[colnames(join1) == "Latitude"] = "Lat_west"
colnames(join1)[colnames(join1) == "Longitude"] = "Lon_west"

# eastern boundary
join1 = left_join(join1,places, by = c("Boundary_east" = "Location"))
colnames(join1)[colnames(join1) == "Latitude"] = "Lat_east"
colnames(join1)[colnames(join1) == "Longitude"] = "Lon_east"

# Filter to only keep species with boundaries
join1 = join1 %>%
  filter(!is.na(Boundary_west))%>%
  filter(!is.na(Boundary_east))%>%
  filter(!is.na(Lat_west))%>%
  filter(!is.na(Lat_east))

rm(places,species_extents,look)

# convert to spatial points
west = join1[,c(5,4)]
east = join1[,c(7,6)]
colnames(west) = c("Lon","Lat")
colnames(east) = c("Lon","Lat")

list = list()
for(i in 1:nrow(join1)){
  west_temp = west[i,]
  east_temp = east[i,]
  coords = rbind(west_temp,east_temp)
  list[[i]] = coords
}

# add data and coordinates in point data frame
points = SpatialMultiPointsDataFrame(list,join1[,c(1:3)])
# convert to sf object
points = st_as_sf(points)
# save
save(points,file = "points.RData")


