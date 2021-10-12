# set working directory
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Data/2-Cleaned_data/Point_data")
getwd()

# load packages
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(sf)
library(sp)
library(rgdal)
library(raster)

#####################  Load data

# list all csv files in cleaned folder (current directory)
temp = list.files(path = "/Users/nfb/Dropbox/6-WILDOCEANS/Data/2-Cleaned_data/Point_data",pattern="*.csv", recursive = TRUE)

# read them into a list
myfiles = lapply(temp, read.csv, header = TRUE)

# combine all files in one dataset
library(plyr)
summary = do.call(rbind.fill,myfiles)
detach("package:plyr")

# remove uneccesary variables
rm(myfiles, temp)

##################### Trim white space after scientific names
summary$Species_scientific = trimws(summary$Species_scientific, which = "both")

#####################  Format dates

# make sure all dates in the import spreadsheet are as dd/mm/yyyy
summary$Date_2 = as.POSIXct(summary$Date, format = "%d/%m/%Y")

##################### Add season

# extract month first
summary$month = month(summary$Date_2)

# convert to austral Winter (S,O,N,D,J,F) and Summer (M,A,M,J,J,A) based on month
summary = summary %>%
  mutate(Season = ifelse(is.na(Season) & month %in% c(9,10,11,12,1,2),"Summer", ifelse(is.na(Season) & month %in% c(3,4,5,6,7,8),"Winter",Season)))

##################### Add genus
summary$Genus = word(summary$Species_scientific, 1)

##################### Extract data with GPS locations only

# stick to species with lat and lon only
summary_2 = summary %>%
  filter(!is.na(Latitude))%>%
  filter(!is.na(Longitude))

rm(summary)

##################### Creat summary table
# this step is to get an idea of how many points are found per area

# convert summary data so spatial database
summary_2 = st_as_sf(summary_2, coords = c("Longitude", "Latitude"), crs = 4326 )

# load Ebert regions
regions = st_read("/Users/nfb/Dropbox/6-WILDOCEANS/Data/GIS/Ebert/ebert_regions.shp",crs = 4326)
colnames(regions)[2] = "Area"

# add region to summary data
summary_2 = st_join(summary_2,regions)

# create a summary table
# get total counts per species and counts per region
summary_table = 
  summary_2 %>%
  group_by(Species_scientific, Area)%>%
  summarise(count = n())

summary_table$geometry = NULL

summary_table = pivot_wider(summary_table,
            names_from = Area,
            values_from = count,
            values_fn = sum)

# turn Nas to 0, these are for points from land
# therefore they have no region assigned
summary_table[is.na(summary_table)] = 0

summary_table$count = rowSums(summary_table[, c(2, 3,4, 5)])

# load target species
targets = read.csv("/Users/nfb/Dropbox/6-WILDOCEANS/Data/consplan_target_spp.csv")
targets$target = "yes"

# join targets to summary table
summary_table = full_join(summary_table, targets)

# read in the master profile sheets
master = read.csv("/Users/nfb/Dropbox/6-WILDOCEANS/Data/2-Cleaned_data/Master_speciesprofiles_modified.csv")

# create species_scientific column
master$Species_scientific = with(master, paste(Genus,Species,sep = " "))

# join summary table with master profile
summary_table = as.data.frame(summary_table)
summary_table$Species_common = NULL
master_2 = left_join(master,summary_table)

# clean master file
master_2 = master_2 %>%
  filter(!is.na(count))

# write file
write.csv(master_2,"/Users/nfb/Dropbox/6-WILDOCEANS/Data/2-Cleaned_data/master_2.csv")
rm(summary_table,targets,regions,master,master_2)

##################### Creat geopackage with all the species

# extract species names (n = 199)
names = unique(summary_2$Species_scientific)

# create the geopackage with first species
temp = summary_2 %>%
  filter(Species_scientific == "Acroteriobatus annulatus")
temp_name = "Acroteriobatus annulatus"

st_write(temp, "/Users/nfb/Dropbox/6-WILDOCEANS/KZN_workshop/sharksandrays_all.gpkg", paste(temp_name))

# append geopackage with data from all other species
for(name in names){
  temp = summary_2 %>%
    filter(Species_scientific == name)
  temp_name = as.character(name)
  st_write(temp, "/Users/nfb/Dropbox/6-WILDOCEANS/KZN_workshop/sharksandrays_all.gpkg", paste(temp_name), append = TRUE)
}

##################### Create geopackage with all the cleaned IUCN distribution

# set working directory
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/KZN_workshop/IUCN_maps")
getwd()

# list all csv files in cleaned folder (current directory)
temp = list.files(path = "/Users/nfb/Dropbox/6-WILDOCEANS/KZN_workshop/IUCN_modified",pattern="*.shp", recursive = TRUE)

# read them into a list, they will be in sp class
myfiles = lapply(temp, shapefile)

# convert to sf class
myfiles = lapply(myfiles, st_as_sf)

# remove fid from all as this seems to cause an issue
# also turn all column names to lower
for(i in 1:length(myfiles)){
  myfiles[[i]]$fid = NULL
  colnames(myfiles[[i]]) = tolower(colnames(myfiles[[i]]))
}

# add missing binomial name
myfiles[[13]]$binomial = "Hemipristis_elongata"


# create the geopackage with first species
temp = myfiles[[1]]
temp_name = unique(temp$binomial)

st_write(temp, "/Users/nfb/Dropbox/6-WILDOCEANS/KZN_workshop/IUCN_modified_ebert.gpkg", paste(temp_name))

# append geopackage with data from all other species
for(i in 1:length(myfiles)){
  temp = myfiles[[i]]
  temp_name = unique(temp$binomial)[1]
  st_write(temp, "/Users/nfb/Dropbox/6-WILDOCEANS/KZN_workshop/IUCN_modified_ebert.gpkg", paste(temp_name),  append =TRUE)
}




