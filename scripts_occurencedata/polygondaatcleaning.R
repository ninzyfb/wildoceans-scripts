# ---------------------------------------------------------------------------------
######### Shark and ray data cleaning - polygon cleaning script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: This script extract coordinates and date when applicable that overlap with data polygons
####

# ---------------------------------
# PACKAGES
# ---------------------------------
library(sf)
library(dplyr)
library(data.table)
library(raster)

# ---------------------------------
# DIRECTORY AND DATA
# ---------------------------------
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling") # all the subscripts are in the scripts folder
template = raster(list.files(pattern = "template.tif", recursive = TRUE))

# ---------------------------------
# DIRECTORY
# ---------------------------------
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/OccurenceData/2-Cleaned_data/Polygon_data")

# ---------------------------------
# FORMATTING
# ---------------------------------

files = list.files(pattern = ".shp") # list polygon files
data_list = list() # empty list
count = 1 # count to add dataframes to list

# this big loop loads in every shapefile one by one
# for every shapefile it checks if there is date information
# it then extract coordinates for each polygon and conserves any date information when applicable
for(i in 1:length(files)){
  temp = st_read(files[i]) # read in first shapefile
  if("Date" %in% names(temp)){ # if the polygons have a date
    sp = unique(temp$Spcs_sc) # get all species names
    dates = unique(temp$Date)
    for(date in dates){ # for each date
      temp2 = temp %>%
        filter(Date == date)
    for(name in sp){ # for each species
      temp3 = temp2 %>% # filter to only keep one species
        filter(Spcs_sc == name)
      if(nrow(temp3)>0){ # some species names have no data associated with them
        temp3 = mask(template,temp3) # get cells overlaid by polygon for that species
        cells = which(values(temp3)==1) # extract cells
        xy = xyFromCell(template,cells) # extract coordinates
        if(length(xy)>0){ # if there are coordinates
          data_temp = as.data.frame(xy) # turn to dataframe
          data_temp$species_scientific = name # add species name
          data_temp$Date = date # add date
          data_list[[count]] = data_temp # add to list
          count = count + 1}}}}
  }else{
  sp = unique(temp$Spcs_sc) # get all species names
  for(name in sp){ # for each species
    temp2 = temp %>% # filter to only keep one species
      filter(Spcs_sc == name)
    if(nrow(temp2)>0){ # some species names have no data associayed with them
      temp2 = mask(template,temp2) # get cells overlaid by polygon for that species
      cells = which(values(temp2)==1) # extract cells
      xy = xyFromCell(template,cells) # extract coordinates
      if(length(xy)>0){ # if there are coordinates
        data_temp = as.data.frame(xy) # turn to dataframe
        data_temp$species_scientific = name # add species name
        data_list[[count]] = data_temp # add to list
        count = count + 1}}}}} # increase count by one

# combine all dataframe together
df = bind_rows(data_list) 
colnames(df)[1:2] = c("Longitude","Latitude") # change column names
df = df %>%
  filter(species_scientific != 0)

# ---------------------------------
# WRITING
# ---------------------------------
write.csv(df,"/Users/nfb/Dropbox/6-WILDOCEANS/OccurenceData/2-Cleaned_data/polygondata_clean.csv")
