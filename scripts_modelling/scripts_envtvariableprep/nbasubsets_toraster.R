# Packages
library(sf)
library(dplyr)
library(raster)
library(eply)

# Data
getwd()
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/environmental_variables/NationalBiodiversityAssessment_2018/nba_shapefiles")
files = list.files(pattern = ".shp", recursive = TRUE)
files = files[c(6,8,9,10,12,13)]

# create list with separate entry for each file
list = list()
for(i in files){
  temp = st_read(i)
  list[[i]] = temp
}
rm(i)

# convert each to a raster

for(i in 1:length(list)){
  temp = list[[i]] # extract one variable
  variable = names(temp)[1] # simplify geometries by grouping by variable and simplifying
  temp = temp %>%
    group_by_(variable)%>%
    summarise()
  r = raster(extent(temp), resolution = 1000, crs = crs(temp)) # create empty raster (1km res)
  r_2 = rasterize(temp, r) # fill with variable
  a = str_split(files[i],pattern = "_")[[1]][2] # extract variable name
  a = str_split(a,pattern = ".shp")[[1]][1]
  writeRaster(r_2, paste0(names(temp)[1],a,".tif",sep="")) # save raster with variable name
}

# convert each to correct resolution and extent
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling")
template = raster(list.files(pattern = "OSTIA_SST_5KM_20070102.tif", recursive = TRUE))

# simplify values (not necessary, but just for me)
values(template)[!is.na(values(template))] = 1

#list files
files = list.files("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/environmental_variables/NationalBiodiversityAssessment_2018/nba_rasters_subset") 
path = "/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/environmental_variables/NationalBiodiversityAssessment_2018/nba_rasters_subset/"

# transform
for(name in files){
  temp = raster(paste(path, name, sep="")) # load raster
  temp_modified = projectRaster(temp,template, method = 'ngb') # project the raster to the template
  writeRaster(temp_modified,paste(path,name, sep=""), overwrite = TRUE) # write raster
  a = crs(temp_modified)
  print(a)
}


