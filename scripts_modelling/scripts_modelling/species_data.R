# ---------------------------------------------------------------------------------
######### Shark and ray species distribution models (SDMs) - species_data script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: loads in occurrence data from both dataframes and shapefiles and formats it properly

detach(package:biomod2,unload=TRUE)

# ---------------------------------
# DATA
# ---------------------------------

# reads in data points of target species from specified folder
# the folder is specified in mainscript.R
files = list.files(path = paste0(path,"Dropbox/6-WILDOCEANS/Modelling/speciesdata"),pattern = paste(toupper(target),".rds",sep=""))
if(length(files)>0){
obs.data = readRDS(paste0(path,"Dropbox/6-WILDOCEANS/Modelling/",folder,toupper(target),".rds",sep="")) 
obs.data_poly = read.csv(list.files(pattern = "polygondata_clean.csv",recursive=TRUE, full.names = TRUE))

# ---------------------------------
# FORMATTING
# ---------------------------------

# extract data (if any) from the polygon file of target species
obs.data_poly$X = NULL
colnames(obs.data_poly) = toupper(colnames(obs.data_poly))
obs.data_poly$SPECIES_SCIENTIFIC = toupper(obs.data_poly$SPECIES_SCIENTIFIC) # turn to upper case
obs.data_poly = obs.data_poly %>%
  filter(SPECIES_SCIENTIFIC == target)

# Turn latitude and longitude to numeric
obs.data$LONGITUDE = as.numeric(obs.data$LONGITUDE)
obs.data$LATITUDE = as.numeric(obs.data$LATITUDE)

# add polygon data to main data
obs.data = full_join(obs.data,obs.data_poly)
rm(obs.data_poly) # remove polygon data

# verify there are no NAs in latitude and longitude
# if there are a warning message will appear
if(unique(!is.na(obs.data$LATITUDE)) == FALSE){
  print("SOME NAs ARE PRESENT IN THE LATITUDE DATA")}
if(unique(!is.na(obs.data$LONGITUDE)) == FALSE){
  print("SOME NAs ARE PRESENT IN THE LONGITUDE DATA")}

# remove 0 latitude and longitude
obs.data = obs.data %>%
  filter(LONGITUDE != 0) %>%
  filter(LATITUDE != 0)

# verify duplicates (for latitude, longitude, and date)
dups = duplicated(obs.data[c("LATITUDE","LONGITUDE", "DATE2")])
table(dups) # TRUE are number of duplicate records
print(table(dups)) # print number of duplicate records
obs.data = obs.data[!dups,] # remove duplicates from data
rm(dups) # remove variable from environment
}

