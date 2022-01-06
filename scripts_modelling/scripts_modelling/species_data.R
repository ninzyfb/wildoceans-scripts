# ---------------------------------------------------------------------------------
######### Shark and ray species distribution models (SDMs) - species_data script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: loads in occurrence data and formats it properly
#IMPORTANT! THIS SCRIPT NEEDS TO BE RUN FROM THE MAIN SCRIPT WHICH SPECIFIES TARGET SPECIES
####

# detach biomod2 if attached as it messes with other packages i.e. tidyr
if("biomod2" %in% (.packages())){
  detach("package:biomod2", unload=TRUE) 
}
# ---------------------------------
# DATA
# ---------------------------------

# reads in data points of target species from specified folder
files = list.files(path = paste0(path,"Dropbox/6-WILDOCEANS/Modelling/speciesdata"),pattern = paste(toupper(target),".rds",sep=""))
if(length(files)>0){ # if species has associated data run rest of script
  # load in data file (point and polygon formats)
obs.data = readRDS(paste0(path,"Dropbox/6-WILDOCEANS/Modelling/",folder,toupper(target),".rds",sep="")) 
obs.data_poly = read.csv(list.files(pattern = "polygondata_clean.csv",recursive=TRUE, full.names = TRUE))

# ---------------------------------
# FORMATTING
# ---------------------------------

# extract data (if any) from the polygon file of target species
obs.data_poly$X = NULL
colnames(obs.data_poly) = toupper(colnames(obs.data_poly))
obs.data_poly$SPECIES_SCIENTIFIC = toupper(obs.data_poly$SPECIES_SCIENTIFIC) # turn to upper case
# if species has no associated polygon data then this will simplY become an empty data frame
obs.data_poly = obs.data_poly %>%
  filter(SPECIES_SCIENTIFIC == target)

# Turn latitude and longitude to numeric
obs.data$LONGITUDE = as.numeric(obs.data$LONGITUDE)
obs.data$LATITUDE = as.numeric(obs.data$LATITUDE)

# add polygon data to main data
obs.data = full_join(obs.data,obs.data_poly)
rm(obs.data_poly) # remove polygon data

if(nrow(obs.data)>0){
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
print(table(dups)) # print number of duplicate records, TRUE are number of duplicates
obs.data = obs.data[!dups,] # remove duplicates from data
rm(dups) # remove variable from environment
abundance = nrow(obs.data)

}else{length(files) = 0}}

