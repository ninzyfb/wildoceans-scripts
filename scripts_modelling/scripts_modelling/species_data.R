# ---------------------------------------------------------------------------------
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script loads the occurrence data and formats it properly
# this script also adds season to the dataset
# Summer months: Sep,Oct,Nov,Dec,Jan,Feb
# Winter months: Mar,Apr,May,Jun,Jul,Aug
# ---------------------------------


# ---------------------------------
# PACKAGES
# ---------------------------------
# detach biomod2 if attached, it seems to mess with other packages i.e. tidyr
if("biomod2" %in% (.packages())){detach("package:biomod2", unload=TRUE) }
# ---------------------------------


# ---------------------------------
# DATA INPUT
# ---------------------------------
# read in occurrences of target species
# the target is specified from the master sheet in the parent script
files = list.files(path = paste0(path,"Dropbox/6-WILDOCEANS/Modelling/speciesdata"),pattern = paste(toupper(target),".rds",sep=""))
# ---------------------------------


# ---------------------------------
# FORMATTING
# ---------------------------------
# not all species from master sheet have data
# this if statement runs the script only if the target species has associated data
if(length(files)>0){ 
  
# load in data file (point and polygon formats)
obs.data = readRDS(paste0(path,"Dropbox/6-WILDOCEANS/Modelling/",folder,toupper(target),".rds",sep="")) 

# convert latitude and longitude to numeric variables
obs.data$LONGITUDE = as.numeric(obs.data$LONGITUDE)
obs.data$LATITUDE = as.numeric(obs.data$LATITUDE)

# this if statement further ensures that the script should only by run if the dataframe has any data
if(nrow(obs.data)>0){

  # verify there are no NAs in latitude and longitude
# if there are a warning message will appear
if(unique(!is.na(obs.data$LATITUDE)) == FALSE){
  print("SOME NAs ARE PRESENT IN THE LATITUDE DATA")}
if(unique(!is.na(obs.data$LONGITUDE)) == FALSE){
  print("SOME NAs ARE PRESENT IN THE LONGITUDE DATA")}

# remove any 0 latitude and longitude
obs.data = obs.data %>%
  filter(LONGITUDE != 0) %>%
  filter(LATITUDE != 0)

# verify duplicates (for latitude, longitude, and date)
dups = duplicated(obs.data[c("LATITUDE","LONGITUDE", "DATE")])
# print number of duplicate records, TRUE are number of duplicates
print(table(dups))
# remove duplicates from data
obs.data = obs.data[!dups,]
rm(dups)
# save number of occurrence points as variable
abundance = nrow(obs.data)}else{length(files) = 0}}

# add month variable from Date
obs.data = obs.data %>%
  mutate(Month = month(obs.data$DATE))

# group observations by season
# important: some datasets came with season already specified
obs.data = obs.data %>%
  # if SEASON variable is empty, convert to NA
  mutate(SEASON = ifelse(SEASON == "",NA,SEASON)) %>%
  # specify winter months
  mutate(SEASON = ifelse(is.na(SEASON) & Month %in% c(3,4,5,6,7,8),"Winter",
                       # specify summer months
                       ifelse(is.na(SEASON) & Month %in% c(9,10,11,12,1,2), "Summer",
                              # if season was stated as autumn then turn to winter
                              ifelse(SEASON == "Autumn","Winter",
                                     # if season was stated as spring then turn to summer
                                     ifelse(SEASON == "Spring","Summer",SEASON)))))

# convert data to spatial points data frame
coordinates(obs.data) =  ~ cbind(obs.data$LONGITUDE,obs.data$LATITUDE)

# set CRS of observations
crs(obs.data) = crs(stack_subset)
# ---------------------------------