# packages
library(sf)

# Expert extents
#load(list.files(pattern = "points.RData", recursive = TRUE))
#expert_extent = points
places = read.csv(list.files(pattern = "placenames.csv", recursive = TRUE))
coordinates(places) =  ~ cbind(places$Longitude,places$Latitude) # convert to spatial points dataframe
places = st_as_sf(places)

# Load species data
#target = "Mustelus mustelus" # species name 
#source(list.files(pattern = "species_data.R", recursive = TRUE)) # species data script
coordinates(obs.data) =  ~ cbind(obs.data$LONGITUDE,obs.data$LATITUDE) # convert to spatial points dataframe

# Set limit for species based on expert advice
# i.e. Port Shepstone for Mustelus mustelus
boundary_location = "Port Shepstown"
limit = places %>%
  filter(Location == boundary_location)

# filter obs data as required 
obs.data = st_as_sf(obs.data)
obs.data = obs.data %>%
  filter(LONGITUDE<limit$Longitude)
obs.data$geometry = NULL

rm(boundary_location,places,limit)
