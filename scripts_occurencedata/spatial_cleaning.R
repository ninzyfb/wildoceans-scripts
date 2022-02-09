# load packages
library(sf)

# read data
inaturalist = read.csv("inaturalist_clean.csv")

# turn to spatial data
inaturalist = st_as_sf(inaturalist, coords = c("Longitude", "Latitude"), crs = 4326)

# load Ebert regions
regions = st_read("/Users/nfb/Dropbox/6-WILDOCEANS/Data/GIS/Ebert/ebert_regions.shp",crs = 4326)
colnames(regions)[2] = "Area"

# clip gbif to regions
# this prevents data on land from being included
inaturalist_clean = st_intersection(inaturalist,regions)
inaturalist_clean$Area = as.factor(inaturalist_clean$Area)
table(inaturalist_clean$Area)
gbif_clean = gbif_clean %>%
  filter(!is.na(Area))

# export data
write.csv("gbif_clean")
