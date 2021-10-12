# before running this script you need to expert_mappingprep script

# package
library(sf)

# load huge data set
iucn = st_read("/Users/nfb/Downloads/IUCN_redlistdownload/data_0.shp",crs = 4326)

unique(iucn$BINOMIAL)

# filter for only species in South Africa
iucn_sa = iucn %>%
  filter(BINOMIAL %in% names)

# clip to south african region
iucn_sa = st_intersection(iucn_sa,regions)

# create the geopackage with first species
temp = iucn_sa %>%
  filter(BINOMIAL == "Acroteriobatus annulatus")
temp_name = "Acroteriobatus annulatus"

st_write(temp, "/Users/nfb/Dropbox/6-WILDOCEANS/KZN_workshop/IUCN_all_sa.gpkg", paste(temp_name))

# append geo package with data from all other species
for(name in names){
  temp = iucn_sa %>%
    filter(BINOMIAL == name)
  temp_name = as.character(name)
  st_write(temp, "/Users/nfb/Dropbox/6-WILDOCEANS/KZN_workshop/IUCN_all_sa.gpkg", paste(temp_name), append = TRUE)
}
