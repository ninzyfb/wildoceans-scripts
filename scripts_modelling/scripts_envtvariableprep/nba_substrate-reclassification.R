# Packages
library(sf)
library(dplyr)
library(raster)

# Data
substrate = st_read(list.files(pattern = "Substratum.shp", recursive = TRUE))
geom = st_geometry(substrate) # extract geometry
st_geometry(substrate) = NULL # remove geometry


# variables to group
bay = grep("Bays",substrate$Substratum, value = TRUE)
island = grep("Islands",substrate$Substratum, value = TRUE)
mixed = grep("Mixed",substrate$Substratum, value = TRUE)
mosaic = grep("Mosaic",substrate$Substratum, value = TRUE)
muddy = grep("Muddy",substrate$Substratum, value = TRUE)
rocky = grep("Rocky",substrate$Substratum, value = TRUE)
sandy = grep("Sandy",substrate$Substratum, value = TRUE)
canyons = grep("Canyons",substrate$Substratum, value = TRUE)
abyss = grep("Abyss",substrate$Substratum, value = TRUE)
kelp = grep("Kelp",substrate$Substratum, value = TRUE)
seamount = grep("Seamounts",substrate$Substratum, value = TRUE)

# add group to substrate data
substrate_2 = substrate %>%
  mutate(group = ifelse(Substratum %in% bay, "bay",Substratum))%>%
  mutate(group = ifelse(Substratum %in% island, "island",group))%>%
  mutate(group = ifelse(Substratum %in% mixed, "mixed",group))%>%
  mutate(group = ifelse(Substratum %in% muddy, "muddy",group))%>%
  mutate(group = ifelse(Substratum %in% mosaic, "mosaic",group))%>%
  mutate(group = ifelse(Substratum %in% rocky, "rocky",group))%>%
  mutate(group = ifelse(Substratum %in% sandy, "sandy",group))%>%
  mutate(group = ifelse(Substratum %in% canyons, "canyons",group))%>%
  mutate(group = ifelse(Substratum %in% abyss, "abyss",group))%>%
  mutate(group = ifelse(Substratum %in% kelp, "kelp",group))%>%
  mutate(group = ifelse(Substratum %in% seamount, "seamount",group))

# group geometries by group
st_geometry(substrate_2) = geom # return geometry

# write file
st_write(substrate_2,"substrate_2.shp")



