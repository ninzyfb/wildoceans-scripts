getwd()

# packages
library(sf)
library(dplyr)

# load polygon
p1 = read_sf("Crownarea.shp", crs = 4326)

# load associated data
d1 = read.csv("Polygon_data/Natalie_crownarea.csv")

# create merging column
colnames(p1)[2] = "Location_specific"
d1 = d1 %>% 
  mutate(Location = ifelse(Location == "Langebaan outside MPA","notnotake",Location))%>% 
  mutate(Location = ifelse(Location == "Langebaan in MPA","notake",Location))

p1$Location_specific= "Crown area"
p1$Location = NULL
p1 = p1 %>% 
  mutate(Location = ifelse(Location == "East Londo","East London",Location))

# merge
merged = left_join(p1, d1, by= "Location_specific")

# write data

write_sf(merged,"Polygon_data/Natalie_crownarea_clean.shp")
