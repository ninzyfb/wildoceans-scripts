# ---------------------------------------------------------------------------------
######### Shark and ray species conservation planning using prioritizr - rasterizing mpas script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: 
####

# ---------------------------------
# PACKAGES
# ---------------------------------
library(sf)
library(fasterize)
library(dplyr)

# ---------------------------------
# DATA
# ---------------------------------
# mpa shapefile
mpas = st_read(list.files(pattern = "SAMPAZ_OR_2021_Q3.shp",recursive=TRUE,full.names = TRUE))

# ---------------------------------
# FORMATTING and EXPLORATION
# ---------------------------------
# turn off scientific numbering
options(scipen = 100) 

# remove prince edwards island
mpas = mpas %>%
  filter(CUR_NME != "Prince Edward Island Marine Protected Area") %>%
  group_by(CUR_ZON_NM,CUR_NME,CUR_ZON_TY,GIS_AREA) %>%
  summarise()

# remove geomatry
mpas$geometry = NULL

# convert area to km2 (currently in ha)
mpas$GIS_AREA = mpas$GIS_AREA*0.01

# total area
total_area = sum(mpas$GIS_AREA)

# no-take area
unique(mpas$CUR_ZON_TY)
idx = which(mpas$CUR_ZON_TY %in% c("Restricted","Sanctuary","Wilderness"))
notake_area = sum(mpas$GIS_AREA[idx])

# add no-take collumn
mpas$notake = NA
mpas$notake[which(mpas$CUR_ZON_TY %in% c("Restricted","Sanctuary","Wilderness"))] = "yes"
mpas$notake[which(!(mpas$CUR_ZON_TY %in% c("Restricted","Sanctuary","Wilderness")))] = "no"


# get total area by mpa and no-take zones
mpa_areas = mpas %>%
  group_by(CUR_NME,notake) %>%
  summarise(total_area = round(sum(GIS_AREA),1)) %>%
  arrange(desc(total_area))

mpa_areas = pivot_wider(mpa_areas,names_from = notake, values_from = total_area)
mpa_areas$no[which(is.na(mpa_areas$no))] = 0
mpa_areas$yes[which(is.na(mpa_areas$yes))] = 0
mpa_areas$total = round(mpa_areas$yes + mpa_areas$no,1)
mpa_areas$no = NULL
mpa_areas$percentage_notake = round((mpa_areas$yes/mpa_areas$total)*100,1)
colnames(mpa_areas) = c("MPA","no-take area","total area","percent_notake")
mpa_areas = mpa_areas %>%
  arrange(desc(`no-take area`))
write.csv(mpa_areas,"mpa_areas.csv",row.names = FALSE)

# classify mpas as either protected or semi-protected
# only fully no take zones will be considered as protected in the conservation plan
# information on this comes from: http://mpaforum.org.za/marine-protected-areas/
# additional information comes from: https://en.wikipedia.org/wiki/Marine_protected_areas_of_South_Africa#Legislation
mpa_layer_all = mpa_layer_all %>%
  # sanctuary means no activities whatsoever
  mutate(CUR_ZON_TY  = ifelse(CUR_ZON_TY == "Sanctuary","Protected",CUR_ZON_TY))%>%
  # wilderness means no activities but eco-tourism is possible
  mutate(CUR_ZON_TY  = ifelse(CUR_ZON_TY == "Wilderness","Protected",CUR_ZON_TY))%>%
  # restricted is a no-take zone
  mutate(CUR_ZON_TY  = ifelse(CUR_ZON_TY == "Restricted","Protected",CUR_ZON_TY))

# only keep protected areas
mpa_layer_protected = mpa_layer_all[mpa_layer_all$CUR_ZON_TY=="Protected",]

mpa_layer_protected = mpa_layer_protected %>%
  group_by(CUR_ZON_NM)%>%
  summarise()

# ---------------------------------
# WRITING
# ---------------------------------
# save raster as locked in
st_write(mpa_layer_protected,paste0(my.directory,"/mpa_layer_protected.shp"))
