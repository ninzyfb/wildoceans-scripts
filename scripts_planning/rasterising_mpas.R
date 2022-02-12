# ---------------------------------------------------------------------------------
######### Shark and ray species conservation planning using prioritizr - rasterizing mpas script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: This script prepares the mpas to be used as "locked in" areas in some planning scenarios
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
mpa_layer_all = st_read(list.files(pattern = "SAMPAZ_OR_2020_Q3.shp",recursive=TRUE,full.names = TRUE))
# planning units raster
pu = raster(list.files(pattern = "template.tif",full.names = TRUE,recursive = TRUE))

# ---------------------------------
# FORMATTING
# ---------------------------------
# exclude the prince edward islands
mpa_layer_all = mpa_layer_all %>%
  filter(CUR_NME != "Prince Edward Island Marine Protected Area") 

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

# check that it has been done properly
unique(mpa_layer_all$CUR_ZON_TY)

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
