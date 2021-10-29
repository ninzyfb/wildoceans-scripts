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
mpas = shapefile(list.files(pattern ="SAMPAZ_OR_2020_Q3.shp" ,recursive = TRUE, full.names = TRUE))
# planning units raster
pu = raster(list.files(pattern = "template.tif",full.names = TRUE,recursive = TRUE))

# ---------------------------------
# FORMATTING
# ---------------------------------
# crop mpas to planning units (this will exclude the prince edward islands)
mpas = crop(mpas,extent(pu))
# turn to sf object
mpas = st_as_sf(mpas)

# classify mpas as either protected or semi-protected
# only fully no take zones will be considered as protected in the conservation plan
# information on this comes from: http://mpaforum.org.za/marine-protected-areas/
# additional information comes from: https://en.wikipedia.org/wiki/Marine_protected_areas_of_South_Africa#Legislation
mpas = mpas %>%
  # sanctuary means no activities whatsoever
  mutate(CUR_ZON_TY  = ifelse(CUR_ZON_TY == "Sanctuary","Protected",CUR_ZON_TY))%>%
  # wilderness means no activities but eco-tourism is possible
  mutate(CUR_ZON_TY  = ifelse(CUR_ZON_TY == "Wilderness","Protected",CUR_ZON_TY))%>%
  # restricted is a no-take zone
  mutate(CUR_ZON_TY  = ifelse(CUR_ZON_TY == "Restricted","Protected",CUR_ZON_TY))%>%
  # activities such as fishing permitted on permit basis
  mutate(CUR_ZON_TY  = ifelse(CUR_ZON_TY == "Controlled Large Pelagic","semi-protected",CUR_ZON_TY))%>%
  mutate(CUR_ZON_TY  = ifelse(CUR_ZON_TY == "Controlled","semi-protected",CUR_ZON_TY))%>%
  mutate(CUR_ZON_TY  = ifelse(CUR_ZON_TY == "Controlled Catch and Release","semi-protected",CUR_ZON_TY))%>%
  mutate(CUR_ZON_TY  = ifelse(CUR_ZON_TY == "Controlled-Pelagic Linefish with list","semi-protected",CUR_ZON_TY))

# check that it has been done properly
unique(mpas$CUR_ZON_TY)
# only keep protected areas
mpas_keep = mpas[mpas$CUR_ZON_TY=="Protected",]
# turn to spatial object
mpas_keep = as(mpas_keep,"Spatial")
# simplify
mpas_keep = gSimplify(mpas_keep,tol = 0.01)
plot(mpas_keep)
# rasterize
mpas_keep_r = rasterize(mpas_keep,pu)
plot(mpas_keep_r)
# turn all values to 1
values(mpas_keep_r)[!is.na(values(mpas_keep_r))] = 1
plot(mpas_keep_r)

# ---------------------------------
# WRITING
# ---------------------------------
# save raster as locked in
writeRaster(mpas_keep_r,"mpa_lockedin.tif")

