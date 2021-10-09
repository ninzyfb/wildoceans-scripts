library(sf)
library(fasterize)
library(dplyr)

# data
mpa = st_read(list.files(pattern = "SAMPAZ_OR_2020_Q3.shp", recursive = TRUE))
eez = raster(list.files(pattern = "template.tif", recursive = TRUE))

# crop mpas to eez (this excludes prince edward islands)
mpa = st_crop(mpa,extent(eez))

mpa2 = mpa %>%
  mutate(CUR_ZON_TY  = ifelse(CUR_ZON_TY == "Sanctuary","Protected",CUR_ZON_TY))%>%
  mutate(CUR_ZON_TY  = ifelse(CUR_ZON_TY == "Wilderness","Protected",CUR_ZON_TY))%>%
  mutate(CUR_ZON_TY  = ifelse(CUR_ZON_TY == "Restricted","Protected",CUR_ZON_TY))%>%
  mutate(CUR_ZON_TY  = ifelse(CUR_ZON_TY == "Controlled Large Pelagic","semi-protected",CUR_ZON_TY))%>%
  mutate(CUR_ZON_TY  = ifelse(CUR_ZON_TY == "Controlled","semi-protected",CUR_ZON_TY))%>%
  mutate(CUR_ZON_TY  = ifelse(CUR_ZON_TY == "Controlled Catch and Release","semi-protected",CUR_ZON_TY))%>%
  mutate(CUR_ZON_TY  = ifelse(CUR_ZON_TY == "Controlled-Pelagic Linefish with list","semi-protected",CUR_ZON_TY))

unique(mpa2$CUR_ZON_TY)

# turn mpas to raster using fasterize
# protected areas will be 1 and semi-protected areas will be 2
mpa2$CUR_ZON_TY = as.factor(mpa2$CUR_ZON_TY)
mpa_raster = fasterize(st_collection_extract(mpa2,"POLYGON"),eez, field = "CUR_ZON_TY")
plot(mpa_raster)
unique(values(mpa_raster))

# turn semi-protected to 3 and protected to 2
mpa_raster[values(mpa_raster)==2] = 3
mpa_raster[values(mpa_raster)==1] = 2

# add eez behind mpas
mpa_raster_clean = cover(mpa_raster,eez)
plot(mpa_raster_clean)

# write this layer as mpalayer_1
writeRaster(mpa_raster_clean,"mpalayer_1.tif", overwrite=TRUE)

# now only keep protected regions
mpa_raster_clean[values(mpa_raster_clean)!=2] = 1

# write this layer as mpalayer_2
writeRaster(mpa_raster_clean,"mpalayer_2.tif", overwrite=TRUE)
