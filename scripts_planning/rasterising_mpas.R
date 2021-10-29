library(sf)
library(fasterize)
library(dplyr)

# data
mpa = st_read(list.files(pattern = "SAMPAZ_OR_2020_Q3.shp", recursive = TRUE))

# crop mpas to planning units (this excludes prince edward islands)
mpa = st_crop(mpa,extent(pu))

# convert to polygon
mpa = st_sf(mpa)
mpa_combined = st_combine(mpa)

mpa_r = as(mpa,"SpatialPolygons")
mpa_r$CUR_ZON_TY = as.factor(mpa_r$CUR_ZON_TY)
unique(mpa_r$CUR_ZON_TY)

raster:rasterize(mpa,pu,field = "GIS_AREA",fun="first")

# assign different zone categories to one of protected or semi-protected
mpa = mpa %>%
  mutate(CUR_ZON_TY  = ifelse(CUR_ZON_TY == "Sanctuary","Protected",CUR_ZON_TY))%>%
  mutate(CUR_ZON_TY  = ifelse(CUR_ZON_TY == "Wilderness","Protected",CUR_ZON_TY))%>%
  mutate(CUR_ZON_TY  = ifelse(CUR_ZON_TY == "Restricted","Protected",CUR_ZON_TY))%>%
  mutate(CUR_ZON_TY  = ifelse(CUR_ZON_TY == "Controlled Large Pelagic","semi-protected",CUR_ZON_TY))%>%
  mutate(CUR_ZON_TY  = ifelse(CUR_ZON_TY == "Controlled","semi-protected",CUR_ZON_TY))%>%
  mutate(CUR_ZON_TY  = ifelse(CUR_ZON_TY == "Controlled Catch and Release","semi-protected",CUR_ZON_TY))%>%
  mutate(CUR_ZON_TY  = ifelse(CUR_ZON_TY == "Controlled-Pelagic Linefish with list","semi-protected",CUR_ZON_TY))
# check that it has been done properly
unique(mpa$CUR_ZON_TY)
# only plot prtected mpas
plot(mpa$geometry[mpa$CUR_ZON_TY=="Protected"])

# only keep full protected mpas (sanctuary wilderness and restricted)
mpa_keep = mpa[mpa$CUR_ZON_TY=="Protected",]

test = as(mpa_keep,Class = "Spatial")
plot(mpa_keep)

test2 = rasterize(mpa, pu, field = "CUR_ZON_TY", fun = "mean", 
          update = TRUE, updateValue = "NA")
plot(test2)

# convert to raster
asterize(test,pu,field = "CUR_ZON_TY")

# turn to raster using fasterize
# protected areas will be 1 and semi-protected areas will be 2
mpa_keep$CUR_ZON_TY = as.factor(mpa_keep$CUR_ZON_TY)
rasterize(mpa_keep,pu)
mpa_keep_raster = rasterize(st_collection_extract(mpa_keep,"POLYGON"),pu, field = "CUR_ZON_TY")
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
