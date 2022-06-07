# ---------------------------------------------------------------------------------
######### Shark and ray species conservation planning using prioritizr - Lockedin script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
#!Run each script one at a time as running the whole at once seems to cause some bugs
#the output line describes what each script produces
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: this script is to prepare the various aggregation spot rasters
####

# ---------------------------------
# DATA
# ---------------------------------

# estuaries
# all estuaries
estuaries = read.csv(list.files(pattern = "estuaries.csv", recursive = TRUE, full.names = TRUE)[2])
# filter to only keep large and/or open ones
estuaries = estuaries %>%
  filter(str_detect(Type_present_2020, c("Large|Open")))
# convert to sf object
estuaries = st_as_sf(estuaries,coords = c("Longitude", "Latitude"))
# rasterize
estuaries = rasterize(st_as_sf(estuaries$geometry),pu)
# convert all values to 1
values(estuaries)[which(values(estuaries)>0)] = 1
# rename layer
names(estuaries) = "estuaries"
estuaries = projectRaster(estuaries,pu)


# aggregation spots
aggregations1 = st_read(list.files(pattern = "aggregationspots_murray.shp",recursive=TRUE,full.names = TRUE))
aggregations2 = st_read(list.files(pattern = "aggregationspots_olbers.shp",recursive=TRUE,full.names = TRUE))
aggregations3 = st_read(list.files(pattern = "aggregationspots_wildoceans.shp",recursive=TRUE,full.names = TRUE))
aggregations = c(aggregations1$geometry,aggregations2$geometry,aggregations3$geometry)
rm(aggregations1,aggregations2,aggregations3)
# drop z dimension
aggregations = st_zm(aggregations, drop = TRUE, what = "ZM")
aggregations = st_as_sf(aggregations)
# rasterize
aggregations = rasterize(aggregations,pu)
# convert all values to 1
values(aggregations)[which(values(aggregations)>0)] = 1
# rename layer
names(aggregations) = "aggregations"
aggregations = projectRaster(aggregations,pu)

# mpas (all of them)
mpa_layer_all = st_read(list.files(pattern = "SAMPAZ_OR_2021_Q3.shp",recursive=TRUE,full.names = TRUE))

# remove prince edward islands and group by name
mpa_layer_all = mpa_layer_all %>%
  filter(CUR_NME != "Prince Edward Island Marine Protected Area") %>%
  group_by(CUR_NME,CUR_ZON_TY) %>%
  summarise()

# extract no take zones
mpa_layer_fullyprotected = mpa_layer_all %>%
  filter(CUR_ZON_TY %in% c("Wilderness","Restricted","Sanctuary"))

# rasterize
mpa_layer_all = rasterize(mpa_layer_all,pu, getCover=TRUE)
# rename layer
names(mpa_layer_all) = "mpa_layer_all"
# only keep in cells wich are covered by 50% or more by the polygon
values(mpa_layer_all)[which(values(mpa_layer_all)>=0.5)] = 1
values(mpa_layer_all)[which(values(mpa_layer_all)<0.5)] = NA

# mpas (only fully protected ones)
# rasterize
mpa_layer_fullyprotected = rasterize(mpa_layer_fullyprotected,pu,getCover=TRUE)
# rename layer
names(mpa_layer_fullyprotected) = "mpa_layer_fullyprotected"
# only keep in cells wich are covered by 50% or more by the polygon
values(mpa_layer_fullyprotected)[which(values(mpa_layer_fullyprotected)>=0.5)] = 1
values(mpa_layer_fullyprotected)[which(values(mpa_layer_fullyprotected)<0.5)] = NA

# ---------------------------------
# FORMATTING
# ---------------------------------

# stack
lockedin = stack(estuaries,aggregations,mpa_layer_all,mpa_layer_fullyprotected)
plot(lockedin)
rm(aggregations,estuaries,mpa_layer_all,mpa_layer_fullyprotected)
# convert all 0 values to NA
values(lockedin)[which(values(lockedin)==0)] = NA
plot(lockedin)
