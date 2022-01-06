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

# mpas (all of them)
mpa_layer_all = st_read(list.files(pattern = "SAMPAZ_OR_2020_Q3.shp",recursive=TRUE,full.names = TRUE))
# drop z dimension
mpa_layer_all = st_zm(mpa_layer_all, drop = TRUE, what = "ZM")
# rasterize
mpa_layer_all = rasterize(mpa_layer_all,pu)
# convert all values to 1
values(mpa_layer_all)[which(values(mpa_layer_all)>0)] = 1
# rename layer
names(mpa_layer_all) = "mpa_layer_all"

# mpas (only fully protected ones)
mpa_layer_fullyprotected = raster(list.files(pattern = "mpa_lockedin.tif",recursive=TRUE,full.names = TRUE))
# rename layer
names(mpa_layer_fullyprotected) = "mpa_layer_fullyprotected"


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
