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
estuaries = st_read(list.files(pattern = "estuaries.shp",recursive=TRUE,full.names = TRUE))

# aggregation spots
aggregations = st_read(list.files(pattern = "aggregationspots_final.shp",recursive=TRUE,full.names = TRUE))

# mpas
mpas = raster(list.files(pattern = "mpalayer_2.tif",recursive=TRUE,full.names = TRUE))

# ---------------------------------
# FORMATTING
# ---------------------------------

# rasterize
estuaries_raster = rasterize(estuaries,pu)
estuaries_raster = rasterize(st_collection_extract(estuaries,"POLYGON"),pu, field = "Ecoregion")
plot(estuaries_raster)

aggregations_raster = rasterize(aggregations,pu)

# standardize crs
estuaries_raster = projectRaster(estuaries_raster,pu)
aggregations_raster = projectRaster(aggregations_raster,pu)
mpas = projectRaster(mpas,pu)

# stack
lockedin = stack(estuaries_raster,aggregations_raster,mpas)
plot(lockedin)

