# Packages
library(sf)
library(dplyr)
library(raster)
library(eply)

# Data
nba = st_read(list.files(pattern = "MarineEcosystemMap2018_beta.shp", recursive = TRUE))
sub = st_read(list.files(pattern = "substrate_simplified.shp", recursive = TRUE))
eco = st_read(list.files(pattern = "ecoregion_modified.shp", recursive = TRUE))

geom = st_geometry(nba) # extract geometry
st_geometry(nba) = NULL # remove geometry

idx = names(nba) # extract column names
nba[ ,idx] <- lapply(nba[ , idx], as.factor) # convert all columns to factors

st_geometry(nba) = geom # return geometry

rm(geom,idx) # remove variables

# create separate list with separate entry for each variable
list = list()
count = 1
for(i in colnames(nba)){
  list[[count]] = nba[,i]
  count = count + 1
}
rm(i, count) # remove variables

# keep only variable of interest
list = list[c(6,7,8,9,10,11)]

for(i in 1:length(list)){
temp = list[[i]] # extract one variable
variable = names(temp)[1] # simplify geometries by grouping by variable and simplifying
temp = temp %>%
  group_by_(variable)%>%
  summarise()
r = raster(extent(temp), resolution = 1000, crs = crs(temp)) # create empty raster (1km res)
r_2 = rasterize(temp, r) # fill with variable
st_write(temp,paste(variable,".shp",sep=""))# write shapefile
writeRaster(r_2, paste0(names(temp)[1],".tif",sep="")) # save raster with variable name
}

r = raster(list.files(pattern="template.tif",recursive = TRUE)) # load template raster
eco$Ecoregion = as.factor(eco$Ecoregion)
eco = st_transform(eco,st_crs(r))

?rasterize
r_2 = rasterize(eco, r, field = "Ecoregion") # fill with variable
r_2 = mask(r_2,r)

plot(r)
plot(r_2, add = TRUE)

writeRaster(r_2, "ecoregion_simplified.tif",overwrite=TRUE) # save raster with variable name
rm(r,r_2,eco)

