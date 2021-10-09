## Packages
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(sp) # package to accompany raster package
library(sf) # for shapefile loading

## Data
DEM <- raster("Bathymetry_GEBCO/gebco_2020_n-26.8621_s-38.1752_w13.348_e36.5307.asc")
eez = st_read("EEZ/eez.shp",crs = 4326)

## Clip to EEZ
DEM_1 = mask(DEM, eez)

values(DEM_1)
## extract contour lines
contours = rasterToContour(DEM_1,maxpixels = 1000000000)
plot(contours)

## save as shapefile
writeOGR(contours,dsn ="Bathymetry_GEBCO",layer="bathymetry_3",driver="ESRI Shapefile")
