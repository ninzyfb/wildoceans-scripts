##### Packages
library(sf)
library(fasterize)
library(dplyr)
library(raster)

##### Data
estuaries = read.csv(list.files(pattern = "^estuaries.csv", recursive = TRUE))
eez = st_read(list.files(pattern = "eez.shp", recursive = TRUE))
estuaries_sf = st_as_sf(estuaries, coords = c('Longitude','Latitude'), crs = 4326)

plot(estuaries_sf$geometry) # plot

#### Rasterize the eez
# create empty raster with desired resolution
# 0.01088889 degrees = 1km and crs = 4326 (this works because the crs of the eez is also 4326)
r = raster(extent(eez), crs = 4326) 
res(r) = 0.01088889

# fasterize fills template raster with values using eez shapefile
r_eez = fasterize(summarize(eez),r)
rm(r) # remove unecessary variables\

plot(r_eez) # plot
unique(values(r_eez)) # land is NA and eez is 1
table(values(r_eez)) # number of raster cells in eez

#### Minimum distance from each gridcell to all estuaries (including microsystems)

estuaries_xy = st_coordinates(estuaries_sf) # isolate estuary coordinates
estuary_cells = cellFromXY(r_eez, estuaries_xy) # isolate raster cells using estuary coordinates

# loop to find distance from each gridcell to each estuary
# this takes about 10min
s <- stack() # create rasterstack
pb = txtProgressBar(min = 0, max = length(estuary_cells), initial = 0, style = 3) # progress bar
# for each occurrence point
for(i in 1:length(estuary_cells)){
  setTxtProgressBar(pb,i) # progress bar
  r_temp = r_eez # create temporary raster
  r_temp[estuary_cells[i]] = 2 # turn focus estuary cell to 2
  # gridDistance() computes distances from focus estuary cell to each raster cell
  # it goes around any land points (through omit = NA)
  distances_around = gridDistance(r_temp, origin = 2, omit = NA) # distance matrix for one estuary
  # add distance matrix raster to rasterstack
  s = addLayer(s, distances_around)
}

distances_min = calc(s, min) # minimum distance from each cell to an estuary
distances_min_km = distances_min/1000 # convert to km
crs(distances_min_km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # set crs
plot(distances_min_km) #plot
writeRaster(distances_min_km,"disttoestuary_all_km.tif", overwrite = TRUE) # save

#### Minimum distance from each gridcell to only main estuaries estuaries (excluding microsystems)
# filter raster stack to remove those and only keep main estuaries
colnames(estuaries)[3] = "estuary_class" # rename class column
estuaries$estuary_class = as.factor(estuaries$estuary_class) # turn class to factor
idx = which(as.numeric(estuaries$estuary_class)<2) # indices for main estuaries only
s_subset = subset(s,idx) # filter rasterstack to only keep main estuaries
distances_largeestuaries_min = calc(s_subset, min) # get new minimum distance from each cell to an estuary
distances_largeestuaries_min_km = distances_largeestuaries_min/1000 # convert to km
crs(distances_largeestuaries_min_km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
plot(distances_largeestuaries_min_km)
writeRaster(distances_largeestuaries_min_km,"disttoestuary_largeestuaries_km.tif")
