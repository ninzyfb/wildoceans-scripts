source(list.files(pattern = "species_data.R", recursive = TRUE))

##### Sampling bias

# convert data to spatial points dataframe
obs.data = st_as_sf(obs.data, coords = c("Longitude","Latitude"), crs = 4326)

# convert to raster
r = raster(obs.data)

# set the resolution of the cells i.e. 1 degree
res(r) =  1

# extend the extent of the rasterlyaer a little
r = extend(r, extent(r)+1)

# sample
# convert data  sp
obs.data = as(obs.data, Class = "Spatial")
# sample points to use
sample = gridSample(obs.data, r, n = 1)
# convert sample to sf
sample = st_as_sf(as.data.frame(sample),coords = c("coords.x1","coords.x2"), crs = 4326)
# convert data back to sf
obs.data = st_as_sf(obs.data)

# illustrate results

# create polygon out of raster
p = rasterToPolygons(r)
# convert to sf
p = st_as_sf(p)
# plot
ggplot() + 
  geom_sf(data = p)+
  geom_sf(data = obs.data)+
  geom_sf(data = sample, fill = "red")

