# Projecting last two years of sst data
# this is necessary because the data downloaded for 2020 and 2021
# differs very slightly in resolution and extent
# from the data downloaded from 2007 to 2019 

# data
sst_2 = list.files(pattern = "sst20", recursive = TRUE)
# stack the last two years
stack = stack(sst_2[145:168])
# get resolution of al other sst files
r_example = raster(list.files(pattern = "sst200701", recursive = TRUE))
res(r_example)
# resolution of the last two years is a little more
res(stack)
# i will decrease the resolution to match all the other files
stack_modified = projectRaster(stack,r_example, method = 'ngb')
# check
res(r_example) == res(stack_modified)
# rewrite 2020 and 2021 rasters with new resolution
layer_filenames <- names(stack_modified)
for(i in 1:24) {
  raster_temp = raster(stack_modified, layer = i)
  writeRaster(raster_temp, paste(layer_filenames[i],".tif", sep=""))
}