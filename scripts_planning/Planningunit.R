# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------

# set default options for printing tabular data
options(tibble.width = Inf)

# planning unit
# the pixel value should represent the cost of that pixel
pu = raster(list.files(path = paste0(path,"Dropbox/6-WILDOCEANS/Planning/"),pattern = "template.tif")) # raster of planning unit
