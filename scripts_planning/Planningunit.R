# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------

# set default options for printing tabular data
options(tibble.width = Inf)

# planning unit
# the pixel value should represent the cost of that pixel
pu = raster(list.files(pattern = "template.tif",full.names = TRUE,recursive = TRUE)) # raster of planning unit
