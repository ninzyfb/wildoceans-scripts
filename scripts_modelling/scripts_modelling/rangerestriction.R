# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------
# set directory to same parent folder where sub-scripts are found
# the subs-scripts can however be in folder within parent folder
path = "/home/nina/" #path for linux
path =  "/Users/nfb/" # path for mac
setwd(paste0(path,"Dropbox/6-WILDOCEANS")) # set directory

# ---------------------------------
# PACKAGES
# ---------------------------------
library(raster)
library(stringr)

# ---------------------------------
# DATA
# ---------------------------------

# list of distribution files (threshold maps)
files = list.files(path = paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs_Round3/sdms/"),pattern = "threshold", ignore.case = TRUE,full.names = TRUE)
# stack them
stack = stack(files)

# 10 km template
template = raster(list.files(pattern = "template_10km.tif", recursive = TRUE, full.names = TRUE))

# ---------------------------------
# FORMATING
# ---------------------------------

# first way to calculate range restriction:
# total number of cells in distribution
ranges = data.frame()
for(i in 1:nlayers(stack)){
ncells = length(which(values(stack[[i]])>0))
name = strsplit(names(stack[[i]]),"_Aseasonalensemblemeanthreshold")[[1]][1]
entry = as.data.frame(cbind(ncells,name))
ranges = rbind(ranges,entry)
}

# arrange by least to most range
ranges$ncells = as.numeric(ranges$ncells)
ranges = arrange(ranges,ncells)

