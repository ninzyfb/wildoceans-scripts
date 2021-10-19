# ---------------------------------------------------------------------------------
######### Shark and ray species distribution models (SDMs) - fishingpressure_NBA script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: 
####

# ---------------------------------
# PACKAGES
# ---------------------------------
library(dplyr)
library(raster)
library(stringr)

# ---------------------------------
# DIRECTORY
# ---------------------------------
path =  "/Users/nfb/"
setwd(paste0(path,"Dropbox/6-WILDOCEANS"))

# ---------------------------------
# DATA
# ---------------------------------
# list fishing pressure files
fp = list.files(pattern = "newres1000.tif", recursive=TRUE,full.names = TRUE, ignore.case = TRUE)
template = raster(list.files(pattern = "template.tif",full.names = TRUE,recursive = TRUE)) # raster of planning unit

# ---------------------------------
# FORMATTING
# ---------------------------------

# stack files
fp_stack = stack(fp)

# reproject raster stack
fp_stack_projected = projectRaster(fp_stack,template)

# mask with template
fp_stack_projected_masked = mask(fp_stack_projected,template)
plot(fp_stack_projected_masked)
names(fp_stack_projected_masked)
values(fp_stack_projected_masked[[1]])[values(fp_stack_projected_masked[[1]])>100] = NA

