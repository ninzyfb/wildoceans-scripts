##### Directory
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling") # set to modelling folder
getwd()

##### Packages
library(sp)
library(raster)

##### Load raster stack
# a stack is a location for all the files
# this means that to load the stack, the directory needs to be the same as the location of the files
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/ALL_LAYERS_5km_resolution")
variable_stack = stackOpen("stack_envtvariables")
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling") # reset directory to parent modelling folder

##### Extract variables from subsampled occurrence points
extracted_subset = extract(variable_stack, pts_subset)

# extract variables for background points
extracted_background = extract(variable_stack, pts_background)

##### Combine presence and background
# indicate presence or absence/background using pa = 1 and pa = 0
combined <- rbind(cbind(pa=1, extracted_subset), cbind(pa=0, extracted_background))
combined = as.data.frame(combined) # save as data frame

# convert appropriate variables to factors
# this prevent prediction later on from working for some reason?

#combined$Bathome = as.factor(combined$Bathome)
#combined$Bathyregion = as.factor(combined$Bathyregion)
#combined$Broadecosystem = as.factor(combined$Broadecosystem)
#combined$Ecoregion = as.factor(combined$Ecoregion)
#combined$Ecosystem = as.factor(combined$Ecosystem)
#combined$Substrate = as.factor(combined$Substrate)

rm(extracted_background,extracted_subset) # remove unecessary variables from environment
