# packages
library(raster)

# data 
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/environmental_variables/Chlorophyll/Chlorophylla_MODISAQUA_SA_EEZ_monthlyavg_2003-2020_originalres_4km/")
chl_stack = stack(list.files(pattern = "MODISAQUA_CHLA_4KM_", recursive = TRUE))
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/environmental_variables/")

# calculate average over all years
# remove na values (not sure taht changes much)
chl_average = calc(chl_stack, fun = mean, na.rm = TRUE)
plot(chl_average)

# write layer
writeRaster(chl_average, "chl_average.tif")



