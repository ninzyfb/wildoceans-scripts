# Set directory
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/ALL_LAYERS_5km_resolution")
getwd()

# Packages
library(raster)
library(ENMTools)
library(dplyr)

# list all your environmental variables
files = list.files()

# stack all rasters
variables = stack(files)
rm(files)
variables$substrate_simplified = as.factor(variables$substrate_simplified)

# look at them
plot(variables)

# compute correlation coefficient between each raster pair (ENMtools package)
r = raster.cor.matrix(stack)

# export correlation coefficients
write.csv(r,"correlations_raw.csv")

# filter any correlations greater than 0.7
names(r)
count = 1
list = list()
for(i in names(r)){
  temp = r %>%
    select(i)
  idx = abs(temp[,1])<0.8
  headers = rownames(temp)[idx]
  list[[count]] = headers
  count = count +1
}

tokeep = sort(unique(unlist(list)))
tokeep2 = sort(unlist(mods_multGLM$variables))


# Look at heavily correlated variables on map to be sure
cor1 = corLocal(variables$Estuaries,variables$Depth, test = TRUE)
plot(cor1)
cor2 = corLocal(variables$Substrate,variables$SST)
plot(cor2)
