# ---------------------------------
# DATA
# ---------------------------------

# species distribution file names
files = list.files(path = paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs_Round3"),pattern = "ensemblemean.tif", recursive = TRUE,full.names = TRUE)
feature_stack = stack()
for(i in 1:length(files)){
  temp = raster(files[i])
  temp = projectRaster(temp,pu)
  feature_stack = addLayer(feature_stack,temp)
}
rm(files,temp) # remove unnecessary variables
# turn all NA values to 0
values(feature_stack)[is.na(values(feature_stack))] = 0

# ---------------------------------
# FORMATTING
# ---------------------------------

# extract scientific name from stack of distributions
featurenames = as.data.frame(names(feature_stack))
colnames(featurenames) = "featurename"
for(i in 1:nrow(featurenames)){
  # extract model type
  featurenames$modeltype[i] = strsplit(featurenames$featurename,"_")[[i]][3]
  featurenames$modeltype[i] = strsplit(featurenames$modeltype,"ensemblemean")[[i]][1]
  # extract scientific name by pasting genus and species name from file name
  featurenames$species_scientific[i] = paste(strsplit(featurenames$featurename,"_")[[i]][1] ,strsplit(featurenames$featurename,"_")[[i]][2])}

rm(i) # remove unnecessary variables

# turn species names to upper case
featurenames$species_scientific = toupper(featurenames$species_scientific)

# ---------------------------------
# FORMATTING
# ---------------------------------

# divide all values by 1000 to get actual probability values between 0 and 1
for(i in 1:nlayers(feature_stack)){ 
  values(feature_stack[[i]]) = values(feature_stack[[i]])/1000
}

# turn any negative values to 0
for(i in 1:nlayers(feature_stack)){ 
  feature_stack[[i]][values(feature_stack[[i]])<0] = 0
}

# clamp values to turn extremely small values to 0
#feature_stack = raster::clamp(feature_stack, lower = 1e-6, useValues = TRUE)
feature_stack = stack(feature_stack)

rm(i) # remove unnecessary variables

