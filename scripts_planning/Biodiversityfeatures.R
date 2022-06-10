# ---------------------------------------------------------------------------------
# AUTHORS: Nina Faure Beaulieu, Dr. Victoria Goodall (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST
# CONTACTs: ninab@wildtrust.co.za; victoria.goodall@mandela.ac.za 
# ---------------------------------------------------------------------------------


# ---------------------------------
# DATA
# ---------------------------------
# species distribution file names (continuous)
files = list.files(path = paste0(getwd(),"/wildoceans-scripts/"),pattern = "ensemblemean.tif", recursive = TRUE,full.names = TRUE)

# create raster stack
feature_stack = stack()
for(i in 1:length(files)){
  temp = raster(files[i])
  temp = projectRaster(temp,pu)
  feature_stack = addLayer(feature_stack,temp)
}
rm(i,files,temp) # remove unnecessary variables

# threshold values (to turn continuous distributions to binary)
threshs = list.files(path = paste0(path,"Dropbox/6-WILDOCEANS/wildoceans-scripts/"),pattern = "thresh.csv", recursive = TRUE, full.names = TRUE)
# ---------------------------------


# ---------------------------------
# FORMATTING
# ---------------------------------
# turn all NA values to 0 (prioritizr does not like NA values)
values(feature_stack)[is.na(values(feature_stack))] = 0

# mask with pu
feature_stack = stack(mask(feature_stack,pu))

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

# turn headers to capital
colnames(featurenames) = toupper(colnames(featurenames))

# turn model type (season) to upper case
featurenames$MODELTYPE = toupper(featurenames$MODELTYPE)

# ---------------------------------
# FORMATTING
# ---------------------------------

# divide all values by 1000 to get actual probability values between 0 and 1
# only divide non NA values by 1000
for(i in 1:nlayers(feature_stack)){ 
  values(feature_stack[[i]])[!is.na(values(feature_stack[[i]]))] = values(feature_stack[[i]])[!is.na(values(feature_stack[[i]]))]/1000
}

# turn any negative values to 0
for(i in 1:nlayers(feature_stack)){ 
  feature_stack[[i]][values(feature_stack[[i]])<0] = 0
}

# clamp values to turn extremely small values to 0
feature_stack = raster::clamp(feature_stack, lower = 1e-6, useValues = TRUE)
feature_stack = stack(feature_stack)

rm(i) # remove unnecessary variables

# create feature stack where threshold values are used to filter low probability portion of occurences
sdms_thresholds = stack()
for(i in 1:nlayers(feature_stack)){
  temp = feature_stack[[i]]
  thresh_value = read.csv(threshs[i])
  thresh_value = (thresh_value$thresh)/1000
  values(temp)[values(temp)<thresh_value] = 0
  sdms_thresholds = addLayer(sdms_thresholds,temp)
}
rm(temp)

# filter to only keep aseasonal features
keep = str_detect(names(feature_stack),"Aseasonal")  # identify aseasonal layers in stack
idx = which(keep == FALSE) # find out layer number with feature to omit
feature_stack_aseasonal = dropLayer(feature_stack,idx)
rm(idx,keep) # remove
names(feature_stack_aseasonal) = featurenames$SPECIES_SCIENTIFIC

# filter to only keep aseasonal features of threshold stack
keep = str_detect(names(sdms_thresholds),"Aseasonal")  # identify aseasonal layers in stack
idx = which(keep == FALSE) # find out layer number with feature to omit
sdms_thresholds = dropLayer(sdms_thresholds,idx)
rm(idx,keep) # remove
names(sdms_thresholds) = featurenames$SPECIES_SCIENTIFIC

# remove redundant feature stacks
rm(feature_stack,feature_stack_aseasonal,feature_stack_thresholds,i,idx, thresh_value,threshs,featurenames)

# remove problem species
idx = which(str_replace(names(sdms_thresholds),"\\."," ") %in% problem_species)
sdms_thresholds = dropLayer(sdms_thresholds,idx)
