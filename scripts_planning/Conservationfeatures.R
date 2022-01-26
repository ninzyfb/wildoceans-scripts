# ---------------------------------
# DATA
# ---------------------------------

# species distribution file names (continuous)
files = list.files(path = paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs"),pattern = "ensemblemean.tif", recursive = TRUE,full.names = TRUE)
feature_stack = stack()
for(i in 1:length(files)){
  temp = raster(files[i])
  temp = projectRaster(temp,pu)
  feature_stack = addLayer(feature_stack,temp)
}
rm(i,files,temp) # remove unnecessary variables

# species distribution file names (binary)
files = list.files(path = paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs"),pattern = "ensemblemeanthreshold.tif", recursive = TRUE,full.names = TRUE)
feature_stack_binary = stack()
for(i in 1:length(files)){
  temp = raster(files[i])
  temp = projectRaster(temp,pu)
  feature_stack_binary = addLayer(feature_stack_binary,temp)
}
rm(i,files,temp) # remove unnecessary variables

# ---------------------------------
# FORMATTING
# ---------------------------------

# turn all NA values to 0
values(feature_stack)[is.na(values(feature_stack))] = 0
values(feature_stack_binary)[is.na(values(feature_stack_binary))] = 0

# mask with pu
feature_stack = stack(mask(feature_stack,pu))
feature_stack_binary = stack(mask(feature_stack_binary,pu))

# extract scientific name from stack of distributions
featurenames = as.data.frame(names(feature_stack))
featurenamesbinary = as.data.frame(names(feature_stack_binary))
colnames(featurenames) = "featurename"
colnames(featurenamesbinary) = "featurename_binary"
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

# add binary stack names
featurenames = cbind(featurenames,featurenamesbinary)
rm(featurenamesbinary)

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

# create raster stack using threshold values
#colnames(thresholds_df) = toupper(colnames(thresholds_df))
#featurenames = left_join(featurenames,thresholds_df)
#rm(thresholds_df)

#feature_stack_binary = feature_stack
#for(i in 1:nlayers(feature_stack_binary)){
#  feature_stack_binary[[i]][values(feature_stack_binary[[i]])<featurenames$THRESH[i]] = 0
#  feature_stack_binary[[i]][values(feature_stack_binary[[i]])>featurenames$THRESH[i]] = 1
#  }

# filter feature stacks by modeltype
# filter feature stack as well
# this creates a separate raster stack for each problem run
keep = str_detect(names(feature_stack),"Aseasonal")  # identify Aseasonal layers in stack
idx = which(keep == FALSE) # find out layer number with feature to omit
feature_stack_aseasonal = dropLayer(feature_stack,idx)
rm(idx,keep) # remove

#keep = str_detect(names(feature_stack_binary),"Aseasonal")  # identify Aseasonal layers in stack
#idx = which(keep == FALSE) # find out layer number with feature to omit
#feature_stack_binary_aseasonal = dropLayer(feature_stack_binary,idx)
#rm(idx,keep) # remove

#keep = str_detect(names(feature_stack),c("summer")) 
#idx = which(keep == FALSE) # find out layer number with feature to omit
#feature_stack_summer = dropLayer(feature_stack,idx)
#rm(idx,keep) # remove

#keep = str_detect(names(feature_stack),c("winter")) 
#idx = which(keep == FALSE) # find out layer number with feature to omit
#feature_stack_winter = dropLayer(feature_stack,idx)
#rm(idx,keep) # remove



# species distribution threshold values
#thresholds = list.files(path = paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs"),pattern = "thresh.csv", recursive = TRUE,full.names = TRUE)
#thresholds_df = data.frame()
#for(i in 1:length(thresholds)){
#  name = str_split(thresholds[i],"/")[[1]][9]
# extract parameters from file name (very clunky for now)
#  model =  str_split(name," ")[[1]][2]
#  genus = str_split(name," ")[[1]][3]
#  species = str_split(name," ")[[1]][4]
# read csv
#  thresh = read.csv(thresholds[i])
#  thresh$X = NULL
# add parameters
#  thresh$modeltype = model
#  thresh$species_scientific = paste(genus, species)
# divide value by 1000
#  thresh$thresh = thresh$thresh/1000
# add to maindataframe
#  thresholds_df = rbind(thresholds_df,thresh)
#  }
#rm(name,model,genus,species,thresh,thresholds)


rm(temp,i,files) # remove
