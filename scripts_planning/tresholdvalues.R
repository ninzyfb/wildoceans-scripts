############# STILL NEEED TO WORK ON THIS SO IGNORE FOR NOW #####
# load calculated thresholds for features in stack
# for this you need to make the naming system in both the feature stack names and threshold file names match
# load threshold file names
thresholds = list.files(pattern = "thresh.csv",recursive = TRUE)
# turn file names into dataframe
thresholds = as.data.frame(thresholds)
colnames(thresholds) = "filename"
for(i in 1:nrow(thresholds)){
  # extract model type
  thresholds$modeltype[i] = strsplit(thresholds$filename," ")[[i]][2]
  # extract scientific name by pasting genus and species name from file name
  thresholds$species_scientific[i] = paste(strsplit(thresholds$filename," ")[[i]][3] ,strsplit(thresholds$filename," ")[[i]][4])}
# read in threshold values from csv files of only features loaded
list = list()
for(i in 1:nrow(featurenames)){
  if(is.na(featurenames$filename[i])){
    list[[i]] = NA
  }
  if(!is.na(featurenames$filename[i])){
    list[[i]] = read.csv(featurenames$filename[i])
    list[[i]]$X = NULL # remove unecessary column
  }}
threshold_values = as.data.frame(unlist(list)) # unlist and turn to df
colnames(threshold_values) = "value"
# add to feature name df
featurenames$value = threshold_values$value
rm(threshold_values,list,i) # remove
thresholds$species_scientific = toupper(thresholds$species_scientific)

# add threshold filenames to feature names
featurenames = left_join(featurenames,thresholds)
rm(thresholds) #remove

# if using modelling thresholds
# turn all values below threshold to NA 
# this means turn any cell with a probability of presence for the species to a presence cells
# and all 0 probability cells become a§bsence cells

# first remove any layers for which you do not have a threshold value
# this probably means the modelling did not work
#omit = featurenames[is.na(featurenames$value),1] # identify column with feature name with NA value
#idx = which(names(feature_stack) == omit) # find out layer number with feature to omit
#feature_stack= dropLayer(feature_stack,idx)
#rm(idx,omit) # remove

if(thresholdmethod == "yes"){
  # threshold values per species
  for(i in 1:nlayers(feature_stack)){
    feature_stack[[i]][values(feature_stack[[i]]) < featurenames$value[i]] = NA
    feature_stack[[i]][values(!is.na(feature_stack[[i]]))] = 1}}

#############
