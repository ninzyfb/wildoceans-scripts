#### Packages
library(sdm)

# Create SDM object using combined data
# pa stands for presence absence
sdm_obj = sdmData(pa~., train = combined)

sdm_obj # look at sdm object

# Create model (without training data)
m_test = sdm(pa~., data = sdm_obj, methods=c('glm','gam','brt','svm','bioclim','maxent','rf'))
m_test
getModelInfo(m_test)
# above, the performance statistics are calculated based on the data used to fit the model
# It is better to have an independent dataset (can be specified in the test argument of sdmData).
# However, there is often no such data available,
# Alternative solution: splitting/partitioning the dataset
# Splitting replicates: once or several replicates
# Splitting methods: several methods to do that: subsampling, cross-validation, bootsrapping

# Create model
m1 = sdm(pa~.,
           data = sdm_obj,
           # we fit 8 models
           methods = c('glm','gam','brt','svm','bioclim','maxent','rf'),
           # and using 30 percent of training data as test data
           test.percent = 30,
           # we subsample them
           replicatin = 'sub',
           # 2 times
           n = 2)

m1 # look at model
getModelInfo(m1) # info on runs: modelID, whether they are successfully fitted and evaluated
roc(m1) # generate the roc curve and compare the results for all models:
roc(m1, smooth=T) # the plots can be smooth 

# write all files as ascii files
# for some reason i can only get the prediction to work on ascii files at the moment
# in addition, the prediction only works if i did not turn raster values to factors
writeRaster(variable_stack,
            # a series of names for output files
            filename=paste0("data/",names(variable_stack),".asc"), 
            format="ascii", ## the output format
            bylayer=TRUE, ## this will save a series of layers
            overwrite=T)

lst <- list.files(path = "data/",pattern='.asc',full.names = T, recursive = TRUE) # list the name of files in the specified path,
variable_stack2 = stack(lst)
rm(path,lst)

# Prediction
# predict the habitat suitability into study area
# new data is a raster object so output is also a raster object
# Take the mean over all replications for each replication method

p1 <- predict(m1, newdata=variable_stack) # many commonly used raster format is supported (through the package rgdal)

# rename plots
list = list()
for(i in 1:length(names(p1))){
temp = strsplit(names(p1), split="_")[[i]][4]
list[i] = temp}
names = unlist(list)
names(p1) = names

# look at models
plot(p1,
     # make sure they all have same scale
     col = rev(terrain.colors(20)),
     breaks = seq(round(min(minValue(p1))), round(max(maxValue(p1))), length.out=5))
