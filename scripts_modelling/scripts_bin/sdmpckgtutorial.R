#### Packages
library(sdm)
library(raster)
library(rgdal)
library(rgeos)

# SPECIES DATA
file <- system.file("external/species.shp", package="sdm") # get the location of the species shapefile in
species = shapefile(file) # read species data
rm(file)

# PREDICTOR VARIABLES
path <- system.file("external", package="sdm") # path to the folder contains the data
lst <- list.files(path=path,pattern='asc$',full.names = T) # list the name of files in the specified path,
preds = stack(lst)
rm(path,lst)

# CREATE SDM OBJECT
sdm = sdmData(formula = Occurrence~., train = species, predictors = preds)

# CREATE MODEL OBJECT
m = sdm(Occurrence ~.,data= sdm, methods=c('glm','rf','tree','fda','mars','svm'), replicatin='sub',test.percent=30,n=2)

# PREDICT
p1 <- predict(m, newdata=preds,filename='p1.img') # many commonly used raster format is supported (through the package rgdal)

