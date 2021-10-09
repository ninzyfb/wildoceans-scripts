library("raster")
library("dismo")
library("rgeos")
library("rJava")
library("knitr")

utils::download.file(url = "https://raw.githubusercontent.com/mrmaxent/Maxent/master/ArchivedReleases/3.3.3k/maxent.jar", 
                     destfile = paste0(system.file("java", package = "dismo"), 
                                       "/maxent.jar"), mode = "wb") 

# plot the final occurrence data on the environmental layer
plot(variable_stack[[1]])
plot(pts_subset, add = T, col = "red")

# crop study area to a manageable extent (rectangle shaped)
studyArea <- crop(variable_stack, geographic.extent)  
plot(studyArea)

# the 'study area' created by extracting the buffer area from the raster stack
# studyArea <- mask(studyArea, occ_buff)

# save the new study area rasters as ascii
writeRaster(studyArea,
            # a series of names for output files
            filename=paste0("data/",names(studyArea),".asc"), 
            format="ascii", ## the output format
            bylayer=TRUE, ## this will save a series of layers
            overwrite=T)

# get the same random sample for training and testing
set.seed(1)

# randomly select 50% for training
selected <- sample(1:length(pts_subset), length(pts_subset) * 0.5)

pts_train <- pts_subset[selected, ]  # this is the selection to be used for model training
pts_test <- pts_subset[-selected, ]  # this is the opposite of the selection which will be used for model testing

# extracting env conditions for training occ from the raster
# stack; a data frame is returned (i.e multiple columns)
p <- extract(variable_stack, pts_train)
# env conditions for testing occ
p_test <- extract(variable_stack, pts_test)
# extracting env conditions for background
a <- extract(variable_stack, pts_background)

# repeat the number 1 as many numbers as the number of rows
# in p, and repeat 0 as the rows of background points
pa <- c(rep(1, nrow(p)), rep(0, nrow(a)))

# (rep(1,nrow(p)) creating the number of rows as the p data
# set to have the number '1' as the indicator for presence;
# rep(0,nrow(a)) creating the number of rows as the a data
# set to have the number '0' as the indicator for absence;
# the c combines these ones and zeros into a new vector that
# can be added to the Maxent table data frame with the
# environmental attributes of the presence and absence
# locations
pder <- as.data.frame(rbind(p, a))

# train Maxent with spatial data
# mod <- maxent(x=clim,p=occ_train)

# train Maxent with tabular data
mod <- maxent(x=pder, ## env conditions
              p=pa,   ## 1:presence or 0:absence
              
              path=paste0("data/maxent_outputs"), ## folder for maxent output; 
              # if we do not specify a folder R will put the results in a temp file, 
              # and it gets messy to read those. . .
              args=c("responsecurves") ## parameter specification
)

# the maxent functions runs a model in the default settings. To change these parameters,
# you have to tell it what you want...i.e. response curves or the type of features

# view the maxent model in a html brower
mod

# view detailed results
mod@results

# example 1, project to study area [raster]
ped1 <- predict(mod, studyArea)  # studyArea is the clipped rasters 
plot(ped1)  # plot the continuous prediction

# example 3, project with training occurrences [dataframes]
ped3 <- predict(mod, p)
head(ped3)

# using 'training data' to evaluate p & a are dataframe/s
# (the p and a are the training presence and background
# points)
mod_eval_train <- dismo::evaluate(p = p, a = a, model = mod)
print(mod_eval_train)


mod_eval_test <- dismo::evaluate(p = p_test, a = a, model = mod)
print(mod_eval_test)  # training AUC may be higher than testing AUC
