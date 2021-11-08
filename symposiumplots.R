# define your path
# for me it changes based on if I am working on pc or mac
# path for pc =  "C:/Users/Administrator/"
path =  "/Users/nfb/"
setwd(paste0(path,"Dropbox/6-WILDOCEANS"))

# ---------------------------------
#  - CONSERVATION FEATURES
# output: stack of all sdms
# ---------------------------------
pu = raster(list.files(pattern = "template.tif",full.names = TRUE,recursive = TRUE))

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
feature_stack = mask(feature_stack,pu)

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
#  - PLOTTING
# ---------------------------------
# eez
eez = shapefile(list.files(pattern="eez.shp", recursive = TRUE, full.names = TRUE)) # load eez
# change names
names(feature_stack) = featurenames$species_scientific
intervals = seq(0,1000,200)
# pretty plots
# every temp is an sdm
values(feature_stack)[values(feature_stack)==0]=NA
p.strip <- list(cex=0.8, lines=1, col="black")
plot=levelplot(feature_stack[[17:24]],
          par.settings = rasterTheme(viridis_pal(option="D")(10)),
          at = intervals,
          margin = FALSE,
          colorkey = FALSE,
          scales=list(draw=FALSE),
          par.strip.text=p.strip)+
  latticeExtra::layer(sp.polygons(eez,col = "black",lwd = 1))

png(file=paste0(path,"Dropbox/6-WILDOCEANS/test.png"), width=2000, height=2000, res=300)
print(plot)
dev.off()
