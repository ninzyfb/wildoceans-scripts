testing

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
files = list.files(path = paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs_Round3"),pattern = "Aseasonalensemblemean.tif", recursive = TRUE,full.names = TRUE)
feature_stack = stack()
for(i in 1:14){
  temp = raster(files[i])
  temp = projectRaster(temp,pu)
  feature_stack = addLayer(feature_stack,temp)
}
rm(files,temp) # remove unnecessary variables

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
# PLOTTING PARAMETERS
# ---------------------------------
# load grid for plotting
grid = shapefile(list.files(pattern = "5km_grid.shp",recursive = TRUE, full.names =TRUE))
# load mpas for solution plotting
mpas = shapefile(list.files(pattern ="SAMPAZ_OR_2020_Q3.shp" ,recursive = TRUE, full.names = TRUE))
mpas = gSimplify(mpas,tol = 0.01)
# three marine regions as defined by Ebert et al.
regions = shapefile(list.files(pattern = "ebert_regions.shp", recursive = TRUE,full.names = TRUE))
# turn region names to upper case
regions$Region = toupper(regions$Region)
# subset by east south and west
range = c("WEST","SOUTH","EAST")
# place names
places = shapefile(list.files(pattern="ebert_placenames.shp", recursive = TRUE, full.names=TRUE)) 
# eez
eez = shapefile(list.files(pattern="eez.shp", recursive = TRUE, full.names = TRUE)) # load eez
# adjusted coordinates for plotting
adjustedcoords = coordinates(places)[c(10,14),]
adjustedcoords[,2] = adjustedcoords[,2]+0.2
# set intervals for contours
intervals = seq(0,1,0.2)
# 250m isobath
contours = shapefile(list.files(pattern="contoursGEBCO.shp", recursive = TRUE, full.names=TRUE)) 
# convert contours to sf object
contours = st_as_sf(contours)
# group by depth and only keep depth of 250
contours = contours %>%
  group_by(DEPTH) %>%
  summarise() %>%
  filter(DEPTH == "-250")
# convert back to sp object
contours = as(contours, Class = "Spatial")
# provinces
sa  <- getData("GADM",country="South Africa",level=1)
# simplify provinces to reduce the weight of the file
sa <- gSimplify(sa, tol=0.01, topologyPreserve=TRUE)
sa = st_as_sf(sa) # convert to sf object
sa_coast = sa$geometry[c(1,4,8,9)] # only keep coastal provinces
sa_coast = as(sa_coast, Class = "Spatial") # convert to sp object


temp = feature_stack[[14]]
values(temp) = values(temp)/1000
names(feature_stack[[14]])

# PLOT
png(file=paste0(path,"Dropbox/6-WILDOCEANS/test.png"),width=3000, height=2000, res=300)
levelplot(temp,
          #main = paste0(target,"\n",model_type),
          #names.attr = c("Ensemble model"),
          par.settings = rasterTheme(viridis_pal(option="D")(10)),
          at = intervals,
          margin = FALSE)+
  latticeExtra::layer(sp.polygons(grid))+
  latticeExtra::layer(sp.polygons(eez,col = "black",lwd = 1))+
  latticeExtra::layer(sp.polygons(contours, col = "black", lwd = 1))+
  latticeExtra::layer(sp.polygons(sa_coast,col = "black",lwd= 1))+
  #latticeExtra::layer(sp.points(places[c(1:3,5,6,18,20:22,10,14),],col = "black",pch = 20))+
  latticeExtra::layer(sp.text(coordinates(places)[c(1:3,5,6),],places$Location[c(1:3,5,6)],col = "black",pch = 20,pos=4,cex = 0.5))+
  latticeExtra::layer(sp.text(coordinates(places)[c(18,20,21,22),],places$Location[c(18,20,21,22)],col = "black",pch = 20,pos=2,cex = 0.5))+
  latticeExtra::layer(sp.text(adjustedcoords,places$Location[c(10,14)],col = "black",pch = 20, pos=2,cex = 0.5))
dev.off()


