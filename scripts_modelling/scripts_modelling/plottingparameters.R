# ---------------------------------------------------------------------------------
######### Shark and ray species distribution models (SDMs) - plotting parameters script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: loads any data required for plotting and sets the plotting parameters so that it is uniform across all plots

# ---------------------------------
# DATA
# ---------------------------------

# eez
eez = shapefile(list.files(pattern="eez.shp", recursive = TRUE, full.names = TRUE)) # load eez

# 250m isobath
contours = shapefile(list.files(pattern="contoursGEBCO.shp", recursive = TRUE, full.names=TRUE)) 

# provinces
sa  <- getData("GADM",country="South Africa",level=1)

# city names
places = shapefile(list.files(pattern="ebert_placenames.shp", recursive = TRUE, full.names=TRUE)) 

# three marine regions as defined by Ebert et al.
regions = shapefile(list.files(pattern = "ebert_regions.shp", recursive = TRUE,full.names = TRUE))

# load mpas for solution plotting
mpas = shapefile(list.files(pattern ="SAMPAZ_OR_2020_Q3.shp" ,recursive = TRUE, full.names = TRUE))

# ---------------------------------
# FORMATTING
# ---------------------------------

# turn region names to upper case
regions$Region = toupper(regions$Region)
# subset by east south and west
range = c("WEST","SOUTH","EAST")

# set intervals for contours
intervals = seq(0,1000,200)

# extract 250m isobath
contours = contours[which(contours$DEPTH=="-250"),]

# simplify provinces to reduce the weight of the file
sa <- gSimplify(sa, tol=0.01, topologyPreserve=TRUE)
# only keep coastal provinces
sa_coast=sa[c(1,4,8,9),]

# adjust coordinates for some places
adjustedcoords = coordinates(places)[c(10,14),]
adjustedcoords[,2] = adjustedcoords[,2]+0.2

# simplify mpas
mpas = gSimplify(mpas,tol = 0.01)

