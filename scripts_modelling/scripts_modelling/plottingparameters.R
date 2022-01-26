# ---------------------------------------------------------------------------------
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script loads any features required for plotting
# This script also sets some plotting parameters to ensure uniform plots
# ---------------------------------


# ---------------------------------
# PLOTTING FEATURES
# ---------------------------------
# South African EEZ
eez = shapefile(list.files(pattern="eez.shp", recursive = TRUE, full.names = TRUE)) # load eez

# isobaths
contours = shapefile(list.files(pattern="contoursGEBCO.shp", recursive = TRUE, full.names=TRUE)) 

# South African province outline
sa  <- getData("GADM",country="South Africa",level=1)

# Names of coastal cities
places = shapefile(list.files(pattern="ebert_placenames.shp", recursive = TRUE, full.names=TRUE)) 

# West, South and East marine regions as defined by Ebert et al.
regions = shapefile(list.files(pattern = "ebert_regions.shp", recursive = TRUE,full.names = TRUE))

# South African continetnal marine protected areas
mpas = shapefile(list.files(pattern ="SAMPAZ_OR_2020_Q3.shp" ,recursive = TRUE, full.names = TRUE))
# ---------------------------------

# ---------------------------------
# FORMATTING
# ---------------------------------

# turn region names to upper case
regions$Region = toupper(regions$Region)

# create west south and east vector
# this will come into handdy during plotting
range = c("WEST","SOUTH","EAST")

# extract 250m isobath
contours = contours[which(contours$DEPTH=="-250"),]

# simplify provinces to reduce the weight of the file
sa <- gSimplify(sa, tol=0.01, topologyPreserve=TRUE)
# only keep coastal provinces
sa_coast=sa[c(1,4,8,9),]

# adjust coordinates for some cities 
# this is to ensure the name is displayed in a "pretty" way on the map
adjustedcoords = coordinates(places)[c(10,14),]
adjustedcoords[,2] = adjustedcoords[,2]+0.2

# simplify MPA shapefile
mpas = gSimplify(mpas,tol = 0.01)

# set intervals for modelling
intervals = seq(0,1000,200)
