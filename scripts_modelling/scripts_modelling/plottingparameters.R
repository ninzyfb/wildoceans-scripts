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

# ---------------------------------
# FORMATTING
# ---------------------------------

# set intervals for contours
intervals = seq(0,1000,200)

# convert contours to sf object
contours = st_as_sf(contours)
# group by depth and only keep depth of 250
contours = contours %>%
  group_by(DEPTH) %>%
  summarise() %>%
  filter(DEPTH == "-250")
# convert back to sp object
contours = as(contours, Class = "Spatial")

# simplify provinces to reduce the weight of the file
sa <- gSimplify(sa, tol=0.01, topologyPreserve=TRUE)
sa = st_as_sf(sa) # convert to sf object
sa_coast = sa$geometry[c(1,4,8,9)] # only keep coastal provinces
sa_coast = as(sa_coast, Class = "Spatial") # convert to sp object
