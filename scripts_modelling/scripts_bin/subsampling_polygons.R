# ---------------------------------------------------------------------------------
######### Shark and ray species distribution models (SDMs) - subsampling script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: extracts a sub-sample from any data polygons to one occurrence per gridcell

# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling") # set to modelling folder

# ---------------------------------
# FORMATTING
# ---------------------------------

#get coordinates of grid cells that are overlaid by polygons
cells = unique(unlist(cellFromPolygon(template, obs.data_poly)))

# get dates for each polygon

# get coordinates of these cells from the template
pts_poly = xyFromCell(template, cells)
pts_poly = SpatialPoints(pts_poly) 

rm(cells) # remove unecessary variables


