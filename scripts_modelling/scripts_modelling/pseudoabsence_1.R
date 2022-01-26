# ---------------------------------------------------------------------------------
# AUTHOR: Nina Faure Beaulieu (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script creates pseudo-absences for the modelling stage
# ---------------------------------


# ---------------------------------
# FORMATTING
# ---------------------------------
# number of background points chosen represents 20% of cells in the EEZ
# pseudo-absences choice procedure: Barbet-Messin et al., 2012
# choose random cells across the EEZ using randomPoints()
cells = randomPoints(stack, n_bckg_pts,cellnumbers=TRUE, ext = extent(stack))

# get xy coordinates from these cells
absences = SpatialPoints(xyFromCell(stack[[1]],cells),bbox =  bbox(stack))

# set crs to match occurrence points
crs(absences) = crs(pts_sub)

# combine presences and background points in single object
# object names pa from presenceabsence even though they are not true absences
pa = rbind(pts_sub,absences) 

# create a data frame 
# add column of 1s and 0s for presences and background points respectively
pa = as.data.frame(c(rep(1,length(pts_sub)),rep(0,length(absences)))) 

# rename that column pa (for presence absence)
colnames(pa) = "pa" 

# add coordinates to data frame
pa = cbind(pa,rbind(coordinates(pts_sub),coordinates(absences)))

# rename columns
colnames(pa)[c(2,3)] = c("LONGITUDE","LATITUDE") 

# extract environmental values from raster stack at presence and background points
vars = as.data.frame(raster::extract(stack, pa))

# add to dataframe
pts_env = cbind(pa,vars) 

rm(pts_sub,vars,pa, absences,coords)

# create background points (seasonal points)
# random background cells
if(seasonal == 'yes'){
absences = list()
for(i in 1:length(pts_sub_seasons)){
  cells = randomPoints(stack, n_bckg_pts,cellnumbers=TRUE, ext = extent(stack))
  absences[[i]] = SpatialPoints(xyFromCell(stack[[1]],cells),bbox =  bbox(stack))
  crs(absences[[i]]) = crs(pts_sub)
  rm(cells)} # remove unnecessary variables
rm(i) # remove unnecessary variables

# extract environmental variables (seasonal points)
pts_env_seasons = list()
for(i in 1:length(pts_sub_seasons)){ # same as for static model but carry it out for both seasons in the list
  pa = rbind(pts_sub_seasons[[i]],absences[[i]])
  vars = as.data.frame(raster::extract(stack, pa))
  pa = as.data.frame(c(rep(1,length(pts_sub_seasons[[i]])),rep(0,length(absences[[i]]))))
  colnames(pa) = "pa"
  coords = as.data.frame(rbind(coordinates(pts_sub_seasons[[i]]),coordinates(absences[[i]])))
  colnames(coords) = c("LONGITUDE","LATITUDE")
  temp = cbind(pa,coords,vars)
  pts_env_seasons[[i]] = temp
  rm(pa,vars,coords,temp) # remove unnecessary variables
}

rm(i,pts_sub_seasons,absences)} # remove unnecessary variables
