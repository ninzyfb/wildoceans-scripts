# ---------------------------------------------------------------------------------
# AUTHORS: Nina Faure Beaulieu, Dr. Victoria Goodall (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST
# CONTACTs: ninab@wildtrust.co.za; victoria.goodall@mandela.ac.za 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script creates pseudo-absences for the modelling stage
# ---------------------------------


# ---------------------------------
# FORMATTING
# ---------------------------------
# pick number of background points to choose from during model development (we went with 20% of modelling surface)
# pseudo-absences choice procedure: Barbet-Messin et al., 2012
if(res == 5){n_bckg_pts = 0.2*length(which(values(!is.na(stack_subset[[1]]))))}
if(res == 10){n_bckg_pts = 0.2*length(which(values(!is.na(stack_subset[[1]]))))}

# isolate these n background points randomly across all cells in the EEZ using randomPoints()
cells = randomPoints(stack_subset, n_bckg_pts,cellnumbers=TRUE, ext = extent(stack_subset))

# get xy coordinates from these cells
absences = SpatialPoints(xyFromCell(stack_subset[[1]],cells),bbox =  bbox(stack_subset))

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

# extract environmental values from raster stack at coordinates of presence and background points
vars = as.data.frame(raster::extract(stack_subset, pa[,c(2,3)]))

# add to dataframe
pts_env = cbind(pa,vars) 

rm(pts_sub,vars,pa, absences)

# create background points (seasonal points)
# random background cells
if(seasonal == 'yes' & !is.na(seasonal)){
absences = list()
for(i in 1:length(pts_sub_seasons)){
  cells = randomPoints(stack_subset, n_bckg_pts,cellnumbers=TRUE, ext = extent(stack_subset))
  absences[[i]] = SpatialPoints(xyFromCell(stack_subset[[1]],cells),bbox =  bbox(stack_subset))
  crs(absences[[i]]) = crs(stack_subset)
  rm(cells)}
rm(i) 

# extract environmental variables (seasonal points)
pts_env_seasons = list()
for(i in 1:length(pts_sub_seasons)){ # same as for static model but carry it out for both seasons in the list
  pa = rbind(pts_sub_seasons[[i]],absences[[i]])
  vars = as.data.frame(raster::extract(stack_subset, pa))
  pa = as.data.frame(c(rep(1,length(pts_sub_seasons[[i]])),rep(0,length(absences[[i]]))))
  colnames(pa) = "pa"
  coords = as.data.frame(rbind(coordinates(pts_sub_seasons[[i]]),coordinates(absences[[i]])))
  colnames(coords) = c("LONGITUDE","LATITUDE")
  temp = cbind(pa,coords,vars)
  pts_env_seasons[[i]] = temp
  rm(pa,vars,coords,temp) 
}

rm(i,pts_sub_seasons,absences)}
