# ---------------------------------------------------------------------------------
######### Shark and ray species distribution models (SDMs) - pseudo-absence selection script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: creates pseudo-absences for the modelling stage

# ---------------------------------
# FORMATTING
# ---------------------------------
# pseudo-absences chosen following: Barbet-Messin et al., 2012

# create background points (static points)
# create 10000 background points
# from these 8500 will be chosen as pseudo-absences 
# 8500 represents 20% of EEZ cells at 5km res
# 2030 represents 20% of EEZ cells at 10km res
# code: length(which(values(template)==1))
# but this will be done later when formatting the data for modeling
# previous way i calculated background cells: 10*length(pts_sub)
absences = SpatialPoints(randomPoints(template, n_bckg_pts))

# extract environmental variables (static points)
pa = rbind(pts_sub,absences) # combine presences and absences
# extract environmental values
vars = as.data.frame(raster::extract(stack, pa))
# create a data frame
pa = as.data.frame(c(rep(1,length(pts_sub)),rep(0,length(absences)))) # add column of 1s and 0s for preseces and absences respectively
colnames(pa) = "pa" # rename that column pa (for presence absence)
coords = as.data.frame(rbind(coordinates(pts_sub),coordinates(absences))) # add coordinates to data frame
colnames(coords) = c("LONGITUDE","LATITUDE") # rename columns
pts_env = cbind(pa,coords,vars) # combine all three created dataframes into 1
rm(pts_sub,vars,pa, absences,coords) # remove unnecessary variables

# create background points (seasonal points)
# random background cells
if(seasonal == 'yes'){
absences = list()
for(i in 1:length(pts_sub_seasons)){
  cells = cellFromXY(template, pts_sub_seasons[[i]]) #  cells in template which overlap with an occurrence point
  values(template)[cells] = NA # turn presence cells to NA. This prevents presence cells being selected as background cells
  absences[[i]] = SpatialPoints(randomPoints(template, n_bckg_pts)) # select random background cells (the same number as presence cells)
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
