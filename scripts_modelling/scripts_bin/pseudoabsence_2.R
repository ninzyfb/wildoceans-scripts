#### Packages
library(fuzzySim) 
library(dismo)

# STATIC

# Compute inverse distance weighted interpolation and create raster with density layer
?geoIDW
idw <- geoIDW(as.matrix(pts_sub@coords), 
              randomPoints(stack[[1]], length(pts_sub)))
idw_r <- predict(template, idw)
# Re-classify density values below 0.05 indicating a 1/20 of the sampling effort
#values(idw_r)[values(idw_r) < 0.05] <- 0.05
# Clip to eez
idw_r <- mask(idw_r,template)
plot(idw_r)
# Randomly select background data proportional to sampling density in presence data
# prob = T means that the values are interpreted as probability weights
bg_idw <- randomPoints(idw_r, length(pts_sub), prob=T)
rm(idw_r, idw)

#### combine presence and absence points + extract envt variables
absences = SpatialPoints(bg_idw) # select random background cells (the same number as presence cells)
pa = rbind(pts_sub,absences)
vars = as.data.frame(extract(stack, pa))
pa = as.data.frame(c(rep(1,length(pts_sub)),rep(0,length(absences))))
colnames(pa) = "pa"
coords = as.data.frame(rbind(coordinates(pts_sub),coordinates(absences)))
colnames(coords) = c("LONGITUDE","LATITUDE")
pts_env = cbind(pa,coords,vars)
rm(pts_sub,vars,pa, absences,coords,bg_idw)

# SEASONAL

#### create background points (seasonal points)
absences = list()
for(i in 1:length(pts_sub_seasons)){
  # Compute inverse distance weighted interpolation and create raster with density layer
  idw <- geoIDW(as.matrix(pts_sub_seasons[[i]]@coords), 
                randomPoints(template, length(pts_sub_seasons[[i]])))
  idw_r <- predict(template, idw)
  # Re-classify density values below 0.05 indicating a 1/20 of the sampling effort
  #values(idw_r)[values(idw_r) < 0.05] <- 0.05
  # Clip to eez
  idw_r <- mask(idw_r,stack[[1]])
  # Randomly select background data proportional to sampling density in presence data
  bg_idw <- randomPoints(idw_r, length(pts_sub_seasons[[i]]), prob=T)
  rm(idw_r, idw)
  absences[[i]] = SpatialPoints(bg_idw) # select random background cells (the same number as presence cells)
  }
rm(i,bg_idw)
#### Extract values from environmental values for each pts dataset (seasonal points)
pts_env_seasons = list()
for(i in 1:length(pts_sub_seasons)){
  pa = rbind(pts_sub_seasons[[i]],absences[[i]])
  vars = as.data.frame(extract(stack, pa))
  pa = as.data.frame(c(rep(1,length(pts_sub_seasons[[i]])),rep(0,length(absences[[i]]))))
  colnames(pa) = "pa"
  coords = as.data.frame(rbind(coordinates(pts_sub_seasons[[i]]),coordinates(absences[[i]])))
  colnames(coords) = c("LONGITUDE","LATITUDE")
  temp = cbind(pa,coords,vars)
  pts_env_seasons[[i]] = temp
  rm(pa,vars,coords,temp)
}

rm(i,pts_sub_seasons,absences)
