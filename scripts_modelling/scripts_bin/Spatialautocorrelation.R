# directory
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling") # set directory to parent modelling folder

# packages
library(spatstat)
library(maptools)

pp = as.ppp.SpatialPoints(pts)

ripley = Kest(pp)

plot(ripley)


coordinates = pts@coords
nb = tri2nb(coords,row.names=NULL)
presences = rep(1,length(nb))
moran.test(presences,nb2listw(nb))


distances = as.matrix(dist(coordinates))

distances.inv = 1/distances

diag(distances.inv) = 0

p = rep(1,nrow(distances.inv))

Moran.I(p,distances.inv)

# package
library(raster)

# overlayraster
r_desiredres = raster(list.files(pattern = "template.tif", recursive = TRUE))

# convert points to grid cells
cells = cellFromXY(r_desiredres, pts)
values(r_desiredres) = NA
values(r_desiredres)[cells] = 1

# compute morans i
Moran(r_desiredres)

?Moran
r <- raster(nrows=10, ncols=10)
values(r) <- 1:ncell(r)
plot(r)
