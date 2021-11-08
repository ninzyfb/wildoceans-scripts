# TEMPORARY CODE TO REPLOT MODELS
#files = list.files(path = paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs_Round3"),pattern = "ensemblemean.tif", recursive = TRUE,full.names = TRUE)
#pu = raster(list.files(pattern = "template.tif",full.names = TRUE,recursive = TRUE))
#feature_stack = stack()
#for(i in 1:length(files)){
#  temp = raster(files[i])
#  temp = projectRaster(temp,pu)
#  feature_stack = addLayer(feature_stack,temp)
#}
#intervals = seq(0,1,0.2)
# this adjust the coordinates for text placement of algoa bay and mossel bay
#adjustedcoords = coordinates(places)[c(10,14),]
#adjustedcoords[,2] = adjustedcoords[,2]+0.2
#for(i in 1:nlayers(feature_stack)){
#temp = feature_stack[[i]]
#temp[values(temp) == 0] = NA # turn 0 values to NA
#values(temp) = values(temp)/1000
#name = strsplit(files[i],"/")[[1]][9]
#name = strsplit(name,"_")[[1]][1]
#season = strsplit(files[i],"/")[[1]][9]
#season = strsplit(season,"_")[[1]][2]
#season = strsplit(season,"ensemblemean.tif")[[1]][1]
#plot = levelplot(temp,
#                 xlab = NULL,
#                 ylab = NULL,
#                 main = paste0(name,"\n",season," model"),
#                 names.attr = c("Ensemble model"),
#                 par.settings = rasterTheme(viridis_pal(begin = 0.2,option="D")(10)),
#                 at = intervals,
#                 margin = FALSE)+
#  latticeExtra::layer(sp.polygons(eez,col = "black",lwd = 1))+
#  latticeExtra::layer(sp.polygons(contours, col = "black", lwd = 1))+
#  latticeExtra::layer(sp.polygons(sa_coast,col = "black",lwd= 0.5,alpha = 0.5))+
#  latticeExtra::layer(sp.points(places[c(1:3,5,6,18,20:22,10,14),],col = "black",pch=20,cex=0.6))+
#  latticeExtra::layer(sp.text(coordinates(places)[c(1:3,5,6),],txt = places$Location[c(1:3,5,6)],col = "black",pos=4,cex = 0.5))+
#  latticeExtra::layer(sp.text(coordinates(places)[c(18,20,21,22),],txt = places$Location[c(18,20,21,22)],col = "black",pos=2,cex = 0.5))+
#  latticeExtra::layer(sp.text(adjustedcoords,txt = places$Location[c(10,14)],col = "black",adj=0.5,pos=2,cex = 0.5))
#png(file=paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs/prettyplots/",name,"_",season,"_","continuous_ensemble.png"), width=3000, height=2000, res=300)
#print(plot)
#dev.off()
#rm(temp,plot) # remove unnecessary variables
#}
