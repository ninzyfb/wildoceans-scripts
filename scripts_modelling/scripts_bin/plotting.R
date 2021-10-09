# IMPORTANT: MODELLING OUTPUTS FOLDER NEEDS TO BE IN DIRECOTRY WHEN MAKING THIS FILES
# OTHERWISE THE PLOTTING DOES NOT WORK

# packages
library(rasterVis)
library(viridis)
library(sf)
library(sp)
library(biomod2)
library(latticeExtra)

# remove files from previous code
rm(biomod_obj_seasons,mxtPh,stack_subset)

# set intervals for contours
intervals = seq(0,1000,100)

# read all model projections in
files = list.files(pattern = "ensemble.rds", recursive = TRUE)
list = list()
for(i in 1:length(files)){
  list[[i]] = readRDS(file = files[i])}
rm(i)

# Save pretty plots
temp = get_predictions(list[[1]])[[1]]
type = strsplit(files[1],"_")[[1]][2]
png(file=paste0(target,"_",type,"_","ensemble.png"), width=3000, height=3000, res=300)
levelplot(temp,
          main = target,
          names.attr=c("Ensemble model"),
          par.settings = rasterTheme(viridis_pal(option="D")(10)),
          at=intervals,
          contour= TRUE,
          margin = FALSE)
dev.off()

temp = get_predictions(list[[2]])[[1]]
type = strsplit(files[2],"_")[[1]][2]
png(file=paste0(target,"_",type,"_","ensemble.png"), width=3000, height=3000, res=300)
levelplot(temp,
          main = target,
          names.attr=c("Ensemble model"),
          par.settings = rasterTheme(viridis_pal(option="D")(10)),
          at=intervals,
          contour= TRUE,
          margin = FALSE)
dev.off()

temp = get_predictions(list[[3]])[[1]]
type = strsplit(files[3],"_")[[1]][2]
png(file=paste0(target,"_",type,"_","ensemble.png"), width=3000, height=3000, res=300)
levelplot(temp,
          main = target,
          names.attr=c("Ensemble model"),
          par.settings = rasterTheme(viridis_pal(option="D")(10)),
          at=intervals,
          contour= TRUE,
          margin = FALSE)
dev.off()

temp = get_predictions(list[[4]])[[1]]
type = strsplit(files[4],"_")[[1]][2]
png(file=paste0(target,"_",type,"_","ensemble.png"), width=3000, height=3000, res=300)
levelplot(temp,
          main = target,
          names.attr=c("Ensemble model"),
          par.settings = rasterTheme(viridis_pal(option="D")(10)),
          at=intervals,
          contour= TRUE,
          margin = FALSE)
dev.off()

temp = get_predictions(list[[5]])[[1]]
type = strsplit(files[5],"_")[[1]][2]
png(file=paste0(target,"_",type,"_","ensemble.png"), width=3000, height=3000, res=300)
levelplot(temp,
          main = target,
          names.attr=c("Ensemble model"),
          par.settings = rasterTheme(viridis_pal(option="D")(10)),
          at=intervals,
          contour= TRUE,
          margin = FALSE)
dev.off()





















# save ensemble prediction to a raster stack
projections_static = get_predictions(en_static)
intervals = seq(0,1000,100)
levelplot(projections_static[[1]],main = target,names.attr=c("Ensemble model"),par.settings = BuRdTheme, at=intervals)



# save ensemble prediction to a raster stack
projections_autumn = get_predictions(en_autumn)

# save single prediction file to folder
model_autumn = subset(projections_autumn,1)
plot(model_autumn)
writeRaster(model_autumn,paste("sdms/",target,"model_autumn",".tif",sep=""), overwrite = TRUE)
model_autumn[model_autumn<500] = NA
writeRaster(model_autumn,paste("sdms/",target,"model_autumn_suitablehabitat",".tif",sep=""), overwrite = TRUE)




# save single prediction file to folder
model_static = subset(projections_static,1)
plot(model_static)
writeRaster(model_static,paste("sdms/",target,"model_static",".tif",sep=""), overwrite = TRUE)
model_static[model_static<500] = NA
writeRaster(model_static,paste("sdms/",target,"model_static_suitablehabitat",".tif",sep=""), overwrite = TRUE)

