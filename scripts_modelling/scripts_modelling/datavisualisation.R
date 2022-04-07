# ---------------------------------------------------------------------------------
# AUTHORS: Nina Faure Beaulieu, Dr. Victoria Goodall (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST
# CONTACTs: ninab@wildtrust.co.za; victoria.goodall@mandela.ac.za 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script plots the models
# ---------------------------------


# ---------------------------------
# PACKAGES
# ---------------------------------
# list of required packages
requiredpackages = c("colorspace","rgeos","viridis","rasterVis","ggplot2","raster","stringr", "raster", "sf","sp", "dplyr", "lubridate","readxl","stringr")
# load packages
lapply(requiredpackages,require, character.only = TRUE)
rm(requiredpackages)
# ---------------------------------


# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------
# set directory to same parent folder where sub-scripts are found
# the subs-scripts can be in folders within this directory as the code will look through all the folders
my.directory = getwd()
# set directory
setwd(my.directory) 
# plot folder
if(!dir.exists("Outputs/modelling/prettyplots")){dir.create("Outputs/modelling/prettyplots")}
plotfolder = paste0(my.directory,"/Outputs/modelling/prettyplots/")
# ---------------------------------


# ---------------------------------
#  PLOTTING LAYERS
# this subscript prepares layers required for plotting and sets plotting parameters i.e. colour etc..
# ---------------------------------
source(list.files(pattern = "plottingparameters.R", recursive = TRUE, full.names= TRUE))
# ---------------------------------


# ---------------------------------
# DATA FILES
# ---------------------------------
sdms_rasters = list.files(pattern = "Aseasonal_res10_ensemblemean.tiff", recursive = TRUE, full.names =TRUE)
# list of species
master = read_xlsx(list.files(pattern = "data_summary_master.xlsx", recursive = TRUE,full.names = TRUE))
# thresholds
threshs = list.files(pattern = "thresh.csv", recursive = TRUE, full.names = TRUE)
# evaluations
evals = list.files(pattern = "allevals.csv", recursive = TRUE, full.names = TRUE)
# ---------------------------------


# ---------------------------------
# PLOTTING
# ---------------------------------
# find species where IUCN data file exists
iucn_available = vector()
for(i in 1:length(sdms_rasters)){
  target = str_split(sdms_rasters[i],"/")[[1]][5]
  target = str_split(target,"_")[[1]][1]
  exists3 = list.files(pattern = paste(target,".gpkg",sep=""), recursive = TRUE, ignore.case = TRUE)
  if(length(exists3) == 0){exists3 = NA}
  iucn_available = c(iucn_available,exists3)
}
noiucn_idx = which(is.na(iucn_available))
iucn_idx = which(!(is.na(iucn_available)))

# plots

for(i in sdms_rasters[iucn_idx]){
  target = str_split(i,"/")[[1]][5]
  target = str_split(target,"_")[[1]][1]
  model_type = str_split(i,"_")[[1]][2]
  temp = raster(i)
  values(temp) = values(temp)/1000
  res = str_split(i,"_")[[1]][3]
  thresh_id = which(str_detect(threshs,target))
  thresh_value = read.csv(threshs[thresh_id])
  thresh_value = (thresh_value$thresh)/1000
  eval_id = which(str_detect(evals,target))
  eval_value = read.csv(evals[eval_id])
  
  ################
  # get average cutoff from evaluations
  #eval_value = eval_value %>%
  #  filter(Testing.data>=0.7)%>%
  #  filter(Specificity>90)%>%
  #  filter(Sensitivity>90)%>%
  #  summarise(mean = mean(Cutoff),
  #            std = sd(Cutoff))
  #eval_value = (eval_value$mean)/1000
  # filter data at evaluations cutoff
  #temp_evalcut = temp
  #values(temp_evalcut)[which(values(temp_evalcut)<eval_value)] = NA
  #temp_evalcut = rasterToPolygons(temp_evalcut)
  #temp_evalcut = st_as_sf(temp_evalcut)
  #temp_evalcut <- st_simplify(temp_evalcut,dTolerance=100, preserveTopology = TRUE)
  #temp_evalcut = st_union(temp_evalcut)
  ################
  
  # filter data at threshold cutoff
  temp_threshcut = temp
  values(temp_threshcut)[which(values(temp_threshcut)<thresh_value)] = NA
  values(temp_threshcut)[which(values(temp_threshcut)>=thresh_value)] = 1
  
  #temp_threshcut = rasterToPolygons(temp_threshcut)
  #temp_threshcut = st_as_sf(temp_threshcut)
  #temp_threshcut <- st_simplify(temp_threshcut,dTolerance=100, preserveTopology = TRUE)
  #temp_threshcut= st_union(temp_threshcut)
  #temp_threshcut = as(temp_threshcut, Class = "Spatial")
  
  # turn values of 0 to NA
  values(temp)[values(temp)==0] = NA
  
  # species info
  spp_info = master %>%
    filter(SPECIES_SCIENTIFIC == target)
  
  # IUCN data
  exists3 = file.info(list.files(pattern = paste(target,".gpkg",sep=""), recursive = TRUE, ignore.case = TRUE))
  if(nrow(exists3 !=0)){
    iucn_extent = st_read(list.files(pattern = paste(target,".gpkg",sep=""), recursive = TRUE, ignore.case = TRUE))
    iucn_extent = as(iucn_extent, Class = "Spatial")}else{rm(iucn_extent)}
  rm(exists3)
  
  # convert target to sentence case for plotting
  target = str_to_sentence(target)
  
  # get endemic status
  if(spp_info$ENDEMIC.STATUS == 1){endemism = "South Africa endemic"}
  if(spp_info$ENDEMIC.STATUS == 0){endemism = "Not endemic"}
  if(spp_info$ENDEMIC.STATUS == 2){endemism = "Southern Africa endemic"}
  
  # plot 1: continuous distributions
plot = levelplot(temp,
                   # plot title in plot
                   main = bquote(italic(.(target))~","~.(spp_info$Species_common)~" - ["~.(spp_info$STATUS)~"] - ["~.(endemism)~"]"),
                   names.attr = c("Ensemble model"),
                   # purple to yellow colour scale bar
                   par.settings = rasterTheme(viridis_pal(option="D")(10)),
                   # intervals on scale bar
                   at = intervals/1000,
                   margin = FALSE,
                   xlab = NULL,
                   ylab=NULL)+
  # model type (aseasonal, summer, winter)
    #latticeExtra::layer(sp.text(c(16,-27.5),paste0(model_type," model"),cex = 1.5))+
    # threshold outline in yellow
    #latticeExtra::layer(sp.polygons(temp_threshcut, col = "yellow",lwd=1.5))+
    # eez
    latticeExtra::layer(sp.polygons(eez,col = "black",lwd = 0.8))+
    # 250m isobath
    latticeExtra::layer(sp.polygons(contours, col = "black", lwd = 1.5,lty = 3))+
    # sa coast
    latticeExtra::layer(sp.polygons(sa,col = "black",fill = "white",lwd= 1))+
    # points for main cities
    latticeExtra::layer(sp.points(places[c(1:3,5,6,18,20:22,10,14),],col = "black",pch = 20))+
    # coordinates and city names
    # done in three lines as a "pretty" position varies based on their place on the map
    latticeExtra::layer(sp.text(coordinates(places)[c(1:3,5,6),],places$Location[c(1:3,5,6)],col = "black",pch = 20,pos=4,cex = 0.5))+
    latticeExtra::layer(sp.text(coordinates(places)[c(18,20,21,22),],places$Location[c(18,20,21,22)],col = "black",pch = 20,pos=2,cex = 0.5))+
    latticeExtra::layer(sp.text(adjustedcoords,places$Location[c(10,14)],col = "black",pch = 20, pos=2,cex = 0.5))+
  # iucn extent
  latticeExtra::layer(sp.polygons(iucn_extent,col = "red",lwd = 1.5))+
  # legend
  latticeExtra::layer(sp.polygons(iucn_line,col = "red",lwd = 1))+
  latticeExtra::layer(sp.polygons(depth_line,col = "black",lwd = 1,lty = 3))+
  latticeExtra::layer(sp.text(c(36.2,-37.1),"250m contour",col = "black",pch = 20, pos=2,cex = 0.7))+
  latticeExtra::layer(sp.text(c(36,-37.6),"IUCN range",col = "red",pch = 20, pos=2,cex = 0.7))
  
  # this saves the plot to a folder
  png(file=paste0(plotfolder,target,"_",model_type,"_",res,"_continuous_ensemble.png"),width=3000, height=2000, res=300)
  print(plot)
  trellis.focus("legend", side="right", clipp.off=TRUE, highlight=FALSE)
  grid.text(expression("Probability of \noccurrence"), 0, 0, hjust=0.4, vjust=4, rot = 0, gp=gpar(fontsize=10))
  trellis.unfocus()
  dev.off()
  
  # plot 2: binary distributions (filtered by cutoff value)
  plot = levelplot(temp_threshcut,
                   main = bquote(italic(.(target))~","~.(spp_info$Species_common)~" - ["~.(spp_info$STATUS)~"] - ["~.(endemism)~"]"),
                   names.attr = c("Ensemble model"),
                   # purple to yellow colour scale bar
                   par.settings = rasterTheme(viridis_pal(option="D")(10)),
                   margin = FALSE,
                   xlab = NULL,
                   ylab=NULL,
                   colorkey = FALSE)+
    # model type
    #latticeExtra::layer(sp.text(c(16,-27.5),paste0(model_type," model"),cex = 1.5))+
    # threshold outline in yellow
    #latticeExtra::layer(sp.polygons(temp_threshcut, col = "yellow",lwd=1.5))+
    # eez
    latticeExtra::layer(sp.polygons(eez,col = "black",lwd = 1))+
    # 250m isobath
    latticeExtra::layer(sp.polygons(contours, col = "black", lwd = 1.5, lty = 3))+
    # sa coast
    latticeExtra::layer(sp.polygons(sa,col = "black",fill = "white",lwd= 1))+
    # points for main cities
    latticeExtra::layer(sp.points(places[c(1:3,5,6,18,20:22,10,14),],col = "black",pch = 20))+
    # coordinates and city names
    # done in three lines as a "pretty" position varies based on their place on the map
    latticeExtra::layer(sp.text(coordinates(places)[c(1:3,5,6),],places$Location[c(1:3,5,6)],col = "black",pch = 20,pos=4,cex = 0.5))+
    latticeExtra::layer(sp.text(coordinates(places)[c(18,20,21,22),],places$Location[c(18,20,21,22)],col = "black",pch = 20,pos=2,cex = 0.5))+
    latticeExtra::layer(sp.text(adjustedcoords,places$Location[c(10,14)],col = "black",pch = 20, pos=2,cex = 0.5))+
    # iucn extent
    latticeExtra::layer(sp.polygons(iucn_extent,col = "red",lwd = 1.5)) +
    # legend
    latticeExtra::layer(sp.polygons(iucn_line,col = "red",lwd = 1))+
    latticeExtra::layer(sp.polygons(depth_line,col = "black",lwd = 1,lty = 3))+
    latticeExtra::layer(sp.polygons(binary_line,col = viridis_pal(option="D")(10)[5],lwd = 5))+
    latticeExtra::layer(sp.text(c(36.2,-37.1),"250m contour",col = "black",pch = 20, pos=2,cex = 0.7))+
    latticeExtra::layer(sp.text(c(36,-37.6),"IUCN range",col = "red",pch = 20, pos=2,cex = 0.7))+
    latticeExtra::layer(sp.text(c(36.2,-36.5),"High probability\nof occurrence",col = viridis_pal(option="D")(10)[5],pch = 20, pos=2,cex = 0.7))
  
  # this saves the plot to a folder
  png(file=paste0(plotfolder,target,"_",model_type,"_",res,"_binary_ensemble.png"),width=3000, height=2000, res=300)
  print(plot)
  dev.off()
  rm(temp,plot,iucn_extent) # remove unnecessary variables
}

for(i in sdms_rasters[noiucn_idx]){
  target = str_split(i,"/")[[1]][5]
  target = str_split(target,"_")[[1]][1]
  model_type = str_split(i,"_")[[1]][2]
  temp = raster(i)
  values(temp) = values(temp)/1000
  res = str_split(i,"_")[[1]][3]
  thresh_id = which(str_detect(threshs,target))
  thresh_value = read.csv(threshs[thresh_id])
  thresh_value = (thresh_value$thresh)/1000
  eval_id = which(str_detect(evals,target))
  eval_value = read.csv(evals[eval_id])

  # get average cutoff from evaluations
  #eval_value = eval_value %>%
  #  filter(Testing.data>=0.7)%>%
  #  filter(Specificity>90)%>%
  #  filter(Sensitivity>90)%>%
  #  summarise(mean = mean(Cutoff),
  #            std = sd(Cutoff))
  #eval_value = (eval_value$mean)/1000
  
  # filter data at evaluations cutoff
  #temp_evalcut = temp
  #values(temp_evalcut)[which(values(temp_evalcut)<eval_value)] = NA
  #temp_evalcut = rasterToPolygons(temp_evalcut)
  #temp_evalcut = st_as_sf(temp_evalcut)
  #temp_evalcut <- st_simplify(temp_evalcut,dTolerance=100, preserveTopology = TRUE)
  #temp_evalcut = st_union(temp_evalcut)
  
  # filter data at threshold cutoff
  temp_threshcut = temp
  values(temp_threshcut)[which(values(temp_threshcut)<thresh_value)] = NA
  values(temp_threshcut)[which(values(temp_threshcut)>=thresh_value)] = 1
  
  #temp_threshcut = rasterToPolygons(temp_threshcut)
  #temp_threshcut = st_as_sf(temp_threshcut)
  #temp_threshcut <- st_simplify(temp_threshcut,dTolerance=100, preserveTopology = TRUE)
  #temp_threshcut= st_union(temp_threshcut)
  #temp_threshcut = as(temp_threshcut, Class = "Spatial")
  
  # turn values of 0 to NA
  values(temp)[values(temp)==0] = NA
  
  # species info
  spp_info = master %>%
    filter(SPECIES_SCIENTIFIC == target)
  
  # get endemic status
  if(spp_info$ENDEMIC.STATUS == 1){endemism = "South Africa endemic"}
  if(spp_info$ENDEMIC.STATUS == 0){endemism = "Not endemic"}
  if(spp_info$ENDEMIC.STATUS == 2){endemism = "Southern Africa endemic"}
  
  # convert target to sentence case for plotting
  target = str_to_sentence(target)
  
  # plot 1: continuous distributions
  plot = levelplot(temp,
                   main = bquote(italic(.(target))~","~.(spp_info$Species_common)~" - ["~.(spp_info$STATUS)~"] - ["~.(endemism)~"]"),
                   names.attr = c("Ensemble model"),
                   # blue color scale bar
                   #col.regions = rev(sequential_hcl(n=10)),
                   # purple to yellow colour scale bar
                   par.settings = rasterTheme(viridis_pal(option="D")(10)),
                   at = intervals/1000,
                   margin = FALSE,
                   xlab = NULL,
                   ylab=NULL)+
    # model type
    latticeExtra::layer(sp.text(c(16,-27.5),paste0(model_type," model"),cex = 1.5))+
    # threshold outline in yellow
    #latticeExtra::layer(sp.polygons(temp_threshcut, col = "yellow",lwd=1.5))+
    # eez
    latticeExtra::layer(sp.polygons(eez,col = "black",lwd = 1))+
    # 250m isobath
    latticeExtra::layer(sp.polygons(contours, col = "black", lwd = 1.5,lty = 3))+
    # sa coast
    latticeExtra::layer(sp.polygons(sa,col = "black",fill = "white",lwd= 1))+
    # points for main cities
    latticeExtra::layer(sp.points(places[c(1:3,5,6,18,20:22,10,14),],col = "black",pch = 20))+
    # coordinates and city names
    # done in three lines as a "pretty" position varies based on their place on the map
    latticeExtra::layer(sp.text(coordinates(places)[c(1:3,5,6),],places$Location[c(1:3,5,6)],col = "black",pch = 20,pos=4,cex = 0.5))+
    latticeExtra::layer(sp.text(coordinates(places)[c(18,20,21,22),],places$Location[c(18,20,21,22)],col = "black",pch = 20,pos=2,cex = 0.5))+
    latticeExtra::layer(sp.text(adjustedcoords,places$Location[c(10,14)],col = "black",pch = 20, pos=2,cex = 0.5))+
    # legend
    latticeExtra::layer(sp.polygons(depth_line,col = "black",lwd = 1,lty = 3))+
    latticeExtra::layer(sp.text(c(36.2,-37.1),"250m contour",col = "black",pch = 20, pos=2,cex = 0.7))
  
  # this saves the plot to a folder
  png(file=paste0(plotfolder,target,"_",model_type,"_",res,"_continuous_ensemble.png"),width=3000, height=2000, res=300)
  print(plot)
  trellis.focus("legend", side="right", clipp.off=TRUE, highlight=FALSE)
  grid.text(expression("Probability of \noccurrence"), 0, 0, hjust=0.4, vjust=4, rot = 0, gp=gpar(fontsize=10))
  trellis.unfocus()
  dev.off()
  
  # plot 2: binary distributions
  plot = levelplot(temp_threshcut,
                   main = bquote(italic(.(target))~","~.(spp_info$Species_common)~" - ["~.(spp_info$STATUS)~"] - ["~.(endemism)~"]"),
                   names.attr = c("Ensemble model"),
                   par.settings = rasterTheme(viridis_pal(option="D")(10)),
                   margin = FALSE,
                   xlab = NULL,
                   ylab=NULL,
                   colorkey = FALSE)+
    # model type
    latticeExtra::layer(sp.text(c(16,-27.5),paste0(model_type," model"),cex = 1.5))+
    # threshold outline in yellow
    #latticeExtra::layer(sp.polygons(temp_threshcut, col = "yellow",lwd=1.5))+
    # eez
    latticeExtra::layer(sp.polygons(eez,col = "black",lwd = 1))+
    # 250m isobath
    latticeExtra::layer(sp.polygons(contours, col = "black", lwd = 1.5,lty = 3))+
    # sa coast
    latticeExtra::layer(sp.polygons(sa,col = "black",fill = "white",lwd= 1))+
    # points for main cities
    latticeExtra::layer(sp.points(places[c(1:3,5,6,18,20:22,10,14),],col = "black",pch = 20))+
    # coordinates and city names
    # done in three lines as a "pretty" position varies based on their place on the map
    latticeExtra::layer(sp.text(coordinates(places)[c(1:3,5,6),],places$Location[c(1:3,5,6)],col = "black",pch = 20,pos=4,cex = 0.5))+
    latticeExtra::layer(sp.text(coordinates(places)[c(18,20,21,22),],places$Location[c(18,20,21,22)],col = "black",pch = 20,pos=2,cex = 0.5))+
    latticeExtra::layer(sp.text(adjustedcoords,places$Location[c(10,14)],col = "black",pch = 20, pos=2,cex = 0.5))+
    # legend
    latticeExtra::layer(sp.polygons(depth_line,col = "black",lwd = 1,lty = 3))+
    latticeExtra::layer(sp.text(c(36.2,-37.1),"250m contour",col = "black",pch = 20, pos=2,cex = 0.7))+
    latticeExtra::layer(sp.polygons(iucn_line,col = viridis_pal(option="D")(10)[5],lwd = 5))+
    latticeExtra::layer(sp.text(c(36.5,-37.7),"High probability\nof occurrence",col = viridis_pal(option="D")(10)[5],pch = 20, pos=2,cex = 0.7))
  
  # this saves the plot to a folder
  png(file=paste0(plotfolder,target,"_",model_type,"_",res,"_binary_ensemble.png"),width=3000, height=2000, res=300)
  print(plot)
  dev.off()
  rm(temp,plot,iucn_extent) # remove unnecessary variables
  }
# END OF SCRIPT ---------------------------------