# ---------------------------------------------------------------------------------
# AUTHORS: Nina Faure Beaulieu, Dr. Victoria Goodall (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST
# CONTACTs: ninab@wildtrust.co.za; victoria.goodall@mandela.ac.za 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script plots the models
# for every species there will be four plots
# 1 - plot of continuous distribution with occurrence points
# 2 - plot of continuous distribution without occurrence points
# 3 - plot of binary distribution with occurrence points
# 4 - plot of binary distribution without occurrence points
# ---------------------------------


# ---------------------------------
# PACKAGES
# ---------------------------------
# list of required packages
requiredpackages = c("grid","colorspace","rgeos","viridis","rasterVis","ggplot2","raster","stringr", "raster", "sf","sp", "dplyr", "lubridate","readxl","stringr")
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
if(!dir.exists("Outputs/modelling/distribution_plots")){dir.create("Outputs/modelling/distribution_plots")}
plotfolder = paste0(my.directory,"/Outputs/modelling/distribution_plots/")
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
# list of species
master = read_xlsx(list.files(pattern = "data_summary_master.xlsx", recursive = TRUE,full.names = TRUE))
# !! make sure the number of rasters, threshs and evals are the same
# sdm rasters
sdms_rasters = list.files(pattern = "Aseasonal_res10_ensemblemean.tiff", recursive = TRUE, full.names =TRUE)
# thresholds
threshs = list.files(pattern = "thresh.csv", recursive = TRUE, full.names = TRUE)
# evaluations
evals = list.files(pattern = "allevals.csv", recursive = TRUE, full.names = TRUE)
# ---------------------------------


# ---------------------------------
# PROBLEM SPECIES
# ---------------------------------
source(list.files(pattern = "pelagic_spp.R", recursive = TRUE))
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

# simplify file names to get species name only
iucn_available = str_replace(iucn_available,"IUCN/Sharks_rays_SA_raw/","")
iucn_available = toupper(str_replace(iucn_available,".gpkg",""))
# species where IUCN range is available or not
iucn_idx = sdms_rasters[!is.na(str_detect(sdms_rasters,iucn_available))]
noiucn_idx = sdms_rasters[is.na(str_detect(sdms_rasters,iucn_available))]

rm(iucn_available, exists3,target,i)

# plotting of maps without IUCN range
for(i in 1:length(iucn_idx)){
  file = iucn_idx[i]
  # species name
  target = str_split(file,"/")[[1]][5]
  target = str_split(target,"_")[[1]][1]
  # only plot if not a problem species
  if(!(target %in% problem_species_all)){
  # model type
  model_type = str_split(file,"_")[[1]][2]
  
  # folder with occurrence data files (all stored as R object)
  folder = "speciesdata/"
  # run script to load species data
  exampledata = "no"
  source(list.files(pattern = "species_data.R", recursive = TRUE)) 
  rm(folder)
  
  # sdm raster
  temp = raster(iucn_idx[i])
  values(temp) = values(temp)/1000
  
  # threshold value to binarize sdm
  thresh_id = which(str_detect(threshs,target))
  thresh_value = read.csv(threshs[thresh_id])
  thresh_value = (thresh_value$thresh)/1000
  
  # evaluations
  eval_id = which(str_detect(evals,target))
  eval_value = read.csv(evals[eval_id])
  rm(thresh_id,eval_id)
  
  # filter data at threshold cutoff
  temp_threshcut = temp
  values(temp_threshcut)[which(values(temp_threshcut)<thresh_value)] = NA
  values(temp_threshcut)[which(values(temp_threshcut)>=thresh_value)] = 1
  
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
  if(spp_info$ENDEMIC.STATUS == 1){endemism = "South Africa"}
  if(spp_info$ENDEMIC.STATUS == 0){endemism = "Not endemic"}
  if(spp_info$ENDEMIC.STATUS == 2){endemism = "Southern Africa"}
  if(spp_info$ENDEMIC.STATUS == "Unknown"){endemism = "Unknown"}
  
  # plot 1: continuous distributions
plot = levelplot(temp,
                   # plot title in plot
                   #main = bquote(italic(.(target))~","~.(spp_info$Species_common)~" - ["~.(spp_info$STATUS)~"] - ["~.(endemism)~"]"),
                   names.attr = c("Ensemble model"),
                   # purple to yellow colour scale bar
                   par.settings = rasterTheme(viridis_pal(option="D")(10)),
                   # intervals on scale bar
                   at = intervals/1000,
                   margin = FALSE,
                   xlab = NULL,
                   ylab=NULL,
                 colorkey = list(space ="bottom", width = 1)
                )+
    # mpa outline
  latticeExtra::layer(sp.polygons(mpas,col = "black",lwd = 0.8))+
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
  latticeExtra::layer(sp.polygons(iucn_extent,col = "red",lwd = 0.8))+
  # legend
  latticeExtra::layer(sp.polygons(iucn_line,col = "red",lwd = 1))+
  latticeExtra::layer(sp.polygons(depth_line,col = "black",lwd = 1,lty = 3))+
  latticeExtra::layer(sp.text(c(36.2,-37.1),"250m contour",col = "black",pch = 20, pos=2,cex = 0.7))+
  latticeExtra::layer(sp.text(c(36,-37.6),"IUCN range",col = "red",pch = 20, pos=2,cex = 0.7))+
# box on top for title
latticeExtra::layer(sp.polygons(tb_top, fill = "white"))+latticeExtra::layer(sp.text(c(20,-27.3),bquote(italic(.(target))~","~.(spp_info$Species_common)),col = "black",pch = 20, cex = 1,adj=0))+
  latticeExtra::layer(sp.text(c(20,-27.7),"Red List status:",col = "black",pch = 20,font = 2,cex = 1,adj=0))+
  latticeExtra::layer(sp.text(c(23.2,-27.7),bquote(.(spp_info$STATUS)),col = "black",pch = 20, cex = 1,adj=0))+
  latticeExtra::layer(sp.text(c(23.9,-27.7),"- Endemic status:",col = "black",pch = 20,font = 2,cex = 1,adj=0))+
  latticeExtra::layer(sp.text(c(27.5,-27.7),bquote(.(endemism)),col = "black",pch = 20, cex = 1,adj=0))
  
  # save plot to folder
  png(file=paste0(plotfolder,target,"_",model_type,"_res10_continuous_ensemble.png"),width=3000, height=2000, res=300)
  print(plot)
  trellis.focus("legend", side="bottom", clipp.off=TRUE, highlight=FALSE)
  grid.text(expression("Probability of occurrence"), -0.05, 0.2, hjust=-0.4, vjust=0, rot = 0, gp=gpar(fontsize=10))
  trellis.unfocus()
  dev.off()
  
  # save plot again with points
  plot = plot + latticeExtra::layer(sp.points(obs.data,col = "red",pch = 4, cex=0.5))
  png(file=paste0(plotfolder,target,"_",model_type,"_res10_withpoints_continuous_ensemble.png"),width=3000, height=2000, res=300)
  print(plot)
  trellis.focus("legend", side="bottom", clipp.off=TRUE, highlight=FALSE)
  grid.text(expression("Probability of occurrence"), -0.05, 0.2, hjust=-0.4, vjust=0, rot = 0, gp=gpar(fontsize=10))
  trellis.unfocus()
  dev.off()
  
  
  # plot 2: binary distributions (filtered by cutoff value)
  plot = levelplot(temp_threshcut,
                  #main = bquote(italic(.(target))~","~.(spp_info$Species_common)~" - ["~.(spp_info$STATUS)~"] - ["~.(endemism)~"]"),
                   names.attr = c("Ensemble model"),
                   # purple to yellow colour scale bar
                   par.settings = rasterTheme(viridis_pal(option="D")(10)),
                   margin = FALSE,
                   xlab = NULL,
                   ylab=NULL,
                   colorkey = FALSE)+
    # mpa outline
    latticeExtra::layer(sp.polygons(mpas,col = "black",lwd = 0.8))+
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
    latticeExtra::layer(sp.polygons(iucn_extent,col = "red",lwd = 0.8)) +
    # legend
    latticeExtra::layer(sp.polygons(iucn_line,col = "red",lwd = 1))+
    latticeExtra::layer(sp.polygons(depth_line,col = "black",lwd = 1,lty = 3))+
    latticeExtra::layer(sp.polygons(binary_line,col = viridis_pal(option="D")(10)[5],lwd = 5))+
    latticeExtra::layer(sp.text(c(36.2,-37.1),"250m contour",col = "black",pch = 20, pos=2,cex = 0.7))+
    latticeExtra::layer(sp.text(c(36,-37.6),"IUCN range",col = "red",pch = 20, pos=2,cex = 0.7))+
    latticeExtra::layer(sp.text(c(36.2,-36.5),"High probability\nof occurrence",col = viridis_pal(option="D")(10)[5],pch = 20, pos=2,cex = 0.7))+
    # box on top for title
    latticeExtra::layer(sp.polygons(tb_top, fill = "white"))+latticeExtra::layer(sp.text(c(20,-27.3),bquote(italic(.(target))~","~.(spp_info$Species_common)),col = "black",pch = 20, cex = 1,adj=0))+
    latticeExtra::layer(sp.text(c(20,-27.7),"Red List status:",col = "black",pch = 20,font = 2,cex = 1,adj=0))+
    latticeExtra::layer(sp.text(c(23.2,-27.7),bquote(.(spp_info$STATUS)),col = "black",pch = 20, cex = 1,adj=0))+
    latticeExtra::layer(sp.text(c(23.9,-27.7),"- Endemic status:",col = "black",pch = 20,font = 2,cex = 1,adj=0))+
    latticeExtra::layer(sp.text(c(27.5,-27.7),bquote(.(endemism)),col = "black",pch = 20, cex = 1,adj=0))
  
  # this saves the plot to a folder
  png(file=paste0(plotfolder,target,"_",model_type,"_res10_binary_ensemble.png"),width=3000, height=2000, res=300)
  print(plot)
  dev.off()
  
  # this saves the plot to a folder
  plot = plot + latticeExtra::layer(sp.points(obs.data,col = "red",pch = 4, cex=0.5))
  png(file=paste0(plotfolder,target,"_",model_type,"_res10_withpoints_binary_ensemble.png"),width=3000, height=2000, res=300)
  print(plot)
  dev.off()
  rm(temp,plot,iucn_extent,obs.data) # remove unnecessary variables
}}

for(i in 1:length(noiucn_idx)){
  
  file = noiucn_idx[i]
  # species name
  target = str_split(file,"/")[[1]][5]
  target = str_split(target,"_")[[1]][1]
  # model type
  model_type = str_split(file,"_")[[1]][2]
  
  # only plot if not a problem species
  if(!(target %in% problem_species_all)){
  
  # folder with occurrence data files (all stored as R object)
  folder = "speciesdata/"
  # run script to load species data
  exampledata = "no"
  source(list.files(pattern = "species_data.R", recursive = TRUE)) 
  rm(folder)
  
  # sdm raster
  temp = raster(iucn_idx[i])
  values(temp) = values(temp)/1000
  
  # threshold value to binarize sdm
  thresh_id = which(str_detect(threshs,target))
  thresh_value = read.csv(threshs[thresh_id])
  thresh_value = (thresh_value$thresh)/1000
  
  # evaluations
  eval_id = which(str_detect(evals,target))
  eval_value = read.csv(evals[eval_id])
  rm(thresh_id,eval_id)
  
  # filter data at threshold cutoff
  temp_threshcut = temp
  values(temp_threshcut)[which(values(temp_threshcut)<thresh_value)] = NA
  values(temp_threshcut)[which(values(temp_threshcut)>=thresh_value)] = 1

  # species info
  spp_info = master %>%
    filter(SPECIES_SCIENTIFIC == target)
  
  # get endemic status
  if(spp_info$ENDEMIC.STATUS == 1){endemism = "South Africa"}
  if(spp_info$ENDEMIC.STATUS == 0){endemism = "Not endemic"}
  if(spp_info$ENDEMIC.STATUS == 2){endemism = "Southern Africa"}
  
  # convert target to sentence case for plotting
  target = str_to_sentence(target)
  
  # plot 1: continuous distributions
  plot = levelplot(temp,
                   #main = bquote(italic(.(target))~","~.(spp_info$Species_common)~" - ["~.(spp_info$STATUS)~"] - ["~.(endemism)~"]"),
                   names.attr = c("Ensemble model"),
                   # blue color scale bar
                   #col.regions = rev(sequential_hcl(n=10)),
                   # purple to yellow colour scale bar
                   par.settings = rasterTheme(viridis_pal(option="D")(10)),
                   at = intervals/1000,
                   margin = FALSE,
                   xlab = NULL,
                   ylab=NULL,
                   colorkey = list(space ="bottom", width = 1))+
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
  # box on top for title
  latticeExtra::layer(sp.polygons(tb_top, fill = "white"))+latticeExtra::layer(sp.text(c(20,-27.3),bquote(italic(.(target))~","~.(spp_info$Species_common)),col = "black",pch = 20, cex = 1,adj=0))+
    latticeExtra::layer(sp.text(c(20,-27.7),"Red List status:",col = "black",pch = 20,font = 2,cex = 1,adj=0))+
    latticeExtra::layer(sp.text(c(23.2,-27.7),bquote(.(spp_info$STATUS)),col = "black",pch = 20, cex = 1,adj=0))+
    latticeExtra::layer(sp.text(c(23.9,-27.7),"- Endemic status:",col = "black",pch = 20,font = 2,cex = 1,adj=0))+
    latticeExtra::layer(sp.text(c(27.5,-27.7),bquote(.(endemism)),col = "black",pch = 20, cex = 1,adj=0))
  
  # this saves the plot to a folder
  png(file=paste0(plotfolder,target,"_",model_type,"_res10_continuous_ensemble.png"),width=3000, height=2000, res=300)
  print(plot)
  trellis.focus("legend", side="bottom", clipp.off=TRUE, highlight=FALSE)
  grid.text(expression("Probability of occurrence"), -0.05, 0.2, hjust=-0.4, vjust=0, rot = 0, gp=gpar(fontsize=10))
  trellis.unfocus()
  dev.off()
  
  # this saves the plot to a folder
  plot = plot + latticeExtra::layer(sp.points(obs.data,col = "red",pch = 4, cex=0.5))
  png(file=paste0(plotfolder,target,"_",model_type,"_res10_withpoints_continuous_ensemble.png"),width=3000, height=2000, res=300)
  print(plot)
  dev.off()
  
  # plot 2: binary distributions
  plot = levelplot(temp_threshcut,
                   #main = bquote(italic(.(target))~","~.(spp_info$Species_common)~" - ["~.(spp_info$STATUS)~"] - ["~.(endemism)~"]"),
                   names.attr = c("Ensemble model"),
                   par.settings = rasterTheme(viridis_pal(option="D")(10)),
                   margin = FALSE,
                   xlab = NULL,
                   ylab=NULL,
                   colorkey = FALSE)+
    # model type
    latticeExtra::layer(sp.text(c(16,-27.5),paste0(model_type," model"),cex = 1.5))+
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
    latticeExtra::layer(sp.text(c(36.5,-37.7),"High probability\nof occurrence",col = viridis_pal(option="D")(10)[5],pch = 20, pos=2,cex = 0.7))+
  # box on top for title
  latticeExtra::layer(sp.polygons(tb_top, fill = "white"))+latticeExtra::layer(sp.text(c(20,-27.3),bquote(italic(.(target))~","~.(spp_info$Species_common)),col = "black",pch = 20, cex = 1,adj=0))+
    latticeExtra::layer(sp.text(c(20,-27.7),"Red List status:",col = "black",pch = 20,font = 2,cex = 1,adj=0))+
    latticeExtra::layer(sp.text(c(23.2,-27.7),bquote(.(spp_info$STATUS)),col = "black",pch = 20, cex = 1,adj=0))+
    latticeExtra::layer(sp.text(c(23.9,-27.7),"- Endemic status:",col = "black",pch = 20,font = 2,cex = 1,adj=0))+
    latticeExtra::layer(sp.text(c(27.5,-27.7),bquote(.(endemism)),col = "black",pch = 20, cex = 1,adj=0))
  
  # this saves the plot to a folder
  png(file=paste0(plotfolder,target,"_",model_type,"_res10_binary_ensemble.png"),width=3000, height=2000, res=300)
  print(plot)
  dev.off()
  rm(temp,iucn_extent) # remove unnecessary variables
  
  # this saves the plot to a folder
  plot = plot + latticeExtra::layer(sp.points(obs.data,col = "red",pch = 4, cex=0.5))
  png(file=paste0(plotfolder,target,"_",model_type,"_res10_withpoints_binary_ensemble.png"),width=3000, height=2000, res=300)
  print(plot)
  dev.off()
}}

