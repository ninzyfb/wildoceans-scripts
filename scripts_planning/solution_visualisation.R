# ---------------------------------------------------------------------------------
# AUTHOR: Nina Faure Beaulieu (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script plots all of the conservation planning solutions
# ---------------------------------

# ---------------------------------
# PACKAGES
# ---------------------------------
# list of required packages
requiredpackages = c("gridExtra","rgeos","sf","dplyr","tidyr","stringr","rasterVis","viridis","raster","scales","readxl","fasterize","sdmvspecies","RColorBrewer")
# load packages
lapply(requiredpackages,require, character.only = TRUE)
rm(requiredpackages)
# ---------------------------------


# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------
# set directory to same parent folder where sub-scripts are found
# the subs-scripts can be in folders within this directory as the code will look through all the folders
path =  "/Users/nfb/" # path for mac
my.directory = paste0(path,"Dropbox/6-WILDOCEANS")
# set directory
setwd(my.directory) 
# ---------------------------------


# ---------------------------------
# PLOTTING PARAMETERS
# ---------------------------------
source(list.files(pattern = "plottingparameters.R", recursive = TRUE, full.names= TRUE))
# plotting settings
settings = latticeExtra::layer(sp.polygons(mpas,col = "black",lwd = 1))+
  # mpa no-take fill
  latticeExtra::layer(sp.polygons(mpas_notake, fill = "purple",alpha = 0.1))+
  # mpa no-take outline
  latticeExtra::layer(sp.polygons(mpas_notake,col = "purple",lwd = 0.7))+
  # eez
  latticeExtra::layer(sp.polygons(eez,col = "black",lwd = 1))+
  # sa coast
  latticeExtra::layer(sp.polygons(sa,col = "black",lwd= 1, fill = "white"))+
  # points for main cities
  latticeExtra::layer(sp.points(places[c(1:3,5,6,18,20:22,10,14),],col = "black",pch = 20))+
  # coordinates and city names
  # done in three lines as a "pretty" position varies based on their place on the map
  latticeExtra::layer(sp.text(coordinates(places)[c(1:3,5,6),],places$Location[c(1:3,5,6)],col = "black",pch = 20,pos=4,cex = 0.5))+
  latticeExtra::layer(sp.text(coordinates(places)[c(18,20,21,22),],places$Location[c(18,20,21,22)],col = "black",pch = 20,pos=2,cex = 0.5))+
  latticeExtra::layer(sp.text(adjustedcoords,places$Location[c(10,14)],col = "black",pch = 20, pos=2,cex = 0.5))+
  latticeExtra::layer(sp.text(coordinates(legend)[1,],paste0("Percentage of EEZ = ",prop_eez,"%"),col = "black",pch = 20, pos=2,cex = 1))+
  latticeExtra::layer(sp.text(coordinates(legend)[2,],paste0("Current MPAs included = ",inclusion),col = "black",pch = 20, pos=2,cex = 1))

mpa_labels = st_as_sf(mpas)
mpa_labels = mpa_labels %>%
  group_by(CUR_NME) %>%
  summarise()
mpa_labels = as(mpa_labels, Class = "Spatial")
# ---------------------------------


# ---------------------------------
# VISUALIZE RESULTS
# ---------------------------------

# scenarios
scenario_sheet = read_xlsx(path=paste0(path,"Dropbox/6-WILDOCEANS/Planning/scenarios.xlsx"),sheet = 1)

# list of all solutions in raster format
files = list.files(path = paste0(my.directory,"/Planning/Outputs/"),pattern = "scenario.tif",recursive = TRUE, full.names = TRUE)

# list of all solution information
files2 = list.files(path = paste0(my.directory,"/Planning/Outputs/"),pattern = "performance.csv",recursive = TRUE, full.names = TRUE)

# list of all ferrier scores in raster format
files3 = list.files(path = paste0(my.directory,"/Planning/Outputs/"),pattern = "scenario_FS.tif",recursive = TRUE, full.names = TRUE)

# the following loop will plot each raster individually
for(i in 1:length(files)){
  
  # temporary raster
  temp = raster(files[i])
  # raster name
  name = names(temp)
  # problem number
  pnumber = str_split(name,"_")[[1]][1]
  # stream
  stream = str_split(name,"_")[[1]][2]
  if(stream == "streamA"){stream = "no"}else{stream = "yes"}
  # scenario
  scenario = str_split(name,"_")[[1]][3]
  # prop_eez
  maxvalue = max(values(temp),na.rm=TRUE)
  prop_eez = round((length(which(values(temp)==maxvalue))/10809)*100,1)
  # target
  t = as.numeric(scenario_sheet$targets[i])*100
  # mpas included
  inclusion = scenario_sheet$lockedin[i]
  if(inclusion == "mpa_layer_fullyprotected"){inclusion = "no-take zones"}
  if(inclusion == "mpa_layer_all"){inclusion = "all MPAs"}
  if(inclusion == "none"){inclusion = "No MPAs"}
  
  # solution plot 
  plot=levelplot(temp,
                 xlab = NULL,
                 ylab = NULL,
                 main = paste0(scenario," scenario","\nWeighted: ",stream," | Species protection: ",t,"%"),
                 col.regions = cols,
                 margin = FALSE,
                 colorkey=FALSE)+settings+
    latticeExtra::layer(sp.text(coordinates(mpa_labels),mpa_labels$CUR_NME,col = "black",pch = 20, pos=2,cex = 1))
  png(file=paste0("Planning/Outputs/solutions/national/",str_pad(pnumber,3,pad = "0"),"_",scenario,"scenario.png"),width=3000, height=2000, res=300)
  print(plot)
  dev.off()
  
  # ferrier/irraplaceability plot
  temp = raster(files3[i])
  temp[which(values(temp) == 0)] = NA
  plot = levelplot(temp,
                   xlab = NULL,
                   ylab = NULL,
                   colorkey=list(space="bottom", title = "Irraplaceability "),
                   main = paste0(scenario," scenario","\nWeighted: ",stream," | Species protection: ",t,"%"),
                   margin = FALSE,
                   col.regions = rev(heat.colors(32)),at = intervals2)+settings
  png(file=paste0("Planning/Outputs/solutions/ferrierscores/",str_pad(pnumber,3,pad = "0"),"_",scenario,"scenario","_ferrierscore.png"),width=3000, height=2000, res=300)
  print(plot)
  dev.off()
  
  # individual region plots (east, south, west)
  for(j in 1:length(range)){
    # subset range
    subset = regions[regions$Region%in%range[j],]
    cropped = crop(temp,subset)
    plot = levelplot(cropped, 
                     xlab = NULL,
                     ylab = NULL,
                     colorkey=list(space="bottom", title = "Irraplaceability "),
                     main = paste0(scenario," scenario","\nWeighted: ",stream," | Species protection: ",t,"%"),
                     margin = FALSE,
                     col.regions = rev(heat.colors(32)),at = intervals2)+settings
    png(file=paste0("Planning/Outputs/solutions/regional/","p",str_pad(pnumber,3,pad = "0"),"_",range[j],"_",scenario,"scenario.png"),width=3000, height=2000, res=300)
    print(plot)
    dev.off()}

  # extract importance values in "take" mpas
  # this will give regions useful for rezoning
  values = extract(temp,mpas_take)
  mpas_take_temp = mpas_take
  for (i in 1:length(values)){
    sum = sum(values[[i]], na.rm = TRUE)
    mpas_take_temp$importance[i] = sum
    result = mpas_take_temp@data %>%
      arrange(importance)
    colnames(result) = c("Zone","MPA","Irreplaceability")
    write.csv(result,paste0("Planning/Outputs/solutions/ferrierscores/",str_pad(pnumber,3,pad = "0"),"_",scenario,"scenario","_notakeimportance.csv"), row.names = FALSE)
    }
  # Find unique colors from color ramp,based on 'importance' column 
  color.match = manual.col(length(unique(mpas_take_temp$importance)))
  # Sort the values of interest (in this case, 'Prop')
  lookupTable = sort(unique(mpas_take_temp$importance))
  # Match colors to sorted unique values in polygon
  # and assign them to a new column in the polygon data
  # so that they plot smallest values as lightest and largest values as darkest
  mpas_take_temp$color = color.match[match(mpas_take_temp$importance, lookupTable)]
  # Plot the final product!
  plot = levelplot(temp,
                   xlab = NULL,
                   ylab = NULL,
                   colorkey=list(space="bottom", title = "Irraplaceability "),
                   main = paste0(scenario," scenario","\nWeighted: ",stream," | Species protection: ",t,"%"),
                   margin = FALSE,
                   col.regions = rev(heat.colors(32)),at = intervals2)+settings+
    latticeExtra::layer(sp.polygons(mpas_take_temp,colour=mpas_take_temp$color,fill = mpas_take_temp$color, lwd = 1))
  png(file=paste0("Planning/Outputs/solutions/ferrierscores/",str_pad(pnumber,3,pad = "0"),"_",scenario,"scenario","_ferrierscore_notakeranks.png"),width=3000, height=2000, res=300)
  print(plot)
  dev.off()  
  rm(mpas_take_temp)
    }


# END OF SCRIPT. SOME SPARE CODE BELOW TO DELETE IF UNUSED


# species targets acheived
#info = read.csv(files2[i])
#info$target_achieved = round(info$target_achieved,1)
#info = info %>%
#  filter(target > target_achieved)
#n_shortfall = nrow(info)
#min_shortfall = round(min(info$km_shortfall_avg),0)
#if(min_shortfall == Inf){min_shortfall = 0}
#max_shortfall = round(max(info$km_shortfall_avg),0)
#if(max_shortfall == -Inf){max_shortfall = 0}


# the following loop will plot rasters in groups of 4 and save each plot
#for(i in seq(1,length(files),4)){
# isolate single raster
temp = stack( raster(files[i]),  raster(files[i+1]), raster(files[i+2]), raster(files[i+3]))
# create plot
temp_plot = rasterVis::levelplot(temp,
                                 names.attr = c("10%","20%","30%","40%"),
                                 margin = FALSE,
                                 colorkey = FALSE,
                                 xlab = "Species area percentage protection - all MPAs included",
                                 ylab = NULL,
                                 col.regions = cols,
                                 # reduces size tick marks
                                 scales = list(tck = c(0.5,0.5)),
                                 # reduces space at top and bottom of plot
                                 par.settings = list(layout.heights=list(top.padding=-2, bottom.padding = -1),
                                                     axis.line = list(col = "transparent"), 
                                                     strip.background = list(col = 'transparent')))+ settings
# save plot
str_split(files[i],"/Users/nfb/Dropbox/6-WILDOCEANS/Planning/Outputs//solutions/rasters_rawsolutions/")[[1]][2]
png(paste0(".png"),width=3000, height=2000, res=300)
temp_plot
dev.off()
