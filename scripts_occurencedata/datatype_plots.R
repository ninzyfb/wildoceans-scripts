# ---------------------------------------------------------------------------------
# AUTHORS: Nina Faure Beaulieu, Dr. Victoria Goodall (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST
# CONTACTs: ninab@wildtrust.co.za; victoria.goodall@mandela.ac.za 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script plots all points from one data type
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
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/")
# directory for plotting
path =  "/Users/nfb/" # path for mac
path =  "/home/nina/" # path for pc
my.directory = paste0(path,"Dropbox/6-WILDOCEANS/")
# ---------------------------------


# ---------------------------------
# PLOTTING PARAMETERS
# ---------------------------------
source(list.files(pattern = "plottingparameters.R", recursive = TRUE, full.names= TRUE))
# ---------------------------------



# load raw data from all files
allfiles = list.files(pattern = "rawdata.csv", recursive=TRUE, full.names = TRUE)
# create empty data frame to place all data in one csv file
all = data.frame(1,1,1,1,1,1,1)
colnames(all) = colnames(temp)
# loop to add each dataset to one file
for(i in 1:length(allfiles)){
  temp = read.csv(allfiles[i])
  all = rbind(all,temp)}

# remove empty first row
all = all[-1,]

# loop to plot one dataset at a time
for(i in unique(all$DATASET)){
  # extract points from that dataset
  temp = all %>% filter(DATASET == i)
  # convert data to spatial points data frame
  coordinates(temp) =  ~ cbind(temp$LONGITUDE,temp$LATITUDE)
  # dataset name
  name = i
  
  # plot
  plot = levelplot(blank_template,
            xlab = NULL,
            ylab = NULL,
            colorkey = FALSE,
            margin = FALSE,
            main = name)+
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
    # data points
    latticeExtra::layer(sp.points(temp,col = "Blue",pch = 20, cex = 1))
  
  png(file=paste0(my.directory,"/ConservationPlan/OccurenceData/Outputs/plots_bydataset/",name,".png"),width=3000, height=2000, res=300)
  print(plot) 
  dev.off()
}
  

write.csv(all,"all_data.csv")
