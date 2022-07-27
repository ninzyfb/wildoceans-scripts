# ---------------------------------------------------------------------------------
# AUTHORS: Nina Faure Beaulieu, Dr. Victoria Goodall (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST
# CONTACT: ninab@wildtrust.co.za; victoria.goodall@mandela.ac.za 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script 
# ---------------------------------


# ---------------------------------
# PACKAGES
# ---------------------------------
# list of required packages
requiredpackages = c("rgeos","sf","dplyr","tidyr","prioritizr","gurobi","stringr","rasterVis","viridis","raster","scales","readxl","fasterize","sdmvspecies","RColorBrewer")
# check which packages you need to install
requiredpackages = requiredpackages[which(!(requiredpackages %in% installed.packages()))]
# install packages
install.packages(requiredpackages)
# load packages
requiredpackages = c("rgeos","sf","dplyr","tidyr","prioritizr","gurobi","stringr","rasterVis","viridis","raster","scales","readxl","fasterize","sdmvspecies","RColorBrewer")
lapply(requiredpackages,require, character.only = TRUE)
rm(requiredpackages)
# ---------------------------------


# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------
# set directory to same parent folder where sub-scripts are found
# the subs-scripts can be in folders within this directory as the code specifies to look through all folders
path =  "/Users/nfb/" # path
my.directory = paste0(path,"Dropbox/6-WILDOCEANS")
# set directory
setwd(my.directory) 
# ---------------------------------


# ---------------------------------
# PLANNING UNITS
# ---------------------------------
# Load the planning unit grid at 10 x 10 km or 5 x 5 km resolution
# Each grid cell has a value of 1 which represents the cost of that grid cell
pu = raster(list.files(pattern = "template_10km.tif",full.names = TRUE,recursive = TRUE))
# ---------------------------------


# ---------------------------------
# SPECIES INFO
# ---------------------------------
# load data summary sheet
master = read_xlsx(list.files(pattern = "data_summary_master.xlsx", recursive = TRUE,full.names = TRUE)[1],sheet = 1)
# load pelagic species that we remove for the conservation plan
source(list.files(pattern = "pelagic_spp.R", recursive = TRUE)) 
# ---------------------------------


# ---------------------------------
# TARGETS
# ---------------------------------
# load target file
unique(master$STATUS)
targets = read_xlsx(list.files(pattern = "perc_targets", recursive = TRUE,full.names = TRUE))
targets = targets %>%
  pivot_longer(!STATUS,names_to = "ENDEMIC.STATUS",values_to = "target")
# add targets to master sheet
master$ENDEMIC.STATUS = as.character(master$ENDEMIC.STATUS )
master = left_join(master,targets)
rm(targets)
# ---------------------------------


# ---------------------------------
# BIODIVERSITY FEATURES
# ---------------------------------
# this script loads all of the SDMs and packages them in a stack
source(list.files(pattern = "Biodiversityfeatures.R", recursive = TRUE)) 
# turn all values to binary
values(sdms_thresholds)[which(values(sdms_thresholds)>0)] = 1
# convert NA values to 0
values(sdms_thresholds)[which(values(sdms_thresholds)!=1)] = 0
values(sdms_thresholds)[which(is.na(values(sdms_thresholds)))] = 0
# ---------------------------------


# ---------------------------------
# IUCN FEATURES
# ---------------------------------
source(list.files(pattern = "iucnmaps.R", recursive = TRUE)) 
# ---------------------------------


# ---------------------------------
# Number of species per MPA
# ---------------------------------
rm(pu,id,problem_species_all)

# South African continental marine protected areas
mpas_spp = st_read(list.files(pattern ="SAMPAZ_OR_2021_Q3.shp" ,recursive = TRUE, full.names = TRUE))
# simplify MPA shapefile
mpas_spp = mpas_spp %>%
  filter(CUR_NME != "Prince Edward Island Marine Protected Area") %>%
  # this groups all zones into the 38 main MPAs
  group_by(CUR_NME) %>%
  summarise()
# simplify mpas to make file less heavy
mpas_spp = st_simplify(mpas_spp,dTolerance = 100)

# sum all distribution maps together
iucn_sum = calc(iucn_stack_all,sum,na.rm=TRUE)
sdm_sum = calc(sdms_thresholds,sum,na.rm=TRUE)

# run a loop to extract which MPAs each species was found in
sdm_mpaoverlay = data.frame(sum = rep(0,38),
                            status_sum_CR = rep(0,38),
                            status_sum_EN = rep(0,38),
                            status_sum_VU = rep(0,38),
                            endemism_sum_1 = rep(0,38),
                            endemism_sum_2 = rep(0,38),
                            endemism_sum_0 = rep(0,38))

# run loop for each species distribution
# this loop will identify number of species per MPA as well as number of threatened spp per MPA and number of endemic species per MPA
for(i in 1:nlayers(sdms_thresholds)){
  # extract presence of each species per mpa
  spp_temp = extract(sdms_thresholds[[i]],mpas, fun=function(x,...)mean(x),na.rm=TRUE)
  # convert values to dataframe
  spp_temp  = data.frame(presence = spp_temp)
  # convert any non 0 value to 1 to indicate presence
  spp_temp$presence[which(spp_temp$presence>0)] = 1
  # add each dataframe to main dataframe
  sdm_mpaoverlay$sum =  sdm_mpaoverlay$sum + spp_temp$presence
  # get status and endemism of species
  master_temp = master %>%
    filter(SPECIES_SCIENTIFIC %in% str_replace(names(sdms_thresholds[[i]]),"\\."," ") )
  status_temp = master_temp$STATUS
  end_temp = master_temp$ENDEMIC.STATUS
  # add 1 to appropriate column (but only for MPAs where species was found )
  rows = which(spp_temp$presence>0)
  # this detects the column for status of the species and adds 1 to all MPAs which contain the species
  sdm_mpaoverlay[rows,str_detect(colnames(sdm_mpaoverlay),status_temp)] = sdm_mpaoverlay[rows,str_detect(colnames(sdm_mpaoverlay),status_temp)]+1
  # this detects the column for endemism of the species and adds 1 to all MPAs which contain the species
  sdm_mpaoverlay[rows,str_detect(colnames(sdm_mpaoverlay),end_temp)] = sdm_mpaoverlay[rows,str_detect(colnames(sdm_mpaoverlay),end_temp)]+1
  rm(end_temp,status_temp,master_temp,spp_temp)
  }

# add to mpa shapefile
mpas_spp = cbind(mpas_spp,sdm_mpaoverlay)

# save data
spp_permpa = mpas_spp@data
write.csv(spp_permpa,"spp_per_mpa.csv")
# ---------------------------------


# ---------------------------------
# Plotting
# ---------------------------------
# convert to spatial for plotting
mpas_spp = as(mpas_spp, Class = "Spatial")

# cut irreplaceability values into 5 catgeories
mpas_spp$categories = as.numeric(cut(mpas_spp$sum,4))
# set 5 colors
color.match = brewer.pal(4,"PuBu")
# Sort the values
lookupTable = sort(unique(mpas_spp$categories))
# Match colors to sorted unique values in polygon
# and assign them to a new column in the polygon data
# so that they plot smallest values as lightest and largest values as darkest
mpas_spp$color = color.match[match(mpas_spp$categories, lookupTable)]

# plot showing number of species as a label
levelplot(blank_template,
          xlab = NULL,
          ylab = NULL,
          colorkey=F,
          main = "Number of species per MPA")+
  # solution
  latticeExtra::layer(sp.polygons(mpas_spp,fill = mpas_spp$color,col =  mpas_spp$color,cex = 0.4,alpha = 0.8, pch = 21))
# ---------------------------------