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
# PACKAGES
# ---------------------------------
# load packages
requiredpackages = c("ggpubr","ggplot2","xlsx","rmapshaper","rgeos","sf","dplyr","tidyr","prioritizr","gurobi","stringr","rasterVis","viridis","raster","scales","readxl","fasterize","sdmvspecies","RColorBrewer")
lapply(requiredpackages,require, character.only = TRUE)
rm(requiredpackages)
# ---------------------------------


# ---------------------------------
# SPECIES INFO
# ---------------------------------
# load data summary sheet
master = read_xlsx(list.files(pattern = "data_summary_master.xlsx", recursive = TRUE,full.names = TRUE)[1],sheet = 1)
# ---------------------------------


# ---------------------------------
# PLANNING UNITS
# ---------------------------------
# Load the planning unit grid at 10 x 10 km or 5 x 5 km resolution
# Each grid cell has a value of 1 which represents the cost of that grid cell
pu = raster(list.files(pattern = "template_10km.tif",full.names = TRUE,recursive = TRUE))
# ---------------------------------


# ---------------------------------
# BIODIVERSITY FEATURES
# ---------------------------------
# load pelagic species that we remove for the conservation plan
source(list.files(pattern = "pelagic_spp.R", recursive = TRUE)) 

# this script loads all of the distribution models and packages them in a stack
source(list.files(pattern = "Biodiversityfeatures.R", recursive = TRUE)) 

# convert all values within the species range to 1
values(sdms_thresholds)[which(values(sdms_thresholds)>0)] = 1
values(sdms_thresholds)[which(values(sdms_thresholds)==0)] = NA

# filter biodiversity raster stack to remove oceanic species
# filtered stack
idx = which(!(str_replace(names(sdms_thresholds),"\\."," ") %in% pelagic_species))
sdms_thresholds = subset(sdms_thresholds,idx)
# ---------------------------------


# ---------------------------------
# IUCN FEATURES
# ---------------------------------
source(list.files(pattern = "iucnmaps.R", recursive = TRUE)) 
idx = which(names(iucn_stack_all) %in% names(sdms_thresholds))
iucn_equivalents = subset(iucn_stack_all,idx)
# ---------------------------------


# ---------------------------------
# MPA shapefile
# ---------------------------------
# South African continental marine protected areas
mpas = st_read(list.files(pattern ="SAMPAZ_OR_2021_Q3.shp" ,recursive = TRUE, full.names = TRUE)[1])
# simplify MPA shapefile
mpas = mpas %>%
  filter(CUR_NME != "Prince Edward Island Marine Protected Area") %>%
  group_by(CUR_NME,CUR_ZON_TY,CUR_ZON_NM,GIS_AREA) %>%
  summarise()
# simplify mpas
mpas = st_simplify(mpas,dTolerance = 100)
# add ID to mpa shapefile so that you can add map names and zone types
mpas$ID = seq(1:length(mpas$CUR_NME))
# ---------------------------------


# ---------------------------------
# Area cover calculations
# ---------------------------------
# get sum of all species ranges per MPA zone
values = raster::extract(sdms_thresholds,mpas, cellnumbers=TRUE, na.rm=TRUE,df=T)
# add mpa info
values2 = left_join(values,mpas[,c(2,6)])
# only keep no-take mpas
values2 = values2 %>%
  filter(CUR_ZON_TY %in% c("Wilderness","Sanctuary","Restricted"))
# remove extra column
values2$CUR_ZON_TY = NULL
values2$geometry = NULL
# values from iucn equivalents from all MPAs
values3 = raster::extract(iucn_equivalents,mpas, cellnumbers=TRUE, na.rm=TRUE,df=T)
# values from iucn equivalents from no take zones
values4  = left_join(values3,mpas[,c(2,6)])
# only keep no-take mpas
values4 = values4 %>%
  filter(CUR_ZON_TY %in% c("Wilderness","Sanctuary","Restricted"))
# remove extra column
values4$CUR_ZON_TY = NULL
values4$geometry = NULL
# values from all iucn ranges from all MPAs
values5 = raster::extract(iucn_stack_all,mpas, cellnumbers=TRUE, na.rm=TRUE,df=T)
# values from all iucn ranges from no take zones
values6  = left_join(values5,mpas[,c(2,6)])
# only keep no-take mpas
values6 = values6 %>%
  filter(CUR_ZON_TY %in% c("Wilderness","Sanctuary","Restricted"))
# remove extra column
values6$CUR_ZON_TY = NULL
values6$geometry = NULL
# ---------------------------------


# ---------------------------------
# SUMMARY SHEET 1: Percentage cover by mpa network per species
# ---------------------------------
species_total = values %>%
  pivot_longer(!c(ID,cell)) %>%
  group_by(name)%>%
  filter(value == 1)%>%
  summarise(cover = n_distinct(cell))
species_total$name = str_replace(species_total$name,"\\."," ")

species_total2 = values2 %>%
  pivot_longer(!c(ID,cell)) %>%
  group_by(name)%>%
  filter(value == 1)%>%
  summarise(cover_notake = n_distinct(cell))
species_total2$name = str_replace(species_total2$name,"\\."," ")
rm(values2)

species_total3 = values3 %>%
  pivot_longer(!c(ID,cell)) %>%
  group_by(name)%>%
  filter(value == 1)%>%
  summarise(cover_iucn82 = n_distinct(cell))
species_total3$SPECIES_SCIENTIFIC = str_replace(species_total3$name,"\\."," ")
rm(values3)

species_total4 = values4 %>%
  pivot_longer(!c(ID,cell)) %>%
  group_by(name)%>%
  filter(value == 1)%>%
  summarise(cover_notakeiucn82 = n_distinct(cell))
species_total4$SPECIES_SCIENTIFIC = str_replace(species_total4$name,"\\."," ")
rm(values4)

# get total area by each species
species_cover = data.frame(1:nlayers(sdms_thresholds))
species_cover$total_area = 0
for(i in 1:nlayers(sdms_thresholds)){
  species_cover$total_area[i] =  sum(values(sdms_thresholds[[i]]),na.rm=TRUE)
  species_cover$name[i] = str_replace(names(sdms_thresholds)[i],"\\."," ")
}
rm(i)
species_cover$X1.nlayers.sdms_thresholds. = NULL
species_total = left_join(species_total,species_cover)
species_total = left_join(species_total,species_total2)
rm(species_cover,species_total2)

# get total area by each IUCN map
species_cover_iucn = data.frame(1:nlayers(iucn_stack_all))
species_cover_iucn$total_area_iucn = 0
for(i in 1:nlayers(iucn_stack_all)){
  species_cover_iucn$total_area_iucn[i] =  sum(values(iucn_stack_all[[i]]),na.rm=TRUE)
  species_cover_iucn$SPECIES_SCIENTIFIC[i] = str_replace(names(iucn_stack_all)[i],"\\."," ")
}
rm(i)
species_cover_iucn$X1.nlayers.iucn_stack_all. = NULL

# get percentage cover
species_total$prop_cover = species_total$cover/species_total$total_area
species_total$prop_cover_notake = species_total$cover_notake/species_total$total_area

# add master sheet
colnames(species_total)[1] = "SPECIES_SCIENTIFIC"
species_total = left_join(species_total,master)
species_total = left_join(species_total,species_total3)
species_total = left_join(species_total,species_total4)

# RECALCULATE PERCENTAGE COVER FOR IUCN EQUIVALENTS
species_total = left_join(species_total,species_cover_iucn)
rm(species_cover_iucn)
species_total$prop_cover_IUCN82 = species_total$cover_iucn82/species_total$total_area_iucn
species_total$prop_cover_notake_IUCN82 = species_total$cover_notakeiucn82/species_total$total_area_iucn

# save spreadsheet
write.xlsx(species_total,"wildoceans-scripts/Outputs/planning/coverbympas/summarysheet1_speciesmpacover.xlsx")

# remove
rm(species_total)

#seperate spreadhseet for all iucn species
species_total = values5 %>%
  pivot_longer(!c(ID,cell)) %>%
  group_by(name)%>%
  filter(value == 1)%>%
  summarise(cover = n_distinct(cell))
species_total$name = str_replace(species_total$name,"\\."," ")

species_total2 = values6 %>%
  pivot_longer(!c(ID,cell)) %>%
  group_by(name)%>%
  filter(value == 1)%>%
  summarise(cover_notake = n_distinct(cell))
species_total2$name = str_replace(species_total2$name,"\\."," ")
rm(values2)
species_total = left_join(species_total,species_total2)
colnames(species_total)[1] = "SPECIES_SCIENTIFIC"
# get total area by each IUCN map
species_cover_iucn = data.frame(1:nlayers(iucn_stack_all))
species_cover_iucn$total_area_iucn = 0
for(i in 1:nlayers(iucn_stack_all)){
  species_cover_iucn$total_area_iucn[i] =  sum(values(iucn_stack_all[[i]]),na.rm=TRUE)
  species_cover_iucn$SPECIES_SCIENTIFIC[i] = str_replace(names(iucn_stack_all)[i],"\\."," ")
}
rm(i)
species_cover_iucn$X1.nlayers.iucn_stack_all. = NULL
species_total = left_join(species_total,species_cover_iucn)
species_total[which(is.na(species_total$cover_notake)),3] = 0
# get percentage cover
species_total$prop_cover = species_total$cover/species_total$total_area_iucn
species_total$prop_cover_notake = species_total$cover_notake/species_total$total_area_iucn
# save spreadsheet
write.xlsx(species_total,"wildoceans-scripts/Outputs/planning/coverbympas/summarysheet1_speciesmpacoveralliucnranges.xlsx")

# ---------------------------------


# ---------------------------------
# SUMMARY SHEET 2: Number of species per mpa and per mpa zone
# ---------------------------------
# get number of unique cells per species
# that is the number of cells per MPA zone
species_total = values %>%
  pivot_longer(!c(ID,cell)) %>%
  group_by(name,ID)%>%
  filter(value == 1)%>%
  summarise(cover = n_distinct(cell))

# add MPA info
species_total = left_join(species_total,mpas)

# get total number of species per mpa and then per mpa zone and zone type
species_n_permpa = species_total %>%
  group_by(CUR_NME) %>%
  summarise(n_spp_mpa = n_distinct(name))

species_n_permpazone = species_total %>%
  group_by(CUR_NME,CUR_ZON_NM,CUR_ZON_TY) %>%
  summarise(n_spp = n_distinct(name))

# add all to same sheet
species_n_permpazone = left_join(species_n_permpazone,species_n_permpa)
species_n_permpazone = as.data.frame(species_n_permpazone)

# save spreadsheet
write.xlsx(species_n_permpazone,"wildoceans-scripts/Outputs/planning/coverbympas/summarysheet2_nspecies_permpazone.xlsx")

# remove
rm(species_n_permpa)
# ---------------------------------


# ---------------------------------
# SUMMARY PLOTS
# ---------------------------------

# PLOT 1: Grouped boxplot showing % cover by IUCN status for entire network and just no-take zones
summary1 = read.xlsx(list.files(pattern = "summarysheet1_speciesmpacover.xlsx",recursive = TRUE,full.names=T),1)
summary1new <- summary1                             # Duplicate data
summary1new$STATUS <- factor(summary1new$STATUS,     # Reorder factor levels
                         c("CR", "EN", "VU", "NT","LC","DD"))
png("wildoceans-scripts/Outputs/planning/coverbympas/coverbyIUCNstatuses.png",height=2000, width=3000, res=300)
summary1new[,c(3,8,9)] %>%
  pivot_longer(!STATUS,names_to = "type") %>%
  ggplot(aes(x=STATUS, y=value))+
  geom_boxplot(aes(fill = type))+
  theme_pubr()+
  ylab("Proportion of range proptected")+
  xlab("")+
  theme(legend.title = element_text(colour="black", size=20, 
                                    face="bold"),
        legend.text = element_text(size=20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_fill_manual(name = "MPA zones included", labels = c("All zones", "No-take zones"),values=c("Skyblue4", "Skyblue2"))
dev.off()

# PLOT 2: Grouped boxplot showing % cover by Endemic status for entire network and just no-take zones
summary1 = read.xlsx(list.files(pattern = "summarysheet1_speciesmpacover.xlsx",recursive = TRUE,full.names=T),1)
summary1new <- summary1    # Duplicate data
summary1new$ENDEMIC.STATUS = as.factor(summary1new$ENDEMIC.STATUS )
summary1new$ENDEMIC.STATUS <- factor(summary1new$ENDEMIC.STATUS,     # Reorder factor levels
                             c("1", "2", "0"))
png("wildoceans-scripts/Outputs/planning/coverbympas/coverbyENDEMICstatuses.png",height=2000, width=3000, res=300)
summary1new[,c(4,8,9)] %>%
  pivot_longer(!ENDEMIC.STATUS,names_to = "type") %>%
  ggplot(aes(x=ENDEMIC.STATUS, y=value))+
  geom_boxplot(aes(fill = type))+
  theme_pubr()+
  ylab("Proportion of range proptected")+
  xlab("")+
  theme(legend.title = element_text(colour="black", size=20, 
                                    face="bold"),
        legend.text = element_text(size=20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_fill_manual(name = "MPA zones included", labels = c("All zones", "No-take zones"),values=c("Skyblue4", "Skyblue2"))
dev.off()

# PLOT 3: number of species across all MPAs
species_total %>%
  group_by(CUR_NME)%>%
  summarise(n_spp = n_distinct(SPECIES_SCIENTIFIC))%>%
  ggplot(aes(x=CUR_NME, y=n_spp))+
  geom_col()+
  theme_pubr()+
  ylab("Number of species (out of 97)")+
  xlab("")+
  theme(legend.title = element_text(colour="black", size=20, 
                                    face="bold"),
        legend.text = element_text( size=20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20,angle=90),
        axis.text.y = element_text(size = 20))

# PLOT 4: number of species across all MPAs but coloured by IUCN STATUS  
colnames(species_total)[1] = "SPECIES_SCIENTIFIC"
species_total$SPECIES_SCIENTIFIC = str_replace(species_total$SPECIES_SCIENTIFIC,"\\."," ")
species_total = left_join(species_total,master)

species_total%>%
  group_by(CUR_NME,STATUS)%>%
  summarise(n_spp = n_distinct(SPECIES_SCIENTIFIC))%>%
  ggplot(aes(x=CUR_NME, y=n_spp))+
  geom_col(aes(color = STATUS,fill=STATUS))+
  theme_pubr()+
  ylab("Number of species (out of 97)")+
  xlab("")+
  theme(legend.title = element_text(colour="black", size=20, 
                                    face="bold"),
        legend.text = element_text( size=20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20,angle=90),
        axis.text.y = element_text(size = 20))

# PLOT 5: number of species across all MPAs but coloured by ENDEMIC STATUS  
species_total$ENDEMIC.STATUS = as.factor(species_total$ENDEMIC.STATUS )
species_total%>%
  group_by(CUR_NME,ENDEMIC.STATUS)%>%
  summarise(n_spp = n_distinct(SPECIES_SCIENTIFIC))%>%
  ggplot(aes(x=CUR_NME, y=n_spp))+
  geom_col(aes(color = ENDEMIC.STATUS,fill=ENDEMIC.STATUS))+
  theme_pubr()+
  ylab("Number of species (out of 97)")+
  xlab("")+
  theme(legend.title = element_text(colour="black", size=20, 
                                    face="bold"),
        legend.text = element_text( size=20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20,angle=90),
        axis.text.y = element_text(size = 20))

# PLOT 6: PLOT OF MPAS BY NUMBER OF SPECIES IN THEM
summary2 = read.xlsx(list.files(pattern = "summarysheet2",recursive = TRUE,full.names=T),1)
mpas = left_join(mpas,unique(summary2[,c(2,6)]))


mpas$categories = as.numeric(cut(mpas$n_spp_mpa,4))
color.match = c("dark green","orange","red","red4")
lookupTable = sort(unique(mpas$categories))
mpas$color = color.match[match(mpas$categories, lookupTable)]
mpas = as(mpas, Class = "Spatial")

# Plot the final product
levelplot(blank_template,
               xlab = NULL,
               ylab = NULL,
               margin = FALSE, 
               colorkey = FALSE)+
  latticeExtra::layer(sp.polygons(mpas,fill=mpas$color, lwd = 1))
