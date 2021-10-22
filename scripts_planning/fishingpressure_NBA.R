# ---------------------------------------------------------------------------------
######### Shark and ray species distribution models (SDMs) - fishingpressure_NBA script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: 
####

# ---------------------------------
# PACKAGES
# ---------------------------------
library(dplyr)
library(stringr)
library(xlsx)
library(tidyr)

# ---------------------------------
# DIRECTORY
# ---------------------------------
path =  "/Users/nfb/"
setwd(paste0(path,"Dropbox/6-WILDOCEANS"))

# ---------------------------------
# DATA
# ---------------------------------
# list fishing pressure files
fp = list.files(pattern = "intensity.tif", recursive=TRUE,full.names = TRUE, ignore.case = TRUE)
template = raster(list.files(pattern = "template.tif",full.names = TRUE,recursive = TRUE)) # raster of planning unit

# ---------------------------------
# FORMATTING
# ---------------------------------

# the files were reduced in size prior to this on a more powerful computer
# the code used to transform each raw file was:
# aggregated(file,,na.rm=TRUE)
# loop through each file and transform to match resolution of template
for(i in 1:length(fp)){
  temp = raster(fp[i])
  temp2 = projectRaster(temp,template, method = "bilinear")
  temp2 = mask(temp2,template) # mask using
  name = strsplit(fp[i],"/")[[1]][6]
  name = strsplit(name,"aggregated.tif")[[1]][1]
  writeRaster(temp2,paste0(name,"transformed.tif"),overwrite=TRUE)
}

# list all fishing pressures and save them as files
fp = list.files(path = "/Users/nfb/Dropbox/6-WILDOCEANS/Planning/Threats/fishing_effort/NBA2018",pattern = ".tif", recursive=TRUE,full.names = TRUE, ignore.case = TRUE)
fp = fp[!str_detect(fp,pattern = "tif.vat")]
fp = fp[!str_detect(fp,pattern = "tif.xml")]
fp = fp[!str_detect(fp,pattern = "tif.aux")]
names = vector()
for(i in 1:length(fp)){
  name = str_split(fp,"/")[[i]][11]
  names[i] = name
  }
names = as.data.frame(names)
write.csv(names,"NBA2018_layernames.csv")

# load master sheet 
master = read.xlsx(list.files(path = "/Users/nfb/Dropbox/6-WILDOCEANS",pattern = "profiles_master.xlsx", recursive=TRUE,full.names = TRUE, ignore.case = TRUE),1,startRow = 2)

# only keep species name and fishing column
master = master[,c(7,35)]
# rename
colnames(master) = c("species_scientific","fisheries_sa")
# seperate fisheries column
temp = master%>%
  separate(fisheries_sa,c("A","B","C","D","E","F"),";")


library(stringr)
library(dplyr)
temp = temp %>%
  mutate_if(str_detect(., 'crustacean'),~str_replace_all(., c("crustacean.*" = "PRT")))%>%
  mutate_if(str_detect(., 'prawn'),~str_replace_all(., c("prawn.*" = "PRT")))%>%
  mutate_if(str_detect(., 'hake longline'),~str_replace_all(., c("hake longline.*" = "HL")))%>%
  mutate_if(str_detect(., 'beach seine'),~str_replace_all(., c("beach seine.*" = "BS")))%>%
  mutate_if(str_detect(., 'small'),~str_replace_all(., c("small.*" = "SP")))%>%
  mutate_if(str_detect(., 'commercial linefishery'),~str_replace_all(., c("commercial linefishery.*" = "LF")))%>%
  mutate_if(str_detect(., 'recreational linefishery'),~str_replace_all(., c("recreational linefishery.*" = "LF")))%>%
  mutate_if(str_detect(., 'pelagic longline'),~str_replace_all(., c("pelagic longline.*" = "PL")))%>%
  mutate_if(str_detect(., 'recreational shore angling'),~str_replace_all(., c("recreational shore angling.*" = "RC")))%>%
  mutate_if(str_detect(., 'deep water'),~str_replace_all(., c("deep water.*" = "")))%>%
  mutate_if(str_detect(., 'KZN deepwater'),~str_replace_all(., c("KZN deepwater.*" = "")))%>%
  mutate_if(str_detect(., 'KZN deepwater'),~str_replace_all(., c("KZN deepwater.*" = "")))%>%
  mutate_if(str_detect(., 'Cape inshore demersal trawl'),~str_replace_all(., c("Cape inshore demersal trawl.*" = "IDT")))%>%
  mutate_if(str_detect(., 'demersal inshore Cape trawl fishery'),~str_replace_all(., c("demersal inshore Cape trawl fishery.*" = "IDT")))%>%
  mutate_if(str_detect(., 'inshore demersal trawl'),~str_replace_all(., c("inshore demersal trawl.*" = "IDT")))%>%
  mutate_if(str_detect(., 'deepwater'),~str_replace_all(., c("deepwater.*" = "")))%>%
  mutate_if(str_detect(., 'deep-water'),~str_replace_all(., c("deep-water.*" = "")))%>%
  mutate_if(str_detect(., 'KZN nets'),~str_replace_all(., c("KZN nets.*" = "SN")))

temp = temp %>%
  mutate_if(is.character, str_trim)

write.csv(temp,"temp.csv")


