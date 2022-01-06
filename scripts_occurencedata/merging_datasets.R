# ---------------------------------------------------------------------------------
######### Shark and ray species data merging
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: this script merges all data sources into one file per species
# specifically this script only deals with point data (not polygon or raster data, it also doesn't include fisheries data)
####

# ---------------------------------
# PACKAGES
# ---------------------------------
library(dplyr)
library(lubridate)
library(stringr)
library(plyr)
library(sf)
library(ggplot2)

# ---------------------------------
# DIRECTORY
# ---------------------------------
# define your path
# for me it changes based on if I am working on pc or mac
path = "/home/nina/Documents/" # path for linux
path =  "/Users/nfb/" # path for mac
setwd(paste0(path,"Dropbox/6-WILDOCEANS"))

# ---------------------------------
# DATA
# ---------------------------------
# list all csv files in cleaned folder
# each csv file is a different cleaned dataset
temp = list.files(path = "/Users/nfb/Dropbox/6-WILDOCEANS/OccurenceData/2-Cleaned_data/Point_data",pattern="*.csv", recursive = TRUE, full.names = TRUE)

# ---------------------------------
# FORMATTING
# ---------------------------------
# read all files into a list
myfiles = lapply(temp, read.csv, header = TRUE)

# turn all headers to capital
for(i in 1:length(myfiles)){
  colnames(myfiles[[i]]) = toupper(colnames(myfiles[[i]]))
}

# combine all files in one dataset
library(plyr)
summary = do.call(rbind.fill,myfiles)
detach("package:plyr")

# remove unnecessary variables
rm(myfiles, temp,i)

# keep headers of interest only
headers = toupper(c("Species_scientific","Longitude","Latitude","Date","Dataset","Season"))
summary2 = summary[,colnames(summary)[colnames(summary) %in% headers]]
rm(headers,summary)

# trim white space after scientific names
summary2$SPECIES_SCIENTIFIC = trimws(summary2$SPECIES_SCIENTIFIC, which = "both")

# Remove any observations are lacking a species name
absentspp = summary2[is.na(summary2$SPECIES_SCIENTIFIC),]
rm(absentspp) # remove unnecessary variable
summary2 = summary2 %>%
  filter(!is.na(SPECIES_SCIENTIFIC))

# Format dates
# check how many sightings do not have a date
nodate = summary2[is.na(summary2$DATE),]
rm(nodate)# remove unnecessary variable
# remove them
summary2 = summary2 %>%
  filter(!is.na(DATE))

# format all dates and specify different formats
summary2$DATE2 = parse_date_time(summary2$DATE,
                                orders = c("%d/%m/%y","%d/%m/%Y","%Y","%Y-%m-%d","%d.%m.%Y","%m/%d/%Y"))

unformated = summary2[is.na(summary2$DATE2),] # check which observations don't have a formatted date
rm(unformated)# remove unnecessary variable

# remove
summary2 = summary2 %>% 
  filter(!is.na(DATE2))

# check empty datasets
missingdataset= summary2[is.na(summary2$DATASET),] # check which observations don't have a formatted date
rm(missingdataset)

# Add genus
summary2$Genus = word(summary2$SPECIES_SCIENTIFIC, 1)

# capitalise species names
summary2$SPECIES_SCIENTIFIC = toupper(summary2$SPECIES_SCIENTIFIC)

# Data exploration 

# look at range of dates
range(summary2$DATE2)

# turn dataset to factor
summary2$DATASET = as.factor(summary2$DATASET)

# add year to data
summary2$year = year(summary2$DATE2)

# plot of which years each dataset have
png("/Users/nfb/Dropbox/6-WILDOCEANS/OccurenceData/2-Cleaned_data/data_explorationplots/summaryplot_yearsperdataset.png", units="in", width=5, height=5, res=300)
ggplot(summary2, aes(DATASET,year, colour = DATASET))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.2,size = 5),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  # line at 1950 which indicates the GBIF data you are removing
  geom_hline(yintercept=1950,colour = "red")
dev.off()

# second plot from 1950 onwards
png("/Users/nfb/Dropbox/6-WILDOCEANS/OccurenceData/2-Cleaned_data/data_explorationplots/summaryplot_yearsperdataset_1950onwards.png", units="in", width=5, height=5, res=300)
ggplot(summary2, aes(DATASET,year, colour = DATASET))+
  geom_point()+
  # rotate x axis
  theme(axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.2, size = 5),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  scale_y_continuous(breaks=seq(1950,2022,10), limits = c(1950,2022))
 dev.off()


# filter to only keep data points after 1950
# this only filters GBIF data in any case
summary3 = summary2 %>%
  filter(DATE2 >= as.Date("1950-01-01"))
rm(summary2)

# check date range again
range(summary3$DATE2)

# trim white space
summary3$SPECIES_SCIENTIFIC = trimws(summary3$SPECIES_SCIENTIFIC, which = "both")

# some species names contain hidden characters
# this cleans all the hidden characters out
for(i in 1:nrow(summary3)){
  summary3[i,4] = str_replace_all(summary3[i,4], "\\s", " ")
}

rm(i)

# change synonyms
# load data sheet with species names to change
# this sheet contains any historical species names that need to be updated
synonym_sheet = read_xlsx(list.files(path = "/Users/nfb/Dropbox/6-WILDOCEANS/","synonymies.xlsx",recursive = TRUE,full.names = TRUE))
# change names to upper case
synonym_sheet$Incorrect_name =toupper(synonym_sheet$Incorrect_name)
synonym_sheet$Correct_name =toupper(synonym_sheet$Correct_name)
# change any synonyms
for(i in 1:nrow(synonym_sheet)){
  summary3 = summary3 %>%
    mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == synonym_sheet$Incorrect_name[i],synonym_sheet$Correct_name[i],SPECIES_SCIENTIFIC))
}
rm(synonym_sheet,i)

# number of species
sort(unique(summary3$SPECIES_SCIENTIFIC))

# summarise number of counts by species name
observation_counts = summary3 %>%
  group_by(SPECIES_SCIENTIFIC)%>%
  summarise(count = n())

# extract species that are lumped together in a group
spp = str_detect(observation_counts$SPECIES_SCIENTIFIC,"SPP")
groups = observation_counts[spp,]
observation_counts = observation_counts[!spp,]
# extract species that are lumped together in a group
sp = str_detect(observation_counts$SPECIES_SCIENTIFIC,"SP\\.")
groups2 = observation_counts[sp,]
observation_counts = observation_counts[!sp,]
# combine removed species
groups = rbind(groups,groups2)
rm(groups2)

# remove observations of less than 5
observation_counts = observation_counts %>%
  filter(SPECIES_SCIENTIFIC != "0") %>%
  filter(SPECIES_SCIENTIFIC != "UNKNOWN")

# load target species
targets = readxl::read_xlsx("/Users/nfb/Dropbox/6-WILDOCEANS/wildoceans_specieslist.xlsx")
colnames(targets)[colnames(targets) == "Scientific name"] = "SPECIES_SCIENTIFIC"
targets$SPECIES_SCIENTIFIC = toupper(targets$SPECIES_SCIENTIFIC )

# join both datasets
observation_counts = left_join(observation_counts,targets)

# write sheet to use for keeping track of species groups not included
write.csv(groups, "/Users/nfb/Dropbox/6-WILDOCEANS/data_summary_excludedspeciesgroups.csv",row.names = FALSE)

##################### extract data per target species 

# extract species names
sp = unique(observation_counts$SPECIES_SCIENTIFIC, ignore.case = TRUE)
summary3$year = NULL
ls = list() # empty list
for(i in sp){
  temp = summary3 %>%
    filter(SPECIES_SCIENTIFIC == i) %>% # filter for species
    filter(!is.na(as.numeric(LONGITUDE))) %>%
    filter(!is.na(as.numeric(LATITUDE)))
  saveRDS(temp,file = paste("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/speciesdata/",i, ".rds", sep=""))
  }

##################### extract data per target GENUS 

# extract species names
gen = unique(summary3$Genus, ignore.case = TRUE)
ls = list() # empty list
for(i in gen){
  temp = summary3 %>%
    filter(Genus == i) %>% # filter for species
    filter(!is.na(as.numeric(LONGITUDE))) %>%
    filter(!is.na(as.numeric(LATITUDE)))
  saveRDS(temp,file = paste("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/generadata/",i, ".rds", sep=""))
  temp = st_as_sf(temp, coords = c("LONGITUDE","LATITUDE"))
  st_write(temp,paste("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/generadata/",i, ".shp", sep=""), append = TRUE)
}



