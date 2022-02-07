# ---------------------------------------------------------------------------------
# AUTHOR: Nina Faure Beaulieu (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST 
# ---------------------------------------------------------------------------------

# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script merges all data sources and produces one file per species
# ---------------------------------


# ---------------------------------
# PACKAGES
# ---------------------------------
library(dplyr)
library(lubridate)
library(stringr)
library(plyr)
library(sf)
library(ggplot2)
library(anytime)
library(readxl)
# ---------------------------------


# ---------------------------------
# DIRECTORY
# ---------------------------------
# define your path
path = "/home/nina/Documents/" # path for linux
path =  "/Users/nfb/" # path for mac
setwd(paste0(path,"Dropbox/6-WILDOCEANS"))
# ---------------------------------


# ---------------------------------
# DATA
# ---------------------------------
# list all csv files, each csv file is a different cleaned dataset
temp = list.files(path = "/Users/nfb/Dropbox/6-WILDOCEANS/OccurenceData/2-Cleaned_data/Point_data",pattern="*.csv", recursive = TRUE, full.names = TRUE)
# ---------------------------------


# ---------------------------------
# FORMATTING
# ---------------------------------
# read all files into a list
myfiles = lapply(temp, read.csv, header = TRUE)

# turn all headers to capital
for(i in 1:length(myfiles)){colnames(myfiles[[i]]) = toupper(colnames(myfiles[[i]]))}

# combine all files in one dataset
library(plyr)
summary = do.call(rbind.fill,myfiles)
detach("package:plyr")

rm(myfiles, temp,i)

# keep headers of interest only
headers = toupper(c("Species_scientific","Longitude","Latitude","Date","Dataset","Season"))
summary2 = summary[,colnames(summary)[colnames(summary) %in% headers]]
rm(headers,summary)

# trim white space after scientific names
summary2$SPECIES_SCIENTIFIC = trimws(summary2$SPECIES_SCIENTIFIC, which = "both")

# Remove any observations lacking a species name
absentspp = summary2[is.na(summary2$SPECIES_SCIENTIFIC),]
rm(absentspp)
summary2 = summary2 %>%
  filter(!is.na(SPECIES_SCIENTIFIC))

# check how many sightings do not have a date
# for some datasets this is normal i.e. ATAP/ORI
nodate = summary2[is.na(summary2$DATE),]
rm(nodate)# remove unnecessary variable

# format all dates and specify different formats
# a warning message will appear if some dates fail to parse
# this may be beacuse all the formats have not been specified
summary2$DATE2 = parse_date_time(summary2$DATE,
                                orders = c("dmy","dmY","Ymd","Y"))
# if no dates failed to parse inly keep one date column
summary2$DATE = NULL
colnames(summary2)[6] = "DATE"

unformated = summary2[is.na(summary2$DATE),] # check which observations don't have a formatted date
rm(unformated)# remove unnecessary variable

# check empty datasets
missingdataset= summary2[is.na(summary2$DATASET),] # check which observations don't have a formatted date
rm(missingdataset)

# capitalise species names
summary2$SPECIES_SCIENTIFIC = toupper(summary2$SPECIES_SCIENTIFIC)

# look at range of dates
range(summary2$DATE, na.rm = TRUE)

# turn dataset to factor
summary2$DATASET = as.factor(summary2$DATASET)

# add year to data
summary2$year = year(summary2$DATE)

# trim white space
summary2$SPECIES_SCIENTIFIC = trimws(summary2$SPECIES_SCIENTIFIC, which = "both")

# only keep unique instance
summary2 = unique(summary2)

# some species names contain hidden characters
# this cleans all the hidden characters out
for(i in 1:nrow(summary2)){summary2[i,3] = str_replace_all(summary2[i,3], "\\s", " ")}
rm(i)

# change synonyms
# load data sheet with species names to change
# this sheet contains any historical species names that need to be updated
synonym_sheet = read_xlsx(list.files(path = "/Users/nfb/Dropbox/6-WILDOCEANS/","synonymies.xlsx",recursive = TRUE,full.names = TRUE))
# change names to upper case
synonym_sheet$Incorrect_name =toupper(synonym_sheet$Incorrect_name)
synonym_sheet$Correct_name =toupper(synonym_sheet$Correct_name)
# remove special characters as well
for(i in 1:nrow(synonym_sheet)){synonym_sheet[i,1] = str_replace_all(synonym_sheet[i,1], "\\s", " ")}
for(i in 1:nrow(synonym_sheet)){synonym_sheet[i,2] = str_replace_all(synonym_sheet[i,2], "\\s", " ")}
# change any synonyms
for(i in 1:nrow(synonym_sheet)){
  summary2 = summary2 %>%
    mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == synonym_sheet$Incorrect_name[i],synonym_sheet$Correct_name[i],SPECIES_SCIENTIFIC))
}
rm(synonym_sheet,i)

# remove duplicates
summary3 = unique(summary2)

# summarise number of counts by species name
observation_counts = summary3 %>%
  group_by(SPECIES_SCIENTIFIC)%>%
  summarise(count = n())

# extract species that are lumped together in a group
spp = str_detect(observation_counts$SPECIES_SCIENTIFIC," SPP")
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
  filter(SPECIES_SCIENTIFIC != "UNKNOWN") %>%
  filter(SPECIES_SCIENTIFIC != "DASYATIDAE")%>%
  filter(SPECIES_SCIENTIFIC != "HIMANTURA")

# load target species
targets = readxl::read_xlsx("/Users/nfb/Dropbox/6-WILDOCEANS/wildoceans_specieslist.xlsx")
colnames(targets)[colnames(targets) == "Scientific name"] = "SPECIES_SCIENTIFIC"
targets$SPECIES_SCIENTIFIC = toupper(targets$SPECIES_SCIENTIFIC )

# join both datasets
observation_counts = left_join(observation_counts,targets)

# write sheet to use for keeping track of species groups not included
write.csv(groups, "/Users/nfb/Dropbox/6-WILDOCEANS/data_summary_excludedspeciesgroups.csv",row.names = FALSE)
# ---------------------------------


# ---------------------------------
# DATA EXTRACTION
# ---------------------------------

# only keep unique records
summary3 = unique(summary3)

# ---------------------------------
# DATE FILTERING
# ---------------------------------
# plot of which years each dataset have
temp = summary3 %>%
  group_by(DATASET, year) %>%
  summarise()%>%
  group_by(DATASET)%>%
  summarise(n = n()) %>%
  arrange(desc(n))
names = temp$DATASET
temp = summary3 
temp$DATASET = as.factor(temp$DATASET, levels = names)
png("/Users/nfb/Dropbox/6-WILDOCEANS/OccurenceData/2-Cleaned_data/data_explorationplots/summaryplot_yearsperdataset.png", units="in", width=7, height=5, res=300)
ggplot(summary3, aes(x = factor(DATASET,level = names),year, colour = DATASET))+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,hjust = 0.95,vjust=0.2,size = 10, colour = "black"),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  # line at 1950 which indicates the GBIF data you are removing
  geom_hline(yintercept=1950,colour = "red")
dev.off()

# second plot from 1950 onwards
png("/Users/nfb/Dropbox/6-WILDOCEANS/OccurenceData/2-Cleaned_data/data_explorationplots/summaryplot_yearsperdataset_1950onwards.png", units="in", width=7, height=5, res=300)
ggplot(summary3, aes(x = factor(DATASET,level = names),year, colour = DATASET))+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.2, size = 10,colour = "black"),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  scale_y_continuous(breaks=seq(1950,2022,10), limits = c(1950,2022))
dev.off()

# filter to only keep data points after 1950
# this only filters GBIF data in any case
gbif_obis = summary3 %>%
  filter(DATASET == "gbif_obis") %>%
  filter(DATE >= as.Date("1950-01-01"))

# remove gbif dataset from main dataset
summary3 = summary3 %>%
  filter(DATASET != "gbif_obis")

# add filtered gbif dataset back into main dataset
summary3 = full_join(summary3,gbif_obis)

# check date range again
range(summary3$DATE, na.rm = TRUE)
##############

# extract species names
sp = unique(observation_counts$SPECIES_SCIENTIFIC, ignore.case = TRUE)
ls = list() # empty list
for(i in sp){
  # extract all data for one species
  temp = summary3 %>%
    filter(SPECIES_SCIENTIFIC == i) %>% 
    filter(!is.na(as.numeric(LONGITUDE))) %>%
    filter(!is.na(as.numeric(LATITUDE)))
  # save as R object to be used in modeling
  saveRDS(temp,file = paste("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/speciesdata/",i, ".rds", sep=""))
  # also save important information on that species
  summary_temp = temp %>%
    group_by(DATASET)%>%
    summarise(Start = as.Date(first(DATE)), End = as.Date(last(DATE)),datapoints = n())%>%
    arrange(desc(datapoints))
  write.csv(summary_temp,file =paste("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/speciesdata/",i,".csv",sep=""))
}
