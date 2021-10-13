# set working directory
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/OccurenceData/2-Cleaned_data/Point_data")
getwd()

# load packages
library(dplyr)
library(lubridate)
library(stringr)
library(plyr)
library(sf)

# Load data
# list all csv files in cleaned folder (current directory)
temp = list.files(path = "/Users/nfb/Dropbox/6-WILDOCEANS/OccurenceData/2-Cleaned_data/Point_data",pattern="*.csv", recursive = TRUE)

# read them into a list
myfiles = lapply(temp, read.csv, header = TRUE)

# turn all headers to capital

for(i in 1:length(myfiles)){
  colnames(myfiles[[i]]) = toupper(colnames(myfiles[[i]]))
}

# combine all files in one dataset
summary = do.call(rbind.fill,myfiles)
detach("package:plyr")

# remove unnecessary variables
rm(myfiles, temp,i)

# keep headers of interest only
headers = toupper(c("Species_scientific","Longitude","Latitude","Date","Dataset","Season","LIFESTAGE","Abundance","Method"))
summary2 = summary[,colnames(summary)[colnames(summary) %in% headers]]
rm(headers)

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
                                orders = c("%d/%m/%y","%d/%m/%Y","%Y","%Y-%m-%d"))

unformated = summary2[is.na(summary2$DATE2),] # check which observations don't have a formatted date
rm(unformated)# remove unnecessary variable

summary2 = summary2 %>% # remove always same 3 empty rows, not sure why
  filter(!is.na(DATE2))

# Add genus
summary2$Genus = word(summary2$SPECIES_SCIENTIFIC, 1)

# capitalise species names
summary2$SPECIES_SCIENTIFIC = toupper(summary2$SPECIES_SCIENTIFIC)

# Data exploration 

# look at range of dates
range(summary2$DATE2)

# turn dataset to factor
summary2$DATASET = as.factor(summary2$DATASET)
plot(summary2$DATE2,summary2$DATASET)

# add year to data
summary2$year = year(summary2$DATE2)

# check which datasets for each year
table = table(summary2$year,summary2$DATASET)
table = as.data.frame(table)
library(tidyr)
table = table %>%
  pivot_wider(names_from = Var1, 
            values_from = Freq)

write.csv(table,"table.csv")


# filter to only keep GBIF data after 1990 (last 30 years)
gbif = summary2 %>%
  filter(DATE2 >= as.Date("1990-01-01") & DATASET == "gbif_obis")

# remove gbif dataset
summary3 = summary2 %>%
  filter(DATASET != "gbif_obis")

# join back gbif dataset 
summary3 = full_join(summary3,gbif)

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

##################### Change synonyms
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "EUGOMPHODUS TAURUS","CARCHARIAS TAURUS",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "RHINOBATOS ANNULATUS","ACROTERIOBATUS ANNULATUS",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "AETOBATUS NARINARI","AETOBATUS OCELLATUS",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "RAJA WALLACEI","LEUCORAJA WALLACEI",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "HIMANTURA GERRARDI","PATEOBATIS FAI",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "RAJA ALBA","ROSTRORAJA ALBA",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "TAENIURA MELANOSPILOS","TAENIUROPS MEYENI",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "TAENIURA MEYENI","TAENIUROPS MEYENI",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "CENTROSCYMNUS CREPIDATER","CENTROSELACHUS CREPIDATER",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "DASYATIS CHRYSONATA","DASYATIS CHRYSONOTA",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "MOBULA BIRSOTRIS","MOBULA BIROSTRIS",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "NOTORHYNCHUS CEPEDIANUS","NOTORYNCHUS CEPEDIANUS",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "RAJA STRAELINI","RAJA STRAELENI",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "RAJA CLAVATA","RAJA STRAELENI",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "RAJA SPINACIDERMIS","MALACORAJA SPINACIDERMIS",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "RAJA DISSIMILIS","RAJELLA DISSIMILIS",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "RAJA CONFUDENS","RAJELLA BARNARDI",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "RAJA BARNARDI","RAJELLA BARNARDI",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "RAJA CAUDASPINOSA","RAJELLA CAUDASPINOSA",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "RAJA MIRALETUS","RAJA OCELLIFERA",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "RAJA LEOPARDUS","RAJELLA LEOPARDUS",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "DASYATIS BREVICAUDATA","BATHYTOSHIA BREVICAUDATA",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "DASYATIS MARMORATA","DASYATIS CHRYSONOTA",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "DASYATIS PASTINACA","DASYATIS CHRYSONOTA",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "DASYATIS VIOLACEA","PTEROPLATYTRYGON VIOLACEA",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "ETMOPTERUS BAXTERI","ETMOPTERUS GRANULOSUS",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "ETMOPTERUS BRACHYURUS","ETMOPTERUS SCULTPUS",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "ETMOPTERUS GRACILISPINIS","ETMOPTERUS COMPAGNOI",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "ETMOPTERUS LUCIFER","ETMOPTERUS SCULTPUS",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "ETMOPTERUS SPINAX","ETMOPTERUS COMPAGNOI",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "HIMANTURA JENKINSII","PATEOBATIS JENKINSII",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "MOBULA EREGOODOOTENKEE","MOBULA EREGOODOO",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "MOBULA JAPANICA","MOBULA MOBULAR",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "MUSTELLUS MUSTELLUS","MUSTELUS MUSTELUS",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "MYLIOBATUS AQUILA","MYLIOBATIS AQUILA",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "PRISTIS MICRODON","PRISTIS PRISTIS",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "PTEROMYLAEUS BOVINUS","AETOMYLAEUS BOVINUS",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "RAJA SPRINGERI","DIPTURUS SPRINGERI",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "RAJELLA LEOPARDUS","RAJELLA LEOPARDA",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "SQUALUS MARGARETSMITHAE","SQUALUS ACUTIPINNIS",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "TETRONARCE NOBILIANA","TETRONARCE COWLEYI",SPECIES_SCIENTIFIC))
summary3 = summary3 %>%
  mutate(SPECIES_SCIENTIFIC = ifelse(SPECIES_SCIENTIFIC == "TORPEDO NOBILIANA","TETRONARCE COWLEYI",SPECIES_SCIENTIFIC))

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
observation_counts = observation_counts %>%
  filter(SPECIES_SCIENTIFIC != "0")

# load target species
targets = readxl::read_xlsx("/Users/nfb/Dropbox/6-WILDOCEANS/wildoceans_specieslist.xlsx")
colnames(targets)[colnames(targets) == "Scientific name"] = "SPECIES_SCIENTIFIC"
targets$SPECIES_SCIENTIFIC = toupper(targets$SPECIES_SCIENTIFIC )

# join both datasets
observation_counts = left_join(observation_counts,targets)

# read sheet to use for keeping track of models done
write.csv(observation_counts, "/Users/nfb/Dropbox/6-WILDOCEANS/data_summary.csv")

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
  temp = st_as_sf(temp, coords = c("LONGITUDE","LATITUDE"))
  st_write(temp,paste("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling/speciesdata/",i, ".shp", sep=""), append = TRUE)
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


list = list_datasets()
