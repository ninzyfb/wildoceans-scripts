# packages
library(dplyr)
library(readxl)
library(xlsx)
library(stringr)

# data
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Planning/targets")
sharks = read_excel("master_db.xlsx", sheet = 2)
rays = read_excel("master_db.xlsx", sheet = 3)
all = rbind(sharks,rays)
all[,c(1,6:10,12:17,19:22,29,30)] = NULL # remove unecessary columns
# rename columsn to shorter names
colnames(all)[c(3,4,5,6,7,8,9,10,11,12)] = c("SPECIES_SCIENTIFIC","common_name","Endemism","IUCN","MLRA","TOPS","CMS","CITES","SBMP","NPAO")
# remove date from IUCN assessment
for(i in 1:nrow(all)){
  all$IUCN[i] = strsplit(all$IUCN," ")[[i]][1]
}
# change endemism code to what it actually means
all = all %>%
  mutate(Endemism = ifelse(Endemism == "1","South African",
                           ifelse(Endemism == "2","Southern African",NA)))

# only keep target species
species = read.csv("/Users/nfb/Dropbox/6-WILDOCEANS/data_summary_master.csv")
species = species %>%
  filter(Static.1 == "yes") %>%
  select(SPECIES_SCIENTIFIC)

# simplify species name from db dataframe
all$SPECIES_SCIENTIFIC = toupper(all$SPECIES_SCIENTIFIC)
for(i in 1:nrow(all)){
  all$SPECIES_SCIENTIFIC[i] = paste((strsplit(all$SPECIES_SCIENTIFIC," ")[[i]][1]),(strsplit(all$SPECIES_SCIENTIFIC," ")[[i]][2]))
}

species$SPECIES_SCIENTIFIC = toupper(species$SPECIES_SCIENTIFIC)

# keep target species with their assesments and save
all_2 = left_join(species,all)

# write 
write.xlsx(all_2,"scores_v3.xlsx")


