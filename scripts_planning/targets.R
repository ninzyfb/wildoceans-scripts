# packages
library(dplyr)
library(readxl)
library(xlsx)
library(stringr)

# data
sharks = read_excel(list.files(pattern = "master_db.xlsx", recursive = TRUE,full.names = TRUE), sheet = 2)
rays = read_excel(list.files(pattern = "master_db.xlsx", recursive = TRUE,full.names = TRUE), sheet = 3)
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
rm(sharks,rays,i)
# only keep modelled species
master = read_xlsx(list.files(pattern = "data_summary_master.xlsx", recursive = TRUE,full.names = TRUE))
master_keep = master %>%
  filter(rounded_10>=1)
rm(master)
# only keep species
species = as.data.frame(master_keep$SPECIES_SCIENTIFIC)
colnames(species) = "SPECIES_SCIENTIFIC"
rm(master_keep)

# simplify species name from db dataframe
all$SPECIES_SCIENTIFIC = toupper(all$SPECIES_SCIENTIFIC)
for(i in 1:nrow(all)){
  all$SPECIES_SCIENTIFIC[i] = paste((strsplit(all$SPECIES_SCIENTIFIC," ")[[i]][1]),(strsplit(all$SPECIES_SCIENTIFIC," ")[[i]][2]))
}

species$SPECIES_SCIENTIFIC = toupper(species$SPECIES_SCIENTIFIC)

# keep target species with their assesments
# this is an updated version of the targets
# however this information lacks what was done manually
# so this sheet now needs to be compared to the up to date targets with the manual changes
all_2 = left_join(species,all)
rm(all)

# most up to date sheet
targets = read_xlsx(list.files(pattern = "species_targets.xlsx",recursive = TRUE), sheet = 1)
colnames(targets)[1] = "SPECIES_SCIENTIFIC"

# find added species
newspp = which(!(all_2$SPECIES_SCIENTIFIC %in% targets$SPECIES_SCIENTIFIC ))
newspp = all_2[newspp,]

# add to updated targets
targets = full_join(targets,newspp)

# write 
write.xlsx(targets,"species_targets.xlsx")


