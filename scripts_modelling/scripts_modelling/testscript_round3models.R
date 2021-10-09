files = list.files( path = "Outputs/prettyplots/",pattern = "Aseasonal_binary_ensemble.png", recursive = TRUE)
files = strsplit(files,"_Aseasonal_binary_ensemble.png")
files = unlist(files)

library(readxl)
library(dplyr)
master = read_xlsx("/Users/nfb/Dropbox/6-WILDOCEANS/data_summary_master.xlsx")
master = as.data.frame(master)

master = master %>% # filter to only keep species with enough data to run static model in round 2
  filter(Round2_Static == "yes") %>%
  select(c(SPECIES_SCIENTIFIC,Round3_seasonal,Round3_Fisheries,Round3_Substrate,Round3_Range))

master = master %>%
  filter(!is.na(SPECIES_SCIENTIFIC)) # remove empty rows

# only keep species that have no restricted range
master_run = master %>% # remove empty rows
  filter(Round3_Range == "yes") %>% # only keep full range species
  filter(Round3_Fisheries == "no") %>% # that don't include fisheries
  filter(Round3_Substrate == "no") # that include substrate

master_run = master_run[c(3),]
getwd()
range = c("East","South")

for(i in 1:nrow(master_run)){
  target = master_run$SPECIES_SCIENTIFIC[i]
  folder = "speciesdata/"
  group = "species"
  restrictedrange = "yes"
  removesubstrate = "yes"
  seasonal = "yes"
  fisheries = "no"
  source(list.files(pattern = "mainscript.R", recursive = TRUE))
  
}
