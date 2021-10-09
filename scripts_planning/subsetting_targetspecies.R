
# Load features file names
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling")
files = list.files(pattern = features, recursive = TRUE)

# load data spreadsheet
setwd("/Users/nfb/Dropbox/6-WILDOCEANS")
species = read.csv("data_summary_master.csv")
# refine to only keep target species
species = species %>%
  filter(Cons.plan.target == "yes") %>%
  filter(Static == "yes")
species = toupper(species$SPECIES_SCIENTIFIC)
species = as.data.frame(species)
species$target = "yes"
colnames(species)[1] = "names"

# extract species names from files
list = list()
for(i in 1:length(files)){
  temp = str_split(files,"/")[i]
  temp = temp[[1]][3]
  temp = str_split(temp,"_")[[1]][1]
  list[[i]] = temp
}
rm(i,temp)

names = unlist(list)
rm(list)

names = as.data.frame(names)
files = as.data.frame(files)
files = cbind(files,names)
rm(names)


files = left_join(files,species)
rm(species)
files = files %>% filter(target == "yes")
files = files$files
