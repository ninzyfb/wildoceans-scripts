# ---------------------------------------------------------------------------------
# AUTHOR: Nina Faure Beaulieu (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script loads all the IUCN shapefiles and creates a rasterstack
# ---------------------------------


# ---------------------------------
# DATA
# ---------------------------------
# extract names of IUCN range maps
files = list.files(path = "wildoceans-scripts/IUCN/Sharks_rays_SA_raw",pattern = paste(".gpkg",sep=""), recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
# ---------------------------------


# ---------------------------------
# FORMATTING
# ---------------------------------

# extract scientific name from file name
names = toupper(files)
names = str_split(names,toupper("wildoceans-scripts/IUCN/Sharks_rays_SA_raw/"), simplify = TRUE)[,2]
names = str_split(names,".GPKG", simplify = TRUE)[,1]
# all iucn maps
iucn_stack_all = stack()
for(i in 1:length(files)){
  temp = st_read(files[i])
  temp = fasterize(temp,pu)
  iucn_stack_all = addLayer(iucn_stack_all,temp)
}
# add names to rasterstack
names(iucn_stack_all) = names

# extract scientific name from file name
names = toupper(files)
names = str_split(names,toupper("wildoceans-scripts/IUCN/Sharks_rays_SA_raw/"), simplify = TRUE)[,2]
names = str_split(names,".GPKG", simplify = TRUE)[,1]

# only keep IUCN ranges from ones which you have distribution data for
files = files[names %in% featurenames$SPECIES_SCIENTIFIC]
# only keep those species' names 
names = names[names %in% featurenames$SPECIES_SCIENTIFIC]

# not all species have an IUCN range map 
# currently missing for Etmopterus granulosus, DEANIA CALCEUS is DEANIA CALCEA, HIMANTURA LEOPARDA
featurenames$SPECIES_SCIENTIFIC[!(featurenames$SPECIES_SCIENTIFIC %in% names)]

# create raster stack of iucn range maps (only for species with modelled data)
iucn_stack = stack()
for(i in 1:length(files)){
temp = st_read(files[i])
temp = fasterize(temp,pu)
iucn_stack = addLayer(iucn_stack,temp)
}

# add names to maps
filtered = featurenames %>%
  filter(SPECIES_SCIENTIFIC %in% names) %>%
  filter(MODELTYPE == "ASEASONAL")
names(iucn_stack) = filtered$FEATURENAME
plot(iucn_stack)
# remove CARCHARHINUS_AMBLYRHYNCHOS
iucn_stack = dropLayer(iucn_stack,13)

# ---------------------------------
