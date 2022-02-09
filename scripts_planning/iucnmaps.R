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
files = list.files(path = "Modelling/Ranges/IUCN",pattern = paste(".gpkg",sep=""), recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
# ---------------------------------


# ---------------------------------
# FORMATTING
# ---------------------------------

# extract scientific name from file name
names = toupper(files)
names = str_split(names,"MODELLING/RANGES/IUCN/SHARKS_RAYS_SA_RAW/", simplify = TRUE)[,2]
names = str_split(names,".GPKG", simplify = TRUE)[,1]

# only keep IUCN ranges from ones which you have distribution data for
files = files[names %in% featurenames$SPECIES_SCIENTIFIC]
# only keep those species' names 
names = names[names %in% featurenames$SPECIES_SCIENTIFIC]

# not all species have an IUCN range map 
# currently missing for Etmopterus granulosus
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
  filter(MODELTYPE == "Aseasonal")
names(iucn_stack) = filtered$FEATURENAME
plot(iucn_stack)
# ---------------------------------
