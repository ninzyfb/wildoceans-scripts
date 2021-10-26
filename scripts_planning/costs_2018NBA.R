# ---------------------------------------------------------------------------------
######### Shark and ray species conservation planning using prioritizr - costs_2018NBA script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
#!Run each script one at a time as running the whole at once seems to cause some bugs
#the output line describes what each script produces
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: loads the fishing pressure layers and assigne the correct coast to each species
####

# ---------------------------------
# DATA
# ---------------------------------
# nba file names
nbafiles = list.files(pattern = "NBA5km.tif", recursive = TRUE,full.names = TRUE) # find file names
# convert to  raster stack
costs = stack(nbafiles)
# project to planning unit
costs = projectRaster(costs,pu)

# fishing threats
threats = read_xlsx(list.files(pattern = "fisheries-risk.xlsx", recursive = TRUE,full.names = TRUE),skip=1)

# ---------------------------------
# FORMATTING
# ---------------------------------

# remove nba5km from stack names
names = vector()
for(i in 1:length(names(costs))){
  names[i] = str_split(names(costs),"_NBA5km")[[i]][1]}
names(costs) = names

# pivot threats 
threats_v2 = threats %>%
  pivot_longer(!species_scientific,names_to = "fisheries",values_to = "affected" ) %>%
  filter(!is.na(affected))

sppthreatsstack = stack()
for(i in unique(threats_v2$species_scientific)){
# start of loop
temp = threats_v2 %>%
  filter(species_scientific == i)
spp_fisheries = unique(temp$fisheries)  
# layers to keep
keep = names(costs) %in% spp_fisheries
# vector of layers to keep
keep2 = which(keep == TRUE)
# new stack of refined layers
tempthreats = costs[[keep2]]
# add all threats together
tempthreatssum = calc(tempthreats,sum, na.rm = TRUE)
sppthreatsstack = addLayer(sppthreatsstack,tempthreatssum)
rm(keep,keep2,tempthreats,tempthreatssum)
}

# rescale all between 0 and 1
sppthreatsstack_scaled = rescale(sppthreatsstack)
plot(sppthreatsstack_scaled)

# sum to one final cost layer
finalthreatlayer = calc(sppthreatsstack_scaled,sum, na.rm = TRUE)
plot(finalthreatlayer)


