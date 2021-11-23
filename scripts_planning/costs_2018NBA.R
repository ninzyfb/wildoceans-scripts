# ---------------------------------------------------------------------------------
######### Shark and ray species conservation planning using prioritizr - costs_2018NBA script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: loads the fishing pressure layers and assigns the correct cost to each species
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
rm(nbafiles)

# raw file
#temp = raster(list.files(pattern = "Pole Tuna Intensity.tif",recursive = TRUE,full.names = TRUE)[1])
#pu_projected = projectRaster(pu,temp)
#temp2 = mask(temp,pu)

# fishing threats
#threats = read_xlsx(list.files(pattern = "fisheries-risk.xlsx", recursive = TRUE,full.names = TRUE),skip=1)

# ---------------------------------
# FORMATTING
# ---------------------------------

# remove nba5km from stack names
names = vector()
for(i in 1:length(names(costs))){
  names[i] = str_split(names(costs),"_NBA5km")[[i]][1]}
names(costs) = names
rm(names, i) # remove unnecessary variables
costs = stack(costs)

# rescale all threat between 0 and 1
costs_scaled = rescale(costs)
plot(costs_scaled)

# create one main layer as summed costs
costs_all = calc(costs_scaled,sum, na.rm = TRUE)
costs_all = mask(costs_all,pu)
# you need to add pu other other cells will have cost of 0
costs_all = costs_all+pu

# pretty plot of fishing pressur
#png(file=paste0("Planning/Outputs/fishingpressure_all.png"),width=3000, height=2000, res=300)
#plot(costs_all)
#plot(mpa_layer, add = TRUE, alpha = 0.5,legend = FALSE) # allows notake mpas to be seen as well
#plot(mpas,add = TRUE,legend = FALSE)
#plot(eez, add = TRUE,legend = FALSE)
#dev.off()

# pivot threats 
#threats_v2 = threats %>%
#  pivot_longer(!species_scientific,names_to = "fisheries",values_to = "affected" ) %>%
#  filter(!is.na(affected))
#rm(threats)

#sppthreatsstack = stack()
#for(i in unique(threats_v2$species_scientific)){
# start of loop
#temp = threats_v2 %>%
#  filter(species_scientific == i)
#spp_fisheries = unique(temp$fisheries)  
# layers to keep
#keep = names(costs) %in% spp_fisheries
# vector of layers to keep
#keep2 = which(keep == TRUE)
# new stack of refined layers
#tempthreats = costs[[keep2]]
# add all threats together
#tempthreatssum = calc(tempthreats,sum, na.rm = TRUE)
#sppthreatsstack = addLayer(sppthreatsstack,tempthreatssum)
#rm(keep,keep2,tempthreats,tempthreatssum)
#}

# rescale all between 0 and 1
#sppthreatsstack_scaled = rescale(sppthreatsstack)
#plot(sppthreatsstack_scaled)

# sum to one final cost layer
#finalthreatlayer = calc(sppthreatsstack_scaled,sum, na.rm = TRUE)
#plot(finalthreatlayer)


