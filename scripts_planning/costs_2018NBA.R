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

# ---------------------------------
# FISHING PRESSURE
# ---------------------------------
# Option 1
# create a binary layer based on the fishing pressure scores
# this is based on a quantile threshold
# used to lock out planning units with high fishing pressure
# binary layer based on the top 5th percentile of pressure scores
fp_threshold = raster::quantile(costs_all, probs = (1 - 0.05), na.rm = TRUE, names = FALSE)
fp_binary = round(costs_all >= fp_threshold)

# Option 2
# prevent total amount of fishing pressure scores from exceeding a certain threshold
# add a constraint to only select pus with fishing pressure scores
# that sum to a total of less than 20% of the total fishing pressure scores
fp_threshold = raster::cellStats(costs_all, "sum") * 0.2
# ---------------------------------
