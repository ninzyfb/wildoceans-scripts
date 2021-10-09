# This script is to add the costs (aka Pressures to the planning unit)
# the number of pressures added will depend on the scenario run


# ---------------------------------
# DATA
# ---------------------------------
# Fishing pressure
files = list.files(pattern = "safinal.tif" , recursive = TRUE)
# stackfiles
fishingrasters = stack(files)
# rescale all values from 0 to 1
for(i in 1:nlayers(fishingrasters)){
values(fishingrasters[[i]]) = rescale(values(fishingrasters[[i]]), to = c(0,10))}
rm(files,i)

# White cage diving pressure
# filenames
files = list.files(pattern = "cagediving.tif" , recursive = TRUE)
# stackfiles
cagedivingraster = stack(files)

# KZN nets pressure
# filenames
files = list.files(pattern = "sharknets.tif" , recursive = TRUE)
# stackfiles
sharknetsraster = stack(files)
rm(files) # remove

# add all pressures to the planning unit
# this means all units with no pressures will keep their value of 1
# other planning units will have the sum of all pressures
stack = stack(pu,cagedivingraster,fishingrasters,sharknetsraster)
names(stack)
pu_updated = calc(stack, fun = sum, na.rm = TRUE) # sum all pressure layers together
rm(stack,cagedivingraster,fishingrasters,sharknetsraster) # remove
pu_costs = mask(pu_updated, pu) # remove values outside of EEZ
rm(pu_updated)



