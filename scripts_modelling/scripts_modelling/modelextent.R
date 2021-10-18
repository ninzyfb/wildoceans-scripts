# ---------------------------------------------------------------------------------
######### Shark and ray species distribution models (SDMs) - modelextent script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: refines the range that will be modelled for the target species
# some species have very range restricted ranges and they require a reduced geographic extent

# ---------------------------------
# DATA
# ---------------------------------

# turn region names to upper case
regions$Region = toupper(regions$Region)

# define the new extent using the regions specified in model paramters
subset = regions[regions$Region%in%range,]

# crop raster stack to new regions
stack_new = raster::crop(stack,extent(subset))
stack_new = stack(stack_new)
stack_new$substrate_simplified = as.factor(stack_new$substrate_simplified) # turn substrate to a factor
