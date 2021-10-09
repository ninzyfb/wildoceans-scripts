# ---------------------------------------------------------------------------------
######### Shark and ray species distribution models (SDMs) - prevalence script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: caluclates how much data is available as a percentage of EEZ area
####

# ---------------------------------
# FORMATTING
# ---------------------------------

# set CRS of observations
crs(obs.data) = crs(template)
# crop template if region is cropped
if(restrictedrange == "yes"){
  template = crop(template,extent(subset))
}
rm(subset)
# crop to template
obs.data = crop(obs.data, template)
if(length(obs.data)>0){
# find number of cells with data
obscells = cellFromXY(template, obs.data)
# find number of cells in eez
allcells = length(values(template)[!is.na(values(template))])
# get percentage of cells with data
perc = (length(obscells)/allcells)*100
rm(obscells)}else{perc = 0}
