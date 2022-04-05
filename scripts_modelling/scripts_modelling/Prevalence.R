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
crs(obs.data) = crs(template_10)

# crop to template
obs.data = crop(obs.data, template_10)

# if there is still observation data after cropping
# get prevalence value
# otherwise percentage will be set to 0
if(length(obs.data)>0){

  # find number of unique cells with data
  obscells_10 = unique(cellFromXY(template_10, obs.data))

  # find number of cells in eez
  allcells_10 = length(values(template_10)[!is.na(values(template_10))])

  # get fraction of cells with data
  perc_10 = (length(obscells_10)/allcells_10)

  # number of obs cells
  obscells_10 = length(obscells_10)

}else{
  perc_10 = 0}
