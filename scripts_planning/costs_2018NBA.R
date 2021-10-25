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
# fishing threats
threats = read_xlsx(list.files(pattern = "fisheries-risk.xlsx", recursive = TRUE,full.names = TRUE),skip=1)



