# ---------------------------------------------------------------------------------
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script loads any features required for plotting
# This script also sets some plotting parameters to ensure uniform plots
# ---------------------------------


# ---------------------------------
# PLOTTING FEATURES
# ---------------------------------
# South African EEZ
eez = shapefile(list.files(pattern="eez.shp", recursive = TRUE, full.names = TRUE)) # load eez

# West, South and East marine regions as defined by Ebert et al.
regions = shapefile(list.files(pattern = "ebert_regions.shp", recursive = TRUE,full.names = TRUE))

# isobaths
contours = shapefile(list.files(pattern="contoursGEBCO.shp", recursive = TRUE, full.names=TRUE)) 

# South African province outline
sa  <- getData("GADM",country="South Africa",level=1)

# Names of coastal cities
places = shapefile(list.files(pattern="ebert_placenames.shp", recursive = TRUE, full.names=TRUE)) 

# South African continetnal marine protected areas
mpas = st_read(list.files(pattern ="SAMPAZ_OR_2020_Q3.shp" ,recursive = TRUE, full.names = TRUE))

# South African continetnal marine protected areas (no takes only)
mpas_notake = st_read(list.files(pattern ="mpa_layer_protected.shp" ,recursive = TRUE, full.names = TRUE))

# ---------------------------------


# ---------------------------------
# FORMATTING
# ---------------------------------

# REGIONS # -----------
# turn region names to upper case
regions$Region = toupper(regions$Region)
# create west south and east vector
# this will come into handdy during plotting
range = c("WEST","SOUTH","EAST")

# ISOBATHS # -----------
# extract 250m isobath
contours = contours[which(contours$DEPTH=="-250"),]

# PROVINCE OUTLINE # -----------
# simplify provinces to reduce the weight of the file
sa = st_as_sf(sa)
sa <- st_simplify(sa,dTolerance=100)
sa = sa %>%
  # only keep coastal provinces
  filter(NAME_1 %in% c("Eastern Cape","KwaZulu-Natal","Northern Cape","Western Cape")) %>%
  group_by(GID_0) %>%
  summarise()
sa = as(sa, Class = "Spatial")

# PLACES # -----------
# adjust coordinates for some cities 
# this is to ensure the name is displayed in a "pretty" way on the map
adjustedcoords = coordinates(places)[c(10,14),]
adjustedcoords[,2] = adjustedcoords[,2]+0.2

# MPAS # -----------
# simplify MPA shapefile
mpas = mpas %>%
  filter(CUR_NME != "Prince Edward Island Marine Protected Area") %>%
  group_by(CUR_ZON_NM,CUR_NME) %>%
  summarise()
mpas = st_simplify(mpas,dTolerance = 100)
mpas = as(mpas, Class = "Spatial")

# simplify MPA no take shapefile
mpas_notake = st_simplify(mpas_notake,dTolerance = 100)
mpas_notake = as(mpas_notake, Class = "Spatial")

# add take and no-take specification to mpas dataframe
mpas@data = mpas@data %>%
  mutate(type = ifelse(CUR_ZON_NM %in% mpas_notake$CUR_ZON_NM,"no-take","take"))

# get all "take" mpas
mpas_take = mpas - mpas_notake

# mpa labels
mpa_labels = st_as_sf(mpas)
mpa_labels = mpa_labels %>%
  group_by(CUR_NME) %>%
  summarise()
mpa_labels$CUR_NME = str_split(mpa_labels$CUR_NME," Marine Protected Area", simplify = TRUE)[,1]
mpa_labels$number = as.numeric(as.factor(mpa_labels$CUR_NME))
mpa_labels = as(mpa_labels, Class = "Spatial")

# label dot
label_coords = data.frame(mpa_labels$number)
coordinates(label_coords) = data.frame(coordinates(mpa_labels))


# OTHER # -----------
# set intervals for modelling
intervals = seq(0,1000,200)

# set intervals for planning plot
intervals2 = seq(0,1,0.1)

# colours for plots
cols <- colorRampPalette(c("white","darkgreen"))
cols2 <- colorRampPalette(c("yellow"))

# Expert extents
load(list.files(pattern = "points.RData", recursive = TRUE, full.names = TRUE))
expert_extent = points
colnames(expert_extent)[1] = "Scientific_name" 
expert_extent = as(expert_extent, Class = "Spatial")
expert_extent$Scientific_name = tolower(expert_extent$Scientific_name)
rm(points)

# blank template (for raw plots script)
blank_template = raster(list.files(pattern = "template_10km.tif", recursive = TRUE, full.names = TRUE))
values(blank_template) = 0

# legend coordinates (for raw plots script)
legend = data.frame()
legend[1,1] = 36
legend[1,2] = -36.5
legend[2,1] = 36
legend[2,2] = -37
legend[3,1] = 36
legend[3,2] = -37.5
coordinates(legend) <- ~V1+V2

# color palette
manual.col = colorRampPalette(c("#f7f6fd","#4635d0"))
