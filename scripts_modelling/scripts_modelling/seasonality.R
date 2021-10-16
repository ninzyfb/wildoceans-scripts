# ---------------------------------------------------------------------------------
######### Shark and ray species distribution models (SDMs) - seasonality script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: seperates the data into a summer and winter season

# ---------------------------------
# FORMATTING
# ---------------------------------

# add month variable from Date
obs.data = obs.data %>%
  mutate(Month = month(obs.data$DATE2))

# group observations by austral Summer (Sep,Oct,Nov,Dec,Jan,Feb) and Winter (Mar,Apr,May,Jun,Jul,Aug)
obs.data = obs.data %>%
  mutate(SEASON = ifelse(is.na(SEASON) & Month %in% c(3,4,5,6,7,8),"Winter",
                         ifelse(is.na(SEASON) & Month %in% c(9,10,11,12,1,2), "Summer",
                                ifelse(SEASON == "Autumn","Winter",
                                       ifelse(SEASON == "Spring","Summer",SEASON)))))%>%
  mutate(SEASON = ifelse(SEASON == "",NA,SEASON)) # if SEASON variable is empty, convert to NA

# convert to spatial points dataframe
obs.data$LONGITUDE = as.numeric(obs.data$LONGITUDE)
obs.data$LATITUDE = as.numeric(obs.data$LATITUDE)
coordinates(obs.data) =  ~ cbind(obs.data$LONGITUDE,obs.data$LATITUDE)

# set CRS of observations
template = raster(list.files(pattern = "template.tif", recursive = TRUE, full.names = TRUE))
crs(obs.data) = crs(template)

# ---------------------------------
# PLOTTING
# ---------------------------------

# plot observations per month
# IMPORTANT! this is not showing variations in abundance of the species over month
# we have no way of knowing if sampling effort is constant throughout the year
# this will also omit fishing data which has only season but no month
png(paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs/seasonal_observations/Montly-obs_",target,".png"),width = 20, height = 10, units = "cm", res = 300)
plot(table(obs.data$Month),
     ylab = "Number of observations",
     xlab = "Month",
     main = paste("Monthly observations of",target))
dev.off()

# plot observations per season
table = table(obs.data$SEASON)
if(length(table)>0){
  png(paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs/seasonal_observations/Seasonal-obs_",target,".png"),width = 20, height = 10, units = "cm", res = 300)
  par(lwd = 0.5)
  plot(table(obs.data$SEASON),
     ylab = "Number of observations",
     xlab = "Season",
     main = paste("Seasonal observations of",target))
  dev.off()
}


# look at distribution per season
# there is likely to also be bias in sampling effort and location per season
# this is dealt with accordingly in the subsampling and pseudo-absence scripts
ggplot(data = st_as_sf(obs.data)) +
  geom_sf(aes(col=SEASON),size = 1)+
  theme_bw()

