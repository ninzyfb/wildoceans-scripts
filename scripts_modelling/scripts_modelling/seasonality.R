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
  dev.off()}
