# Assis, J. (2020)
# R Pipelines to reduce the spatial autocorrelation in Species Distribution Models.
# theMarineDataScientist
# -----------------------------------------------
# Read libraries and main functions

library(raster)
library(ggplot2)
library(sdmpredictors)
library(ecodist)
library(sp)
library(spThin)

source(list.files(pattern = "functions.R", recursive=TRUE))

# -----------------------------------------------
# Read occurrence records 
occurrenceRecords = data.frame(coordinates(pts))

# Define the distance class 
autocorrelationClassDistance <- 5

# Define the maximum distance at which autocorrelation will be determined
autocorrelationMaxDistance <- 500

# Define the significance level of the test
autocorrelationSignif <- 0.05

distanceUncorr <- data.frame(Predictor=names(stack),Distance=NA)
for( i in 1:length(names(stack))) {
  distanceUncorr[i,2] <- spatialAutocorrelation(occurrenceRecords=occurrenceRecords,subset(stack,i),autocorrelationClassDistance,autocorrelationMaxDistance,autocorrelationSignif)
}

distanceUncorrPlot <- ggplot(distanceUncorr[sort(distanceUncorr[,2],decreasing = TRUE,index.return=TRUE)$ix,]) +
  geom_bar( aes(x= reorder(Predictor, Distance) , y=Distance), stat="identity", fill="black", alpha=0.5) +
  coord_flip() + theme(
    axis.text=element_text(size=12),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0) , size=12),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0) , size=12),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "#EFEFEF", colour = "#EFEFEF",size = 0, linetype = "solid"),
    panel.grid.major = element_line(size = 0, linetype = 'solid',colour = "#EFEFEF"), 
    panel.grid.minor = element_line(size = 0, linetype = 'solid',colour = "#EFEFEF")
  ) + labs(x = "Predictor") + 
  labs(y = "Spatial correlation (km)") + geom_hline(aes(yintercept=round(mean(distanceUncorr[,2]),digits=2)   ),color="Black", linetype="dashed", size=0.3) +
  annotate("text", y = round(mean(distanceUncorr[,2]),digits=2) + 2 , x = 1 , label = paste0( round(mean(distanceUncorr[,2]),digits=2)," Km") , hjust = 0)

distanceUncorrPlot

meanCorrDistance <- mean(distanceUncorr[,2])

# Prune occurrence recors 
pts_subset <- spatialThinning(occurrenceRecords,stack, meanCorrDistance )

plot(coordinates(pts))
plot(pts_subset, add = TRUE, col = "red")
