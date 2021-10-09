##### Directory
setwd("/Users/nfb/Dropbox/6-WILDOCEANS/Modelling") # set to modelling folder
getwd()

#### Packages
library(dismo)
library(rpart)

#### GLM model
m1 <- glm(pa~., data=combined) # glm model
summary(m1) # look at summary
p_glm = predict(variable_stack, m1) # predictions

par(mar=c(1,1,1,1))
plot(p_glm) # look at predictive distribution
plot(p_glm>0.8) # look at thresholds

#### Bioclim model
bc <- bioclim(combined[1:380,-1]) # remove the first column which indicates presence, and only keep first 380 rows as rest are background samples
p_bioclim = predict(variable_stack, bc) # predictions
plot(p_bioclim)
evaluate(bc)
?evaluate


# 1 - run bioclim model on presence training points
bc_test <- bioclim(combined[1:380,-1], test) # remove the first column which indicates presence, and only keep first 380 rows as rest are background samples
test = pts_subset[1:90]
test = test[c(1:190),]
?bioclim





