library(corrplot)

# variables at occurences
sp = list_pa[[1]][,-1]
cors = abs(cor(sp, use = "complete.obs"))
corrplot.mixed(cors,tl.pos='lt', tl.cex=0.6, number.cex=0.5, addCoefasPercent=T)

# all variables
stack$substrate_simplified = as.factor(stack$substrate_simplified)
all = values(stack)
cors = abs(cor(all, use = "complete.obs"))
corrplot.mixed(cors,tl.pos='lt', tl.cex=0.6, number.cex=0.5, addCoefasPercent=T)
?corrplot.mixed
