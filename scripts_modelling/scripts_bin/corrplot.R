library(corrplot)

# all variables
stack$substrate_simplified = as.factor(stack$substrate_simplified)
all = values(stack)
cors = abs(cor(all, use = "complete.obs"))
corrplot.mixed(cors,tl.pos='lt', tl.cex=0.6, number.cex=0.5, addCoefasPercent=T)
?corrplot.mixed

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
library(Hmisc)
res2<-rcorr(all)
coor_table = flattenCorrMatrix(res2$r, res2$P)
coor_table %>%
  filter(cor<0.7) %>%
  filter(cor>-0.7) %>%
  group_by(row) %>%
  summarise()
