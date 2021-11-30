SA_env = na.omit(as.data.frame(stack[[1:27]]))
pca_ZA <- dudi.pca(SA_env,scannf = F, nf = 2)
plot(pca_ZA$li[,1:2])
