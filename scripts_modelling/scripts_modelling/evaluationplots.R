library(biomod2)
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library(ggpubr)

# all files
models_all = list.files(pattern = "models.out", full.names = TRUE, recursive = TRUE)

# single models and ensemble models
ensembles_all = models_all[which(str_detect(models_all,"ensemble"))]
models_all = models_all[-which(str_detect(models_all,"ensemble"))]

# evaluation plots for single models
for(i in 1:length(models_all)){
  temp = get(load(models_all[i]))
  name = temp@sp.name
  a = bm_PlotEvalBoxplot(bm.out = temp,group.by = c('run','dataset'))
  b = bm_PlotEvalBoxplot(bm.out = temp,group.by = c('run','algo'))
  c = bm_PlotEvalBoxplot(bm.out = temp,group.by = c('dataset','algo'))
  ggarrange(a, b, c, 
            labels = c("A", "B", "C"),
            ncol = 2, nrow = 2)%>%
    ggexport(filename = paste0(name,".png"),width=3000, height=2000, res=300)
}
