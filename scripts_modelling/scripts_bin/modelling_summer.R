# Build individual models
summer <- BIOMOD_Modeling(
  biomod_obj_seasons[[3]], # your biomod object
  models = c('GLM', 'GAM', 'MAXENT.Phillips'), # 3 chosen models to run
  models.options = mxtPh, # add modified model parameters, if not remove this command
  NbRunEval = 1, # 2-fold cross validation (number of evaluations to run)
  DataSplit = 75, # % of data used for calibration,rest for testing
  models.eval.meth = c('TSS'), # evaluation method
  SaveObj = TRUE, # keep all results on hard drive 
  rescal.all.models = FALSE, # if true, all model prediction will be scaled with a binomial GLM
  modeling.id = target)

# Build ensemble model
en_summer <- BIOMOD_EnsembleModeling(
  modeling.output = summer, # your models object
  chosen.models = 'all', # use all your models
  em.by='all', # the way the models will be combined to build the ensemble models.
  eval.metric = c('TSS'), # which metric to use to keep models for the ensemble (requires the threshold below)
  eval.metric.quality.threshold = c(0.7), # only keep models with a TSS score >0.7
  prob.mean = T, #  Estimate the mean probabilities across predictions
  prob.cv = T, # Estimate the coefficient of variation across predictions
)

# Individual model projections over current conditions
summer =
  BIOMOD_Projection(
    proj.name = target, # new folder will be created with this name
    modeling.output = summer, # your modelling output object
    new.env = stack_subset, # same environmental variables on which model will be projected
    selected.models = "all", # which models to project, in this case only the full ones
    binary.meth = 'TSS', # model evaluation method
    compress = 'xz', # to do with how r stores the file
    build.clamping.mask = FALSE,
    output.format = '.grd')

# Ensemble model projection 
en_summer = BIOMOD_EnsembleForecasting(
  EM.output = en_summer,
  projection.output = summer)

##### Pretty plots
library(rasterVis)
library(viridis)
en_preds = get_predictions(en_summer)
type = "Summer"
# set intervals for contours
intervals = seq(0,1000,100)

# run this after code has run as doesnt seem to like this step
eez = shapefile(list.files(pattern="eez.shp", recursive = TRUE))
temp = en_preds[[1]]
temp[values(temp) == 0] = NA
plot = levelplot(temp,
          main = paste0(target,"\n",type),
          names.attr=c("Ensemble model"),
          par.settings = rasterTheme(viridis_pal(option="D")(10)),
          at=intervals,
          margin = FALSE)+
  layer(sp.polygons(eez,col = "black"))
png(file=paste0("prettyplots/",target,"_",type,"_","ensemble.png"), width=3000, height=3000, res=300)
print(plot)
dev.off()
rm(temp)

##### Planning software plots
writeRaster(en_preds[[1]],paste0("sdms/",target,"_",type,"ensemblemean.tiff"), overwrite = TRUE)
writeRaster(en_preds[[2]],paste0("sdms/",target,"_",type,"ensemblecv.tiff"), overwrite = TRUE)

