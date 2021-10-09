# Build individual models
models <- BIOMOD_Modeling(
  biomod_obj_seasons[[season]], # your biomod object with season specified in main script
  models = c('GLM', 'MAXENT.Phillips'), # 3 chosen models to run
  models.options = mxtPh, # add modified model parameters, if not remove this command
  NbRunEval = 10, # 2-fold cross validation (number of evaluations to run)
  DataSplit = 75, # % of data used for calibration,rest for testing
  models.eval.meth = c('TSS'), # evaluation method
  SaveObj = TRUE, # keep all results on hard drive 
  rescal.all.models = FALSE, # if true, all model prediction will be scaled with a binomial GLM
  modeling.id = target)

# Build ensemble model
ensemblemodel <- BIOMOD_EnsembleModeling(
  modeling.output = models, # your models object
  chosen.models = 'all', # use all your models
  em.by='all', # the way the models will be combined to build the ensemble models.
  eval.metric = c('TSS'), # which metric to use to keep models for the ensemble (requires the threshold below)
  eval.metric.quality.threshold = c(0.7), # only keep models with a TSS score >0.7
  prob.mean = T, #  Estimate the mean probabilities across predictions
  prob.cv = T, # Estimate the coefficient of variation across predictions
)

# Individual model projections over current conditions
projections_all =
  BIOMOD_Projection(
    proj.name = target, # new folder will be created with this name
    modeling.output = models, # your modelling output object
    new.env = stack_subset, # same environmental variables on which model will be projected
    selected.models = "all", # which models to project
    binary.meth = 'TSS', # model evaluation method
    compress = 'xz', # to do with how r stores the file
    build.clamping.mask = FALSE,
    output.format = '.grd')

# Ensemble model projection 
projection_ensemble = BIOMOD_EnsembleForecasting(
  EM.output = ensemblemodel,
  projection.output = projections_all)
?BIOMOD_EnsembleForecasting

# Threshold calculation
response = get_formal_data(models,"resp.var") # response variable 
predictions = get_predictions(ensemblemodel)[,1] # predicted variables
thresh = Find.Optim.Stat(Stat='TSS',
                         predictions,
                         response,
                         Nb.thresh.test = 100,
                         Fixed.thresh = NULL)[2]
rm(predictions,response)

##### Pretty plots
library(rasterVis)
library(viridis)
en_preds = get_predictions(projection_ensemble)

# set intervals for contours
intervals = seq(0,1000,100)
eez = shapefile(list.files(pattern="eez.shp", recursive = TRUE))

# PLOT 1 - CONTINUOUS DISTRIBTUION VALUES
temp = en_preds[[1]]
temp[values(temp) == 0] = NA
plot = levelplot(temp,
          main = paste0(target,"\n",model_type),
          names.attr=c("Ensemble model"),
          par.settings = rasterTheme(viridis_pal(option="D")(10)),
          at=intervals,
          margin = FALSE)+
  layer(sp.polygons(eez,col = "black"))
png(file=paste0("Outputs/prettyplots/",target,"_",model_type,"_","ensemble.png"), width=3000, height=3000, res=300)
print(plot)
dev.off()
rm(temp,plot)

# PLOT 2 - BINARY DISTRIBTUION VALUES
temp = en_preds[[1]] # temporary layer to turn 0 values to NA
temp[values(temp)<thresh] = NA # turn values below threshold to NA
temp[values(temp)>thresh] = 1 # turn values above threshold to 1
plot = levelplot(temp,
                 main = paste0(target,"\n",model_type,"\n","Binary presence absence map"),
                 names.attr=c("Ensemble model"),
                 margin = FALSE,
                 colorkey=FALSE)+
  layer(sp.polygons(eez,col = "black"))
png(file=paste0("Outputs/prettyplots/",target,"_",model_type,"_","binary_ensemble.png"), width=3000, height=3000, res=300)
print(plot)
dev.off()
rm(temp,plot)

# PLOT 3 - PLANNING SOFTWARE PLOTS
writeRaster(en_preds[[1]],paste0("Outputs/sdms/",target,"_",model_type,"ensemblemean.tiff"), overwrite = TRUE)
writeRaster(en_preds[[2]],paste0("Outputs/sdms/",target,"_",model_type,"ensemblecv.tiff"), overwrite = TRUE)
temp = en_preds[[1]] # temporary layer to turn 0 values to NA
temp[values(temp)<thresh] = NA # turn values below threshold to NA
temp[values(temp)>thresh] = 1 # turn values above threshold to 1
writeRaster(temp,paste0("Outputs/sdms/",target,"_",model_type,"ensemblebinarymean.tiff"), overwrite = TRUE)

rm(eez,models,ensemblemodel,projections_all,projection_ensemble,intervals,model_type,thresh,en_preds,temp)

