# ---------------------------------------------------------------------------------
######### Shark and ray species distribution models (SDMs) - modelling script
#AUTHOR: Nina Faure Beaulieu (2021)
#PROJECT: the shark and ray conservation plan developed under the WILDOCEANS 3-year shark and ray project in South Africa  
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: this script runs and then projects the models
####

# ---------------------------------
# MODELING
# ---------------------------------

# Build individual models
static_models <- BIOMOD_Modeling(
  data, # your biomod object
  models = c('GLM', 'MAXENT.Phillips'), # 2 chosen models to run
  models.options = mxtPh, # add modified model parameters, if not remove this command
  NbRunEval = 10, # 10-fold cross validation (number of evaluations to run)
  DataSplit = 75, # % of data used for calibration,rest for testing
  models.eval.meth = c('TSS'), # evaluation method
  SaveObj = TRUE, # keep all results on hard drive 
  rescal.all.models = FALSE, # if true, all model prediction will be scaled with a binomial GLM
  modeling.id = target)

# Build ensemble model
static_ensemblemodel  <- BIOMOD_EnsembleModeling(
  modeling.output = static_models, # your models object
  chosen.models = 'all', # use all your models
  em.by='PA_dataset+repet', # the way the models will be combined to build the ensemble models.
  eval.metric = 'TSS', # which metric to use to keep models for the ensemble (requires the threshold below)
  eval.metric.quality.threshold = c(0.7), # only keep models with a TSS score >0.7
  prob.mean = T, #  Estimate the mean probabilities across predictions
  #prob.cv = T, # Estimate the coefficient of variation across predictions
)

# Individual model projections over current environmental variables
static_modelprojections =
  BIOMOD_Projection(
    proj.name = paste0(target,model_type), # new folder will be created with this name
    modeling.output = static_models, # your modelling output object
    new.env = stack_subset, # same environmental variables on which model will be projected
    selected.models = "all", # which models to project, in this case only the full ones
    binary.meth = 'TSS', # model evaluation method
    compress = 'xz', # to do with how r stores the file
    build.clamping.mask = FALSE,
    silent=TRUE)

# Ensemble model projection 
static_ensembleprojection = BIOMOD_EnsembleForecasting(
  EM.output = static_ensemblemodel,
  projection.output = static_modelprojections)

# get all models evaluation scores
all_evals = get_evaluations(static_models, as.data.frame = TRUE)
ensemble_evals = get_evaluations(static_ensemblemodel, as.data.frame = TRUE)
write.csv(all_evals,paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs/evaluations/",model_type,target,"allevals.csv"))
write.csv(ensemble_evals,paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs/evaluations/",model_type,target,"ensembleevals.csv"))

# Threshold calculation
# this function calculates the threshold at which a probability can be considered a presence
response = get_formal_data(static_models,"resp.var") # response variable 
predictions = get_predictions(static_ensemblemodel)[,1] # predicted variables
thresh = Find.Optim.Stat(Stat='TSS',
                         predictions,
                         response,
                         Nb.thresh.test = 100,
                         Fixed.thresh = NULL)[2]
thresh = as.data.frame(thresh) # save the output as a dataframe
write.csv(thresh,paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs/thresholds/",model_type,target,"thresh.csv")) # save the dataframe
rm(predictions,response) # remove unnecessary variables

# ---------------------------------
# PLOTTING
# ---------------------------------

# isolate ensemble prediction raster
en_preds = get_predictions(static_ensembleprojection) 


# PLOT 1 - CONTINUOUS DISTRIBUTION VALUES (pretty plots)
# this is the plot to use in any reports or papers
temp = en_preds[[1]] # temporary layer to turn 0 values to NA
temp[values(temp) == 0] = NA # turn 0 values to NA
plot = levelplot(temp,
          main = paste0(target,"\n",model_type),
          names.attr = c("Ensemble model"),
          par.settings = rasterTheme(viridis_pal(option="D")(10)),
          at = intervals,
          margin = FALSE)+
  # eez
  latticeExtra::layer(sp.polygons(eez,col = "black",lwd = 1))+
  # 250m isobath
  latticeExtra::layer(sp.polygons(contours, col = "black", lwd = 1))+
  # sa coast
  latticeExtra::layer(sp.polygons(sa_coast,col = "black",lwd= 1))+
  # points for main cities
  latticeExtra::layer(sp.points(places[c(1:3,5,6,18,20:22,10,14),],col = "black",pch = 20))+
  # coordinates and city names
  # done in three lines as a "pretty" position varies based on their place on the map
  latticeExtra::layer(sp.text(coordinates(places)[c(1:3,5,6),],places$Location[c(1:3,5,6)],col = "black",pch = 20,pos=4,cex = 0.5))+
  latticeExtra::layer(sp.text(coordinates(places)[c(18,20,21,22),],places$Location[c(18,20,21,22)],col = "black",pch = 20,pos=2,cex = 0.5))+
  latticeExtra::layer(sp.text(adjustedcoords,places$Location[c(10,14)],col = "black",pch = 20, pos=2,cex = 0.5))

# this saves the plot to a folder
png(file=paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs/prettyplots/",target,"_",model_type,"_","continuous_ensemble.png"), width=3000, height=3000, res=300)
print(plot)
dev.off()
rm(temp,plot) # remove unnecessary variables

# PLOT 2 - BINARY DISTRIBUTION VALUES
temp = en_preds[[1]] # temporary layer to turn values below the threshold to NA
values(temp)[values(temp)<thresh$thresh]=NA # turn values below threshold to NA
values(temp)[values(temp)>=thresh$thresh]=1 # turn values below threshold to NA
plot = levelplot(temp,
                 main = paste0(target,"\n",model_type,"\n","Binary presence absence map"),
                 names.attr=c("Ensemble model"),
                 margin = FALSE,
                 colorkey=FALSE)+
  latticeExtra::layer(sp.polygons(eez,col = "black"))

# this saves the plot to a folder
png(file=paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs/prettyplots/",target,"_",model_type,"_","binary_ensemble.png"), width=3000, height=3000, res=300)
print(plot)
dev.off()
rm(plot) # remove unnecessary variables

# PLOT 3, 4 and 5 - PLANNING SOFTWARE PLOTS
# these are the plots to use in the planning software
# they are simple rasters with probability values from 0 to 1000
# both plots (ensemble mean and ensemble coefficient of variation) are saved directly to a folder
writeRaster(en_preds[[1]],paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs/sdms/",target,"_",model_type,"ensemblemean.tiff"), overwrite = TRUE)
#writeRaster(en_preds[[2]],paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs/sdms/",target,"_",model_type,"ensemblecv.tiff"),  overwrite = TRUE)
writeRaster(temp,paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs/sdms/",target,"_",model_type,"ensemblemeanthreshold.tiff"),  overwrite = TRUE)
