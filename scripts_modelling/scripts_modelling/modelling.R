# ---------------------------------------------------------------------------------
# AUTHOR: Nina Faure Beaulieu (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script runs and projects the models
# ---------------------------------


# ---------------------------------
# OUTPUT FOLDER DESTINATION
# ---------------------------------
evalutationfolder = paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs/evaluations/")
plotfolder = paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs/prettyplots/")
# ---------------------------------


# ---------------------------------
# MODELLING
# ---------------------------------

# Build individual aseasonal models
# this static models object will contain 60 model projections
# this is because we are running 3 model algorithms (GLM, MAXENT, GAM)
# each model algorithm is being run 10 times as a cross-validation approach
# and these 10 runs are being run on two different set sof backgorund points
# 3 * 10 * 2 = 60
static_models <- BIOMOD_Modeling(
  data, # your biomod object
  VarImport = 5,
  models = c('GAM','GLM','MAXENT.Phillips'), # 3 modelling algorithms 
  models.options = mxtPh, # modified model parameters, unnecessary if you are happy with default biomod2 parameters
  NbRunEval = 10, # 10-fold cross validation (number of evaluations to run)
  DataSplit = 75, # 75% of data used for calibration, 25% for testing
  models.eval.meth = c('TSS'), # evaluation method, TSS is True Statistics Skill
  SaveObj = TRUE, # keep all results on hard drive 
  rescal.all.models = FALSE, # if true, all model prediction will be scaled with a binomial GLM
  modeling.id = target) # name of model = species name (target)

# get important variables
variables = as.data.frame(get_variables_importance(static_models))
# save
write.csv(variables,paste0(evalutationfolder,model_type,target,"_res",res,"_variableimportance.csv"), row.names = FALSE)

#rm(i,pa_xy,exp,pa,temp,pts_env,pts_env_seasons)

# Build ensemble model
static_ensemblemodel  <- BIOMOD_EnsembleModeling(
  modeling.output = static_models, # all model projections
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
    new.env = stack_model, # same environmental variables on which model will be projected
    selected.models = "all", # which models to project, in this case only the full ones
    binary.meth = 'TSS', # model evaluation method
    compress = 'xy', # to do with how r stores the file
    build.clamping.mask = FALSE)

# Ensemble model projection 
static_ensembleprojection = BIOMOD_EnsembleForecasting(
  EM.output = static_ensemblemodel,
  projection.output = static_modelprojections)

# get all models evaluation scores
all_evals = get_evaluations(static_models, as.data.frame = TRUE)
ensemble_evals = get_evaluations(static_ensemblemodel, as.data.frame = TRUE)
write.csv(all_evals,paste0(evalutationfolder,model_type,target,"_res",res,"_allevals.csv"))
write.csv(ensemble_evals,paste0(evalutationfolder,model_type,target,"_res",res,"_ensembleevals.csv"))

# Threshold calculation
# this function calculates the threshold at which a probability can be considered a presence
predictions = get_predictions(static_ensemblemodel)[,1] # predicted variables
response = get_formal_data(static_models,"resp.var") # response variable 
response[which(is.na(response))] = 0 # change NA to 0
thresh = Find.Optim.Stat(Stat='TSS',
                         Fit = predictions,
                         Obs = response,
                         Nb.thresh.test = 100,
                         Fixed.thresh = NULL)[2]
thresh = as.data.frame(thresh) # save the output as a dataframe
write.csv(thresh,paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs/thresholds/",model_type,target,"_res",res,"_thresh.csv")) # save the dataframe
rm(predictions,response) # remove unnecessary variables
# ---------------------------------


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
  latticeExtra::layer(sp.polygons(sa_coast,col = "black",fill = "white",lwd= 1))+
  # points for main cities
  latticeExtra::layer(sp.points(places[c(1:3,5,6,18,20:22,10,14),],col = "black",pch = 20))+
  # coordinates and city names
  # done in three lines as a "pretty" position varies based on their place on the map
  latticeExtra::layer(sp.text(coordinates(places)[c(1:3,5,6),],places$Location[c(1:3,5,6)],col = "black",pch = 20,pos=4,cex = 0.5))+
  latticeExtra::layer(sp.text(coordinates(places)[c(18,20,21,22),],places$Location[c(18,20,21,22)],col = "black",pch = 20,pos=2,cex = 0.5))+
  latticeExtra::layer(sp.text(adjustedcoords,places$Location[c(10,14)],col = "black",pch = 20, pos=2,cex = 0.5))

# this saves the plot to a folder
png(file=paste0(plotfolder,target,"_",model_type,"_res",res,"_continuous_ensemble.png"), width=3000, height=3000, res=300)
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
png(file=paste0(plotfolder,target,"_",model_type,"_res",res,"_binary_ensemble.png"), width=3000, height=3000, res=300)
print(plot)
dev.off()
rm(plot) # remove unnecessary variables

# PLOT 3, 4 and 5 - PLANNING SOFTWARE PLOTS
# these are the plots to use in the planning software
# they are simple rasters with probability values from 0 to 1000
# both plots (ensemble mean and ensemble coefficient of variation) are saved directly to a folder
writeRaster(en_preds[[1]],paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs/sdms/",target,"_",model_type,"_res",res,"_ensemblemean.tiff"), overwrite = TRUE)
#writeRaster(en_preds[[2]],paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs/sdms/",target,"_",model_type,"ensemblecv.tiff"),  overwrite = TRUE)
writeRaster(temp,paste0(path,"Dropbox/6-WILDOCEANS/Modelling/Outputs/sdms/",target,"_",model_type,"_res",res,"_ensemblemeanthreshold.tiff"),  overwrite = TRUE)
# ---------------------------------