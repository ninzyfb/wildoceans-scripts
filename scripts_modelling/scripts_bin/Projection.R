# Individual model projections over current conditions
static =
  BIOMOD_Projection(
  proj.name = target, # new folder will be created with this name
  modeling.output = static, # your modelling output object
  new.env = list_stack[[1]], # same environmental variables on which model will be projected
  selected.models = "all", # which models to project, in this case only the full ones
  binary.meth = 'TSS', # model evaluation method
  compress = 'xz', # to do with how r stores the file
  build.clamping.mask = FALSE,
  output.format = '.grd')

summer =
  BIOMOD_Projection(
    proj.name = target, # new folder will be created with this name
    modeling.output = summer, # your modelling output object
    new.env = list_stack[[2]], # same environmental variables on which model will be projected
    selected.models = "all", # which models to project, in this case only the full ones
    binary.meth = 'TSS', # model evaluation method
    compress = 'xz', # to do with how r stores the file
    build.clamping.mask = FALSE,
    output.format = '.grd')

winter =
  BIOMOD_Projection(
    proj.name = target, # new folder will be created with this name
    modeling.output = winter, # your modelling output object
    new.env = list_stack[[3]], # same environmental variables on which model will be projected
    selected.models = "all", # which models to project, in this case only the full ones
    binary.meth = 'TSS', # model evaluation method
    compress = 'xz', # to do with how r stores the file
    build.clamping.mask = FALSE,
    output.format = '.grd')

# save all predictions in a raster stack
projections_stack = get_predictions(static)
plot(projections_stack)
writeRaster(projections_stack,"projections_stack")
rm(projections_stack)

# Ensemble model projection 
en_static = BIOMOD_EnsembleForecasting(
  EM.output = en_static,
  projection.output = static)

en_summer = BIOMOD_EnsembleForecasting(
  EM.output = en_summer,
  projection.output = summer)

en_winter = BIOMOD_EnsembleForecasting(
  EM.output = en_winter,
  projection.output = winter)

# save ensemble prediction to a raster stack
projections_static = get_predictions(en_static)
projections_summer = get_predictions(en_summer)
projections_winter = get_predictions(en_winter)

# save single prediction file to folder
model_static = subset(projections_static,1)
model_summer = subset(projections_summer,1)
model_winter = subset(projections_winter,1)

plot(model_static)
plot(model_summer)
plot(model_winter)

model_static[model_static<500] = NA
model_summer[model_summer<500] = NA
model_winter[model_winter<500] = NA


writeRaster(model_static,paste("sdms/",target,"model_static",".tif",sep=""), overwrite = TRUE)
writeRaster(model_summer,paste("sdms/",target,"model_summer",".tif",sep=""), overwrite = TRUE)
writeRaster(model_winter,paste("sdms/",target,"model_winter",".tif",sep=""), overwrite = TRUE)

rm(target,pts,pts_subset,projections,projections_stack,projection_ensemble,projections_stack_ensemble)
