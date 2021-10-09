library(biomod2)

## Plot 2D response curves
temp <- BIOMOD_LoadModels(static_ensemblemodel)[1]
rp2 = response.plot2(
    models = temp,
    Data = get_formal_data(static_ensemblemodel, 'expl.var'),
    show.variables = get_formal_data(static_ensemblemodel,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = 'median',
    col = c("blue", "red"),
    legend = TRUE,
    data_species = get_formal_data(static_ensemblemodel, 'resp.var')
  )

## Plot 3D response curves
response.plot2(
  models = GLMs[1],
  Data = get_formal_data(static, 'expl.var'), 
  show.variables = get_formal_data(static, 'expl.var.names'),
  do.bivariate = TRUE,
  fixed.var.metric = 'median',
  data_species = get_formal_data(static, 'resp.var'),
  display_title = FALSE
)

look = get_predictions(static)
dimnames(look)


# get all models evaluation scores
all_evals = get_evaluations(static_models)
ensemble_evals = get_evaluations(ensemble)

# Print TSS and ROC scores of all models
dimnames(all_evals) # print the dimnames
all_evals[c("TSS"),"Testing.data",,,]

# Print TSS and ROC scores of ensemble model
ensemble_evals
