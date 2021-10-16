
## Plot 2D response curves
temp <- BIOMOD_LoadModels(static_models, models = "GLM")
pdf.options(width = 10, height = 10, pointsize = 20)
response.plot2(
    models = temp,
    Data = get_formal_data(static_models, 'expl.var'),
    show.variables = get_formal_data(static_models,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = 'median',
    legend = FALSE,
    data_species = get_formal_data(static_models, 'resp.var'),
    save.file = "pdf",
    name = paste0(target,"responseplotsGLM"))
temp <- BIOMOD_LoadModels(static_models, models = "MAXENT.Phillips")
response.plot2(
  models = temp,
  Data = get_formal_data(static_models, 'expl.var'),
  show.variables = get_formal_data(static_models,'expl.var.names'),
  do.bivariate = FALSE,
  fixed.var.metric = 'median',
  legend = FALSE,
  data_species = get_formal_data(static_models, 'resp.var'),
  save.file = "pdf",
  name = paste0(target,"responseplotsMAXENT"))
