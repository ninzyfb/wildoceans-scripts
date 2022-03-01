for(i in 1:nlayers(feature_stack_aseasonal_targetsonly)){
  look = sum(values(feature_stack_aseasonal_targetsonly[[i]]),na.rm = TRUE)
  vector = c(vector,look)
}

vector = vector()
sort(round(vector,0))
names(feature_stack_aseasonal_targetsonly)
