#### Packages
library(fuzzySim) 
library(dismo)
library(caret)

?findCorrelation
all_noNAs = na.omit(all)
remove = findCorrelation(cor(all_noNAs), cutoff = 0.7,names = TRUE)
names(stack)[!(names(stack)%in%remove)]

#### Variable selection function
list_variables = list()
for(i in 1:length(list_pa)){
  temp = list_pa[[i]]
  temp$substrate_simplified = NULL
  temp = multGLM(temp, sp.cols = 1 , var.cols = 2:16, FDR = TRUE, corSelect = TRUE, cor.thresh = 0.7, step = FALSE, trim = FALSE)
  list_variables[[i]] = temp$variables$pa # variables that remain
  list_variables[[i]] = c(list_variables[[i]],"substrate_simplified")
}
rm(temp,i)

# filter df to only keep chosen variables
for(i in 1:length(list_pa)){
  pa = list_pa[[i]]$pa
  list_pa[[i]] = list_pa[[i]][,names(list_pa[[i]])%in%list_variables[[i]]] # filter df to only keep chosen variables
  list_pa[[i]] = cbind(pa,list_pa[[i]])}
rm(i,pa)

# filter stack to only keep chosen variables
list_stack = list()
for(i in 1:length(list_pa)){
  list_stack[[i]] = stack
  # invalid layer names ommited error is normal as refers to pa column
  list_stack[[i]] = subset(list_stack[[i]],names(list_pa[[i]]))}

rm(i)

# detach dismo package 
detach(package:fuzzySim, unload=TRUE)
# detach dismo package 
detach(package:dismo,unload=TRUE)


#### Variable selection function
# I  chose this method based on a paper by Baez et al. (2019) in Ecology and Evolution which modelled whale sharks in the Atlantic ocean
# The following  function implements a variable selection procedure that takes into account several criteria:
# - correlations among variables (removing, from each pair of variables with an absolute correlation greater than 0.8, the one that is least informative regarding the species' occurrence)
# - the false discovery rate (removing variables whose relationship with the species became nonsignificant after accounting for the number of variables in the dataset, hence reducing type I errors);
# -  parsimony (performing a forward–backward stepwise selection of the remaining variables using AIC until no variable provides a relevant improvement to the model).
all_noNAs = as.data.frame(all_noNAs)
all_noNAs$p = 1
models_multGLM = multGLM(all_noNAs, # the dataset with extract values of environmental variables
                         sp.cols = 28 , # species column index number
                         var.cols = 1:27, # variable columns index numbers
                         FDR = TRUE, #  preliminary exclusion of variables based on the false discovery rate (type 1 error)
                         corSelect = TRUE, # preliminary exclusion of highly correlated variables
                         cor.thresh = 0.8, # correlation threshold of 0.8
                         step = FALSE, # perform a stepwise variable selection (based on AIC or BIC)
                         trim = FALSE) # whether to trim off non-significant variables from the models using modelTrim function
