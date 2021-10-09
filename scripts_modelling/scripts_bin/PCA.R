# rank the variables according to their loadings at each PCA axis
# the variable with the highest loading on PCA1 gets the rank 1
# the variables with highest loading on PCA2 gets the rank 2 and so on.
# If a variable appears two or more times as highest loading we ignore this
# Following the select07() procedure from Dormann et al. (2013),
# calculate Spearman's rank correlation among all variable pairs
# remove the variable with the lower rank if |p| < 0.7,
# thus leaving only the most informative in terms of environmental variation, uncorrelated environmental predictors.

library(raster)
library(tidyverse)
library(ade4)

df = as.data.frame(values(stack))
df = df %>%
  drop_na()

pca_1000 = dudi.pca(df, center = T, scale = T, scannf = F, nf = ncol(df))


# Quantify loadings and get most important vars per PC
get_var_ranking = function(pca){
  x = pca$c1
  variables = vector(mode = "character", length = ncol(x))
  for(i in 1:ncol(x)){
    variables[i] = rownames(x)[which.max(abs(x[,i]))]
    x = subset(x, rownames(x) != variables[i])
  }
  return(data.frame(var_name = variables, rank = 1:length(variables), stringsAsFactors = F))
}

rankings = list("1000" = get_var_ranking(pca_1000))

# Reduce to uncorrelated variables using select07 function from Dorman et al 2013
select07 = function(ranking, X, threshold = 0.7, method = "spearman"){
  cm = cor(X, method = method)
  
  pairs = which(abs(cm) >= threshold, arr.ind = T) # identifies correlated variable pairs
  pairs = pairs[pairs[,1] != pairs[,2],] # removes diagonal entries
  
  exclude = NULL
  for(i in seq_len(length(ranking))){
    if(ranking[i] %in% row.names(pairs) & !(ranking[i] %in% exclude)){
      cv = cm[setdiff(row.names(cm), exclude), ranking[i]]
      cv = cv[setdiff(names(cv), ranking[1:i])]
      exclude = c(exclude, names(which((abs(cv) >= threshold)))) 
    }
  }
  return(ranking[!(ranking %in% exclude), drop=F])
}

# run function
var_select = list("125" = select07(rankings$`1000`$var_name, df))
option1 = unlist(var_select)
rm(var_select,rankings,pca_1000,get_var_ranking,select07)

