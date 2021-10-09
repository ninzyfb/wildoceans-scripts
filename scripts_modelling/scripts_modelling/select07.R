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
