 
# Evaluate solution performance
performances = data.frame()

#  number of planning units selected within a solution.
for(i in 1:length(list_problems)){
  pus = eval_n_summary(list_problems[[i]], list_solutions[[i]])
  performances[i,1] = pus[1,2]
  performances$scenario = scenario
  performances$problem[i] = i}

# calculate % of EEZ represented
performances$prop_eez = (performances$cost/42053)*100

# write csv to performance folder
write.csv(performances,paste0("Outputs/performances/scenario",scenario,"_eez_proportion.csv"))

# how well features are represented by a solution and if targets are met
a = eval_target_coverage_summary(list_p[[1]], list_s[[1]])
a = a[!which(a$met == TRUE),]
b = eval_target_coverage_summary(list_p[[2]], list_s[[2]])
b = b[!which(b$met == TRUE),]
c = eval_target_coverage_summary(list_p[[3]], list_s[[3]])
c = c[!which(c$met == TRUE),]
if(nrow(a)>0){
  write.csv(a,paste("Outputs/performances/scenario",scenario,"subscenarion",1,".csv"))
}
if(nrow(b)>0){
  write.csv(b,paste("Outputs/performances/scenario",scenario,"subscenarion",2,".csv"))
}
if(nrow(c)>0){
  write.csv(c,paste("Outputs/performances/scenario",scenario,"subscenarion",3,".csv"))
}


