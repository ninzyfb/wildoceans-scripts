
# plot single solution
png(file=paste0("Planning/Outputs/solutions/national/",scenario,"_",type,"_","targets",targettype,"_penaltytype",penalty,"_lockedin",locked_in,".png"),width=3000, height=3000, res=300)
plot(solution_single, col = c("grey90", "darkgreen"),
     # add scenario parameters as title
     main = paste(scenario,"scenario","\nTargets:",targettype),
     # add problem number and % of EEZ taken to subtitle
     sub = paste("Percentage of EEZ = ",round(performances$prop_eez,0),"%"),
     legend = FALSE)
plot(mpas,add = TRUE)
dev.off()

# plot single solution per ebert range
for(j in 1:length(range)){
  # subset range
  subset = regions[regions$Region%in%range[j],]
  png(file=paste0("Planning/Outputs/solutions/regional/",scenario,"_",type,"_","targets",targettype,"_penaltytype",penalty,"_lockedin",locked_in,"_",range[j],".png"), width=3000, height=3000, res=300)
  plot(crop(solution_single,subset), col = c("grey90", "darkgreen"), main = paste(scenario,"scenario","\nTargets:",targettype),legend = FALSE)
  plot(crop(mpas,subset),add = TRUE)
  dev.off()
}

# ferrier score for single problem
ferrierscore_single = eval_ferrier_importance(problem_single, solution_single)[["total"]]

# plot solutions
  # plotted with the mpas
png(file=paste0("Planning/Outputs/solutions/ferrierscores/",scenario,"_",type,"_","targets",targettype,"_penaltytype",penalty,"_lockedin",locked_in,"_FS.png"), width=3000, height=3000, res=300)
plot(ferrierscore_single, main = paste("Ferrier score",scenario,"scenario","\nTargets:",targettype))
plot(mpas,add = TRUE)
dev.off()


# solve each problem in the list
list_solutions = list()
for(i in 1:length(list_problems)){
  list_solutions[[i]] = solve(list_problems[[i]])
}
rm(i)



# target group order
# this doesn't refer to the species specific targets, this refers to the target group (a,b,c)
target_group = rep(c("low","medium","high"),3) # repeat each three times 
# feature stack order
season_order = rep(c("aseasonal","summer","winter"),each=3) # repeat each three times


for(i in 1:length(list_solutions)){
# plotted with the mpas
png(file=paste0("Planning/Outputs/solutions/national/","p",count+i,"_scenario",scenario,"_targets",target_group[i],"_",season_order[i],".png"),width=3000, height=3000, res=300)
plot(list_solutions[[i]], col = c("grey90", "darkgreen"),
     # add scenario parameters as title
     main = paste("Scenario:",scenario,"\nSeason:",season_order[i],"| Targets:",target_group[i],"\nPenalty:", penalty),
     # add problem number and % of EEZ taken to subtitle
     sub = paste("Problem number:",count+i,"\nPercentage of EEZ = ",round(performances$prop_eez[i],0),"%"),
     legend = FALSE)
plot(mpas,add = TRUE)
dev.off()
}

# plot solutions for each Ebert region
for(i in 1:length(list_solutions)){
  for(j in 1:length(range)){
    # subset range
    subset = regions[regions$Region%in%range[j],]
    png(file=paste0("Planning/Outputs/solutions/regional/","p",count+i,"_scenario",scenario,"_targets",target_group[i],"_",season_order[i],"_",range[j],".png"), width=3000, height=3000, res=300)
    plot(crop(list_solutions[[i]],subset), col = c("grey90", "darkgreen"), main = paste("Scenario:",scenario,"\nSeason:",season_order[i],"| Targets:",target_group[i],"\nPenalty:", penalty), sub = paste("Problem number:",count+i),legend = FALSE)
    plot(crop(mpas,subset),add = TRUE)
    dev.off()
}}

# Ferrier importance scores using replacement cost scores
# quantifies the importance of planning units for meeting feature targets
# can only be applied to problems with a minimum set objective and a single zone
# provides a score for each feature within each planning unit
# provides insight into why certain planning units are more important than other planning units. 
list_ferrierscores = list()
for(i in 1:length(list_solutions)){
  list_ferrierscores[[i]] = eval_ferrier_importance(list_problems[[i]], list_solutions[[i]])[["total"]]
}

# plot solutions
for(i in 1:length(list_ferrierscores)){
  # plotted with the mpas
  png(file=paste0("Planning/Outputs/solutions/ferrierscores/","p",count+i,"_scenario",scenario,"_targets",target_group[i],"_",season_order[i],"FS.png"), width=3000, height=3000, res=300)
  plot(list_ferrierscores[[i]], main = paste("Ferrier score | Scenario:",scenario,"\nSeason:",season_order[i],"| Targets:",target_group[i],"\nPenalty:", penalty), sub = paste("Problem number:",count+i))
  plot(mpas,add = TRUE)
  dev.off()
}
