# solve problem
list_solutions = list()
for(i in 1:length(list_problems)){
  list_solutions[[i]] = solve(list_problems[[i]])
}

# load mpas for solution plotting
mpas = shapefile(list.files(pattern ="SAMPAZ_OR_2020_Q3.shp" ,recursive = TRUE, full.names = TRUE))
mpas = gSimplify(mpas,tol = 0.01)

# target group order
target_group = rep(1:3,3)
# season order
season_order = rep(c("aseasonal","summer","winter"),each=3)
# scenario
scenario = 1

# plot solutions
p = 0
for(i in 1:length(list_solutions)){
# plotted with the mpas
png(file=paste0("Planning/Outputs/solutions/","p",p+i,"_scenario",scenario,"_target",target_group[i],"_",season_order[i],".png"), width=3000, height=3000, res=300)
plot(list_solutions[[i]], col = c("grey90", "darkgreen"), main = paste("Scenario:",scenario," Season:",season_order[i]," Target:",target_group[i]), sub = paste("penalty:", penalty,"problem:",p+i))
plot(mpas,add = TRUE)
dev.off()
}

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
p = 0
for(i in 1:length(list_ferrierscores)){
  # plotted with the mpas
  png(file=paste0("Planning/Outputs/solutions/","p",p+i,"_scenario",scenario,"_target",target_group[i],"_",season_order[i],"ferrierscore.png"), width=3000, height=3000, res=300)
  plot(list_ferrierscores[[i]], main = paste("Ferrier score Scenario:",scenario," Season:",season_order[i]," Target:",target_group[i]), sub = paste("penalty:", penalty,"problem:",p+i))
  plot(mpas,add = TRUE)
  dev.off()
}

