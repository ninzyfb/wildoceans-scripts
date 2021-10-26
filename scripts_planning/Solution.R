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
for(i in 1:length(list_solutions)){
# one without the mpas
png(file=paste0("Planning/Outputs/solutions/","p",p+i,"_scenario",scenario,"_","target",target_group[i],"_",season_order[i],".png"), width=3000, height=3000, res=300)
plot(list_solutions[[i]], col = c("grey90", "darkgreen"), main = paste("Scenario:",scenario,"Season:",season_order[i],"Target:",target_group[i]), sub = paste("penalty:", penalty,"problem:",p+i))
dev.off()
# one with the mpas
png(file=paste0("Planning/Outputs/solutions/","p",p+i,"_scenario",scenario,"_","target",target_group[i],"_",season_order[i],"_withmpas.png"), width=3000, height=3000, res=300)
plot(list_solutions[[i]], col = c("grey90", "darkgreen"), main = paste("Scenario:",scenario,"Season:",season_order[i],"Target:",target_group[i]), sub = paste("penalty:", penalty,"problem:",p+i))
plot(mpas,add = TRUE)
dev.off()
}

# calculate importance scores using replacement cost scores

test <- eval_replacement_importance(list_problems[[1]], list_solutions[[1]])
plot(test)



