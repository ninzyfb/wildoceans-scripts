# solve each problem in the list
list_solutions = list()
for(i in 1:length(list_problems)){
  list_solutions[[i]] = solve(list_problems[[i]])
}
rm(i)

# load mpas for solution plotting
mpas = shapefile(list.files(pattern ="SAMPAZ_OR_2020_Q3.shp" ,recursive = TRUE, full.names = TRUE))
mpas = gSimplify(mpas,tol = 0.01)
# three marine regions as defined by Ebert et al.
regions = shapefile(list.files(pattern = "ebert_regions.shp", recursive = TRUE,full.names = TRUE))
# turn region names to upper case
regions$Region = toupper(regions$Region)
# subset by east south and west
range = c("WEST","SOUTH","EAST")


# target group order
# this doesn't refer to the species specific targets, this refers to the target group (a,b,c)
target_group = rep(c("low","medium","high"),3) # repeat each three times 
# feature stack order
season_order = rep(c("aseasonal","summer","winter"),each=3) # repeat each three times
# scenario
scenario = 1

# plot solutions for whole EEZ
count = 0 # count for solution number (problem number)
for(i in 1:length(list_solutions)){
# plotted with the mpas
png(file=paste0("Planning/Outputs/solutions/","p",count+i,"_scenario",scenario,"_targets",target_group[i],"_",season_order[i],".png"), width=3000, height=3000, res=300)
plot(list_solutions[[i]], col = c("grey90", "darkgreen"), main = paste("Scenario:",scenario,"\nSeason:",season_order[i],"| Targets:",target_group[i],"\nPenalty:", penalty), sub = paste("Problem number:",count+i),legend = FALSE)
plot(mpas,add = TRUE)
dev.off()
}

# plot solutions for each Ebert region
count = 0 # count for solution number (problem number)
for(i in 1:length(list_solutions)){
  for(j in 1:length(range)){
    # subset range
    subset = regions[regions$Region%in%range[j],]
    png(file=paste0("Planning/Outputs/solutions/","p",count+i,"_scenario",scenario,"_targets",target_group[i],"_",season_order[i],".png"), width=3000, height=3000, res=300)
    plot(crop(list_solutions[[i]],subset), col = c("grey90", "darkgreen"), main = paste("Scenario:",scenario,"\nSeason:",season_order[i],"| Targets:",target_group[i],"\nPenalty:", penalty), sub = paste("Problem number:",count+i),legend = FALSE)
    plot(crop(mpas,subset),add = TRUE)
    dev.off()
}}



# Ferrier importance scores using replacement cost scores
# quantifies the importance of planning units for meeting feature targets
# can only be applied to problems with a minimum set objective and a single zone
# provides a score for each feature within each planning unit
# provides insight into why certain planning units are more important than other planning units. 
?eval_ferrier_importance
list_ferrierscores = list()
for(i in 1:length(list_solutions)){
  list_ferrierscores[[i]] = eval_ferrier_importance(list_problems[[i]], list_solutions[[i]])[["total"]]
}

# plot solutions
count = 0 
for(i in 1:length(list_ferrierscores)){
  # plotted with the mpas
  png(file=paste0("Planning/Outputs/solutions/","p",count+i,"_scenario",scenario,"_targets",target_group[i],"_",season_order[i],"FS.png"), width=3000, height=3000, res=300)
  plot(list_ferrierscores[[i]], main = paste("Ferrier score | Scenario:",scenario,"\nSeason:",season_order[i],"| Targets:",target_group[i],"\nPenalty:", penalty), sub = paste("Problem number:",count+i))
  plot(mpas,add = TRUE)
  dev.off()
}

