
# fishing pressure planning units, every cell has fishing pressure value between 0 and 100
# for now we only keep offshore trawling and pelagic longline
costs_reduced = costs[[c(7,8)]]
# turn all NAs to 0
values(costs_reduced)[is.na(values(costs_reduced))] = 0
# mask with no cost planning unit to only keep eez
costs_reduced = mask(costs_reduced,pu)
plot(costs_reduced)
rm(costs)

# PLANNING UNITS
# no pressure planning unit, sum of trawling and longlining pressure
pu_nopressure  = mask(calc(costs_reduced,sum,na.rm = TRUE),pu)
plot(pu_nopressure)

# trawling planning unit (remove trawling so just longlining)
pu_trawl = costs_reduced[[2]]
plot(pu_trawl)

# longlining planning unit (remove longlining so just trawling)
pu_longline = costs_reduced[[1]]
plot(pu_longline)

# longlining AND trawling planning unit (all planning units have value of 1?)
pu_allpressures = pu
plot(pu_allpressures)

# now create rasterstack
pu_final = stack(pu_nopressure,pu_trawl,pu_longline,pu_allpressures)
names(pu_final) = c("no.pressure","trawl","longline","all.pressures")
plot(pu_final)
names(pu_final)
scales::rescale(pu_final,2,100)

# FEATURES
# continuous distribution of 41 species distribution maps
feature_stack_aseasonal
plot(feature_stack_aseasonal)

# species affected by trawling
# any species not affected by trawling is just a layer with a value of 0
layers_trawling = stack(layers_trawling)

# species affected by long lining
# any species not affected by long lining is just a layer with a value of 0
layers_longline = stack(layers_longline)

# species affected by both
layers_both = stack(layers_both)

# species affected by none
layers_none = stack(layers_none)

# TARGETS
# create a matrix with the targets
# here each column corresponds to a different zone,
# each row corresponds to a different feature, and
# each cell value corresponds to the target
# create targets
targets <- tibble::tibble(feature = as.character(names(layers_both)),
                     zone = list(names(pu_final_rescale))[rep(1, 41)],
                     target = unname(0.41 * cellStats(feature_stack_aseasonal, "sum")),
                     type = rep("absolute", 41))

# PROBLEM
# create problem
p <- problem(pu_final_rescale, zones(feature_stack_aseasonal,layers_longline,layers_trawling,layers_none,
                             feature_names = names(layers_both),
                             zone_names = names(pu_final))) %>%
  add_min_set_objective() %>%
  add_binary_decisions() %>%
  add_manual_targets(targets)

#solve
s <- solve(p)

# calculate feature representation
r <- eval_feature_representation_summary(p, s)
print(r)

# plot solution
plot(category_layer(s), main = "solution", axes = FALSE, box = FALSE)




# plot cost data
plot(costs_temp, main = c("zone 1", "zone 2"), axes = FALSE, box = FALSE)

# rasterstack representing the conservation features
plot(feature_stack_aseasonal)

# find out which species are affected by offshore trawling
offshoretrawl = threats_v2 %>%
  filter(fisheries == "offshore_trawl")
offshoretrawl = toupper(offshoretrawl$species_scientific)
pelagiclongline = threats_v2 %>%
  filter(fisheries == "pelagic_longline")
pelagiclongline = toupper(pelagiclongline$species_scientific)
both = unique(c(offshoretrawl,pelagiclongline))

# create two raster stacks of features depending on which threats they are affected by

# rasterstack layers of species affected by trawling
names = featurenames[featurenames$species_scientific %in% offshoretrawl,]$featurename
layers_trawling = names(feature_stack_aseasonal) %in% names
layers_trawling2 = which(layers_trawling == TRUE)
layers_trawling3 = feature_stack_aseasonal[[layers_trawling2]]
layers_trawling2 = which(layers_trawling == FALSE)
layers_trawling4 = feature_stack_aseasonal[[layers_trawling2]]
names4 = names(layers_trawling4) # save names as these dissapear when turning values to 0
values(layers_trawling4) = 0
names(layers_trawling4) = names4
rm(names4)
layers_trawling = stack(layers_trawling3,layers_trawling4)
layers_trawling = mask(layers_trawling,pu)
rm(names,layers_trawling2,layers_trawling3,layers_trawling4)

# rasterstack layers of species affected by longlining
names = featurenames[featurenames$species_scientific %in% pelagiclongline,]$featurename
layers_longline = names(feature_stack_aseasonal) %in% names
layers_longline2 = which(layers_longline == TRUE)
layers_longline3 = feature_stack_aseasonal[[layers_longline2]]
layers_longline2 = which(layers_longline == FALSE)
layers_longline4 = feature_stack_aseasonal[[layers_longline2]]
names4 = names(layers_longline4) # save names as these dissapear when turning values to 0
values(layers_longline4) = 0
names(layers_longline4) = names4
layers_longline = stack(layers_longline3,layers_longline4)
layers_longline = mask(layers_longline,pu)
rm(names,layers_longline2,layers_longline3,layers_longline4)

# rasterstack layers of species affected by both
names = featurenames[featurenames$species_scientific %in% both,]$featurename
layers_both = names(feature_stack_aseasonal) %in% names
layers_both2 = which(layers_both == TRUE)
layers_both3 = feature_stack_aseasonal[[layers_both2]]
layers_both2 = which(layers_both == FALSE)
layers_both4 = feature_stack_aseasonal[[layers_both2]]
names4 = names(layers_both4) 
values(layers_both4) = 0
names(layers_both4) = names4
layers_both = stack(layers_both3,layers_both4)
layers_both = mask(layers_both,pu)
rm(names,layers_both2,layers_both3,layers_both4,names4)

# rasterstack layers of species affected by none
names = featurenames[featurenames$species_scientific %in% both,]$featurename
layers_none = names(feature_stack_aseasonal) %in% names
layers_none2 = which(layers_none == FALSE)
layers_none3 = feature_stack_aseasonal[[layers_none2]]
layers_none2 = which(layers_none == TRUE)
layers_none4 = feature_stack_aseasonal[[layers_none2]]
names4 = names(layers_none4) 
values(layers_none4) = 0
names(layers_none4) = names4
layers_none = stack(layers_none3,layers_none4)
layers_none = mask(layers_none,pu)
rm(names,layers_none2,layers_none3,layers_none4)


# rescaling function
rescale <- function(x, x.min = NULL, x.max = NULL, new.min = 0, new.max = 1) {
  if(is.null(x.min)) x.min = min(x)
  if(is.null(x.max)) x.max = max(x)
  new.min + (x - x.min) * ((new.max - new.min) / (x.max - x.min))
}

cellStats(pu_final, stat = "max")
pu_final_rescale = rescale(pu_final,x.min = 1, x.max = 200, new.min = 2, new.max = 100)
plot(pu_final_rescale)

cellStats

