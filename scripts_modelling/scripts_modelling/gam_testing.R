library(mgcv)
pts_env$substrate_simplified = as.factor(pts_env$substrate_simplified)
sdm_gam_full <- gam(pa ~ depth +
                      DissolvedO2_max + DissolvedO2_mean + DissolvedO2_min +
                      disttoestuary_all_km +
                      Salinity_Lt_max + Salinity_Lt_min + Salinity_max +Salinity_mean +Salinity_min+Salinity_range+
                      sst_average+sst_Lt_max+sst_Lt_min+sst_max+sst_min+sst_range+
                      substrate_simplified,
                    family = binomial(link = "logit"), data=pts_env,method = "REML")
summary(sdm_gam_full)
