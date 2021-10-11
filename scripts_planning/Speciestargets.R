
# load in spreadsheet with species and their targets
targets = read_xlsx(list.files(pattern = "species_targets.xlsx",recursive = TRUE), sheet = 1)

# only keep species names and targets
targets = targets %>%
  dplyr::select(species_scientific,Target)

# turn colnames to upper case
colnames(targets) = toupper(colnames(targets))

# add these targets to master sheet
master_sheet = left_join(master_sheet,targets)

# join these targets to featurenames dataframe
colnames(featurenames) = toupper(colnames(featurenames))
featurenames = left_join(featurenames,targets)
rm(targets) # remove

# add number targets
featurenames = featurenames %>%
  mutate(targetsa = ifelse(TARGET == "low",0.2,
                           ifelse(TARGET == "medium",0.3,
                                  ifelse(TARGET == "high",0.4,NA))))

featurenames$targetsb = featurenames$targetsa + 0.1
featurenames$targetsc = featurenames$targetsb + 0.1

