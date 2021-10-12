# packages
library(dplyr)
library(tidyr)

# load data
sasc = read.csv("SASC_raw.csv")

# clean abundane column
vs = vs %>%
  mutate(Abundance = ifelse(is.na(Abundance),0,Abundance))

# simplify dataset
sasc_clean = sasc %>%
  group_by_at(names(sasc)) %>%
  summarise(Abundance = n())

albano_clean = as.data.frame(albano_clean)

# turn sex into a factor
table(dunlop_clean$Sex)
kznsb_clean$Sex = as.factor(kznsb_clean$Sex)


# turn unknown values to U
sasc_clean = sasc_clean %>%
  mutate(Sex = ifelse(Sex=="","U",Sex))

# add abundance column
marshall$Abundance = 1

# pivot wider
albano_clean_2 = pivot_wider(albano_clean,
            names_from = c(Sex),
            values_from = Abundance,
            values_fn = sum)

albano_clean_2 = as.data.frame(albano_clean_2)

# get total abundance
vs_clean = as.data.frame(natalie_clean)
abundance = albano_clean %>%
  group_by(Date,Life_stage, Species_scientific, Latitude, Longitude)%>%
  summarise(Abundance = sum(Abundance))

# join datasets
albano_clean_3 = full_join(abundance,albano_clean_2)

# write csv
write.csv(sasc_clean,"sasc_clean.csv")
