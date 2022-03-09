library(tidyr)
variables = read.csv(list.files(pattern = "selectedvariables_all.csv", recursive = TRUE, full.names = TRUE))
variables = variables[-18,]
variableimportance = list.files(pattern = "variableimportance.csv", recursive = TRUE, full.names = TRUE)
all = list()
for(i in 1:length(variableimportance)){
  temp = read.csv(variableimportance[i])
  if(nrow(temp)>10){
    if(nrow(temp)>17){temp = temp[-18,]}
    temp = cbind(temp,variables)
    temp = temp %>%
      pivot_longer(!variables,names_to = "test",values_to = "value")%>%
      group_by(variables)%>%
      summarise(sum = mean(value),
                std = sd(value))%>%
      arrange(desc(sum))
    all[[i]]= temp
  }}

all = do.call("rbind",all)
all$variables = as.factor(all$variables)
boxplot(all$sum~all$variables)
all_summed = all %>%
  group_by(variables)%>%
  summarise(avg = mean(sum))%>%
  arrange(desc(avg))
# drop to only keep top 9 variables
removedvariables = all_summed[c(10:17),]$variables
idx = which(names(stack_subset) %in% removedvariables)
stack_subset = dropLayer(stack_subset,idx)