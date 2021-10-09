library(raster)
library(stringr)
library(sf)

files = list.files(pattern ="ensemblemean.tif" ,recursive = TRUE)
stack = stack(files)
# only keep areas coming up as 70% probability of suitable habitat
for(i in 1:nlayers(stack)){
  stack[[i]][values(stack[[i]]) < 700] = NA}
# average the results
result = mean(stack, na.rm = TRUE)
# plot
plot(result)

writeRaster(result,"result.tif")
writeRaster(result2,"result2.tif")


# finding which species per MPA
sp3b = st_read("mpas/sp3b.shp")
list = list()
for(i in 1:nlayers(stack)){
temp = crop(stack[[i]],extent(sp3b))
sum = sum(values(temp), na.rm = TRUE)
list[[i]] = sum}

sp3b_sp = files[unlist(list) != 0]





library(sf)
library(dplyr)

mpa =st_read(list.files(pattern ="SAMPAZ_OR_2020_Q3.shp" ,recursive = TRUE))

isi = mpa[str_detect(mpa$CUR_NME,pattern = "iSimangaliso"),]
isi_2  = isi %>%
  filter(!(CUR_ZON_NM %in% c("iSimangaliso Offshore Controlled Pelagic Linefish Zone North","iSimangaliso Offshore Controlled Pelagic Linefish Zone South","iSimangaliso Offshore Restricted Zone North","iSimangaliso Offshore Restricted Zone South","iSimangaliso Offshore Wilderness Zone"))) %>%
  filter(CUR_ZON_TY != "Wilderness")

sp1a = isi_2 %>%
  group_by(CUR_NME) %>%
  summarise()
sp1a = st_zm(sp1a,drop=TRUE)
st_write(sp1a,"sp1a.shp")
st_area(sp1a)



uth = mpa[str_detect(mpa$CUR_NME,pattern = "uThukela"),]
uth_2  = uth %>%
  filter(!(CUR_ZON_NM %in% c("uThukela Offshore Controlled Zone North","uThukela Offshore Controlled Zone South","uThukela Offshore Restricted Zone","uThukela Offshore Controlled Commercial Zone","uThukela Offshore Controlled-Pelagic Linefish Zone"))) %>%
  filter(CUR_ZON_TY != "Wilderness")

uth_2 = uth_2 %>%
  group_by(CUR_NME) %>%
  summarise()
sp1b = uth_2
sp1b = st_zm(sp1b,drop=TRUE)
st_write(sp1b,"sp1b.shp",append = TRUE)
st_area(sp1b)
plot(sp1b)


ali = mpa[str_detect(mpa$CUR_ZON_NM,pattern = "Aliwal Shoal Offshore Controlled-Pelagic Linefish Zone"),]
ali = ali %>%
  group_by(CUR_NME) %>%
  summarise()
sp1c = ali
sp1c = st_zm(sp1c,drop=TRUE)
plot(sp1c)
st_write(sp1c,"sp1c.shp",append = TRUE)
st_area(sp1c)
plot(sp1b)

prot = mpa[str_detect(mpa$CUR_ZON_NM,pattern = "Protea Banks Controlled-Pelagic Linefish Zone"),]
prot = prot %>%
  group_by(CUR_NME) %>%
  summarise()
sp1d = prot
sp1d = st_zm(sp1d,drop=TRUE)
plot(sp1d)
st_write(sp1d,"sp1d.shp",append = TRUE)
st_area(sp1d)
library(plyr)

a = as.data.frame(sp1a_sp)
b = as.data.frame(sp1b_sp)
c = as.data.frame(sp1c_sp)
d = as.data.frame(sp1d_sp)
e = as.data.frame(sp2a_sp)
f = as.data.frame(sp2b_sp)
g = as.data.frame(sp3a_sp)
h = as.data.frame(sp3b_sp)

look = rbind.fill(a,b,c,d,e,g,h)
write.csv(look,"look.csv")
