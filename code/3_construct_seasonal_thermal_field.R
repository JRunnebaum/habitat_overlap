##########################################
#original author = Lisha Guan
#revised by = Jocelyn Runnebaum
#last revised = 3/10/2019
###########################################


#seasonal-specific Thermal field in the GOM by grid

###########################################
#load packages
###########################################
library(dplyr)

###########################################
#load data
###########################################
load("data/grid/lobster_grid_GB_GOM_BODCdepth_SEDcat_area_swept_strata_noNA.Rda")


summary(grid_no_na)
coors <-  grid_no_na %>% 
  dplyr::select(Lat, Lon)

plot(coors$Lat ~ coors$Lon)

###########################################
#attach average seasonal temps by grid cell
###########################################
#Step 1. find neighbor FVCOM stations around each grid----
station = read.csv("data/NECOFS_FVCOM/FVCOMStations.csv")
avail.id = station$id #not real FVCOM station ID, "FVCOMid + 1"
avail.lon = station$longitude
avail.lat = station$latitude
FVCOM.Station.id = list()
neigh.length = numeric()
for (i in 1:dim(coors)[1]){
  half_length_side = 0.025
  xlon = coors[i, 2]
  ylat = coors[i, 1]
  neigh.id = which(avail.lon >= xlon - half_length_side & 
                     avail.lon <= xlon + half_length_side &
                     avail.lat >= ylat - half_length_side &
                     avail.lat <= ylat + half_length_side)
  while (length(neigh.id) == 0) {
    half_length_side = half_length_side + 0.025
    neigh.id = which(avail.lon >= xlon - half_length_side & 
                       avail.lon <= xlon + half_length_side &
                       avail.lat >= ylat - half_length_side &
                       avail.lat <= ylat + half_length_side)
  }
  neigh.id = avail.id[neigh.id]
  neigh.length[i] = half_length_side
  FVCOM.Station.id[[i]] = neigh.id
}

summary(is.na(FVCOM.Station.id))
summary(neigh.length)

aa = numeric()

for (i in 1:length(FVCOM.Station.id)){
  aa = c(aa, length(FVCOM.Station.id[[i]]))
}
which(aa == max(aa))
max(aa)

#Step 2. For each neighboring FVCOM station, find its corresponding grid id
N = length(unlist(FVCOM.Station.id))
grid.id = numeric(N)
x1 = 1
x2 = length(FVCOM.Station.id[[1]])
grid.id[x1:x2] = 1
for (i in 2:length(FVCOM.Station.id)){
  t = length(FVCOM.Station.id[[i]])  
  x1 = x2 + 1
  x2 = x1 + t - 1
  grid.id[x1:x2] = i
}
x2 #should equal to N
grid.id = as.factor(grid.id)
summary(grid.id)


#step 3. match FVCOM month id with specific time we need----
#year: 1982 - 2013
#month = 1:12

#find needed column id in FVCOM monthly hindcast
month = 1:12
ori.month = month 
ori.id = ori.month
month_id = numeric()
for (i in 1980:2013){
  month_id = c(month_id, ori.id)
  ori.id = ori.id + 12
}

#extract BT and BS data for neighboring FVCOM stations and month indices
#BT
BT = read.csv("data/NECOFS_FVCOM/BT_1978to2013.csv", header = F)
BT = as.matrix(BT)
myBT = matrix(0, nr = length(grid.id), nc = length(month_id))
colnames(myBT) = paste0("mon", month_id)
x1 = 1
for (i in 1:length(FVCOM.Station.id)){
  t = length(FVCOM.Station.id[[i]])
  x2 = x1 + t - 1
  row.id = FVCOM.Station.id[[i]]
  #col.id is month_id
  myBT[x1:x2, ] = BT[row.id, month_id]
  x1 = x2 + 1
}

#BS
BS = read.csv("data/NECOFS_FVCOM/BS_1978to2013.csv", header = F)
BS = as.matrix(BS)
myBS = matrix(0, nr = length(grid.id), nc = length(month_id))
colnames(myBS) = paste0("mon", month_id)
x1 = 1
for (i in 1:length(FVCOM.Station.id)){
  t = length(FVCOM.Station.id[[i]])
  x2 = x1 + t - 1
  row.id = FVCOM.Station.id[[i]]
  #col.id is month_id
  myBS[x1:x2, ] = BS[row.id, month_id]
  x1 = x2 + 1
}


#average monthly BT and BS data by grid
my.aggregate = function(X, by = grid.id, FUN = mean){
  aggregate(X ~ by, FUN = FUN)[,2]
}

#BT
BTbyGrid = matrix(0, nr = length(levels(grid.id)), nc = length(month_id))
BTbyGrid = apply(myBT, MARGIN = 2, FUN = my.aggregate)
dim(BTbyGrid)
plot(apply(BTbyGrid, 2, mean), type = "l", col = 1,
     xlab="Month, Jan 1980 - Dec 2013",ylab = "Mean Bottom Temperature", 
     cex=2,cex.lab=1.5,cex.axis = 1.5)
BTbyGrid <- cbind(coors, BTbyGrid)

write.csv(BTbyGrid, "data/NECOFS_FVCOM/BTbyGrid_1980to2013.csv", row.names = F)

#BS
BSbyGrid = matrix(0, nr = length(levels(grid.id)), nc = length(month_id))
BSbyGrid = apply(myBS, MARGIN = 2, FUN = my.aggregate)
dim(BSbyGrid)
plot(apply(BSbyGrid, 2, mean), type = "l", col = 1,
     xlab="Month, Jan 1980 - Dec 2013",ylab = "Mean Bottom Salinity", 
     cex=2,cex.lab=1.5,cex.axis = 1.5)
BSbyGrid <- cbind(coors, BSbyGrid)
write.csv(BSbyGrid, "data/NECOFS_FVCOM/BSbyGrid_1980to2013.csv", row.names = F)

###########################################
#Get correct months for the spring and fall
#spring (April and May) and 
#fall (October and November) 
#thermal-saline field for the GOM
###########################################

BSbyGrid = read.csv("data/NECOFS_FVCOM/BSbyGrid_1980to2013.csv")
BTbyGrid = read.csv("data/NECOFS_FVCOM/BTbyGrid_1980to2013.csv")


#spring
month = 4:6
ori.id = month
month_id = numeric()
for (i in 1980:2013){
  month_id = c(month_id, ori.id)
  ori.id = ori.id + 12
}

Thermo.field.sp = (BTbyGrid[,month_id[seq(1, length(month_id),3)]] +
                     BTbyGrid[,month_id[seq(3, length(month_id),3)]] ) /2 
Saline.field.sp = (BSbyGrid[,month_id[seq(1, length(month_id),3)]] +
                     BSbyGrid[,month_id[seq(3, length(month_id),3)]] ) /2

colnames(Thermo.field.sp) = as.character(1980:2013)
colnames(Saline.field.sp) = as.character(1980:2013)
sp.year.avg <- cbind(coors,Thermo.field.sp)
head(sp.year.avg)
bs.sp.year.avg <- cbind(coors,Saline.field.sp)
head(bs.sp.year.avg)

spring_temp <- apply(Thermo.field.sp, 2, mean) %>% 
  as.data.frame() %>% 
  gather(year, temp) %>% 
  mutate(year = 1980:2013)

plot(temp ~ year, data = spring_temp, type = "l")
plot(apply(Thermo.field.sp, 2, mean), type = "l", col = 1,
     xlab="Year",ylab = "Mean Bottom Temperature", 
     cex=2,cex.lab=1.5,cex.axis = 1.5)


write.csv(sp.year.avg, "data/NECOFS_FVCOM/BT_GOM_GB_spring_grid_3mon.csv", row.names = F)
write.csv(bs.sp.year.avg, "data/NECOFS_FVCOM/BS_GOM_GB_spring_grid_3mon.csv", row.names = F)

head(grid_no_na)
head(Thermo.field.sp)

grid_with_springBT = cbind(grid_no_na, Thermo.field.sp)
head(grid_with_springBT)
write.csv(grid_with_springBT, file="data/NECOFS_FVCOM/GOM_GB_grid_BODCdepth_areaswept_ID_spBT_forHSI_3mon.csv")

head(Saline.field.sp)
grid_with_springBS = cbind(grid_no_na, Saline.field.sp)
head(grid_with_springBS)
write.csv(grid_with_springBS, file="data/NECOFS_FVCOM/GOM_GB_grid_BODCdepth_areaswept_ID_spBS_forHSI_3mon.csv")

#fall
month = 9:11
ori.id = month
month_id = numeric()
for (i in 1980:2013){
  month_id = c(month_id, ori.id)
  ori.id = ori.id + 12
}
Thermo.field.fl = (BTbyGrid[,month_id[seq(1, length(month_id),3)]] +
                     BTbyGrid[,month_id[seq(3, length(month_id),3)]] ) / 2     
Saline.field.fl = (BSbyGrid[,month_id[seq(1, length(month_id),3)]] +
                     BSbyGrid[,month_id[seq(3, length(month_id),3)]] ) / 2     
colnames(Thermo.field.fl) = as.character(1980:2013)
colnames(Saline.field.fl) = as.character(1980:2013)
write.csv(Thermo.field.fl, "data/NECOFS_FVCOM/BT_GOM_GB_fall_grid.csv", row.names = F)
write.csv(Saline.field.fl, "data/NECOFS_FVCOM/BS_GOM_GB_fall_grid.csv", row.names = F)

head(Saline.field.fl)


grid_with_fallBT = cbind(grid_no_na, Thermo.field.fl)
write.csv(grid_with_fallBT,file="data/NECOFS_FVCOM/GOM_GB_grid_BODCdepth_areaswept_ID_flBT_forHSI.csv")

grid_with_fallBS = cbind(grid_no_na, Saline.field.fl)
write.csv(grid_with_fallBS, file="data/NECOFS_FVCOM/GOM_GB_grid_BODCdepth_areaswept_ID_flBS_forHSI.csv")


#yearly average
month = 1:12
ori.id = month
month_id = numeric()
for (i in 1980:2013){
  month_id = c(month_id, ori.id)
  ori.id = ori.id + 12
}
Thermo.field = (BTbyGrid[,month_id[seq(1, length(month_id),12)]] +
                     BTbyGrid[,month_id[seq(2, length(month_id),12)]] ) / 2     
Saline.field = (BSbyGrid[,month_id[seq(1, length(month_id),12)]] +
                     BSbyGrid[,month_id[seq(2, length(month_id),12)]] ) / 2     
colnames(Thermo.field) = as.character(1980:2013)
colnames(Saline.field) = as.character(1980:2013)
head(Thermo.field)



#show thermal/Saline field
library(RColorBrewer)
mycolors = brewer.pal(9, "YlOrRd")
display.brewer.pal(9, "YlOrRd")
i = sample(1:32, 1)
col.id = findInterval(Saline.field.sp[,i], quantile(Saline.field.sp[,i], seq(0,1,0.125)))
# plot(Lat~Lon, coors, col = mycolors[col.id], pch = 15)

#scale spring and fall thermo-saline field
scaled_thermo_field_sp = apply(Thermo.field.sp, MARGIN = 2, FUN = scale)
scaled_saline_field_sp = apply(Saline.field.sp, MARGIN = 2, FUN = scale)
scaled_thermo_field_fl = apply(Thermo.field.fl, MARGIN = 2, FUN = scale)
scaled_saline_field_fl = apply(Saline.field.fl, MARGIN = 2, FUN = scale)
thermo_saline_field_sp = array(NA, dim = c(dim(Thermo.field.sp), 2))
thermo_saline_field_fl = thermo_saline_field_sp
thermo_saline_field_sp[ , , 1] = scaled_thermo_field_sp
thermo_saline_field_sp[ , , 2] = scaled_saline_field_sp
thermo_saline_field_fl[ , , 1] = scaled_thermo_field_fl
thermo_saline_field_fl[ , , 2] = scaled_saline_field_fl
head(thermo_saline_field_sp)

save(thermo_saline_field_sp, file = "dynamic_covariates_sp.Rda")
save(thermo_saline_field_fl, file = "dynamic_covariates_fl.Rda")

#show thermal/Saline field
mycolors.bt = brewer.pal(9, "YlOrRd")
mycolors.bs = brewer.pal(9, "PuBuGn")
display.brewer.pal(9, "YlOrRd")
i = sample(1:32, 1)
col.id.bt = findInterval(thermo_saline_field_sp[,i,1], quantile(thermo_saline_field_sp[,i,1], seq(0,1,0.125)))
col.id.bs = findInterval(thermo_saline_field_sp[,i,2], quantile(thermo_saline_field_sp[,i,2], seq(0,1,0.125)))
plot(Lat~Lon, coors, col = mycolors.bt[col.id.bt], pch = 15)
plot(Lat~Lon, coors, col = mycolors.bs[col.id.bs], pch = 15)


