
library(dplyr)
###################################
#match density estimates to grid
#we do not use the estimates from Dens_xt because they are in log scale
# “Extrapolation_List” and "Spatial_List" are in the same grid order
###################################

grid_abund = Report$D_xcy[Spatial_List$NN_Extrap$nn.idx, , ] 
grid_abund <- as.data.frame(grid_abund)
names(grid_abund)[1:36] <- paste("abun", 1980:2015, sep = "")

grid_inf = Extrapolation_List$Data_Extrap %>% 
  select("Lat", "Lon")

grid_abundance = cbind(grid_abund, grid_inf) 


grid_abundance <- grid_abundance %>% 
  dplyr::rename(lat = Lat,
                lon = Lon) %>% 
  mutate(mean_abun = rowMeans(select(., starts_with("abun"))))

head(grid_abundance)
###################################
#load spring data
###################################

#spring bt and bs
bt_sp <- read.csv("data/NECOFS_FVCOM/GOM_GB_grid_BODCdepth_areaswept_ID_spBT_forHSI_3mon.csv") 
bt_sp <- bt_sp %>%   
  dplyr::rename(lat = Lat,
         lon = Lon,
         id = X) %>% 
  select(-id) 
bt_sp <- bt_sp %>% 
  select_all(~gsub("X", "bt", colnames(bt_sp), fixed = TRUE)) %>% 
  mutate(mean_bt = rowMeans(select(., starts_with("bt"))))

bs_sp <- read.csv("data/NECOFS_FVCOM/GOM_GB_grid_BODCdepth_areaswept_ID_spBS_forHSI_3mon.csv")

bs_sp <- bs_sp %>% 
  dplyr::rename(lat = Lat,
         lon = Lon,
         id = X)
bs_sp <- bs_sp %>% 
  select_all(~gsub("X", "bs", colnames(bs_sp), fixed = TRUE)) %>% 
  select(-id)%>% 
  mutate(mean_bs = rowMeans(select(., starts_with("bs"))))


spring_env <-  
  left_join(grid_abundance, bt_sp) %>% 
  left_join(bs_sp) 
 
summary(spring_env)

# write.csv(spring_env, file="data/HSI_data/lob_spring_3mon_grid_abndance.csv")
# write.csv(spring_env, file="data/HSI_data/cusk_spring_3mon_BT_BS_grid_abundance.csv")


###################################
#load fall data
###################################

#fall bt and bs
bt_fl <- read.csv("data/NECOFS_FVCOM/GOM_GB_grid_BODCdepth_areaswept_ID_flBT_forHSI.csv") 
bt_fl <- bt_fl %>%   
  rename(lat = Lat,
         lon = Lon,
         id = X) %>% 
  select(-id)

bt_fl <- bt_fl %>% 
  select_all(~gsub("X", "bt", colnames(bt_fl), fixed = TRUE)) %>% 
  mutate(mean_bt = rowMeans(select(., starts_with("bt"))))

bs_fl <- read.csv("data/NECOFS_FVCOM/GOM_GB_grid_BODCdepth_areaswept_ID_flBS_forHSI.csv")
bs_fl <- bs_fl %>% 
  rename(lat = Lat,
         lon = Lon,
         id = X) %>% 
  select(-id) 

bs_fl <- bs_fl %>% 
  select_all(~gsub("X", "bs", colnames(bs_fl), fixed = TRUE)) %>% 
  mutate(mean_bs = rowMeans(select(., starts_with("bs"))))


fall_env <-  
  left_join(grid_abundance, bt_fl) %>% 
  left_join(bs_fl) 
summary(fall_env)


# write.csv(fall_env, file="data/HSI_data/lob_fall_3mon_grid_abndance.csv")
write.csv(fall_env, file="data/HSI_data/cusk_fall_3mon_BT_BS_grid_abundance.csv")



