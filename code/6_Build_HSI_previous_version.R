###############################
#original author = Kisei Tanaka
#revised by = Jocelyn Runnebaum
#last revised = 3/12/2019
################################

################################
library(classInt)
################################

################################
#load data
################################
#use for cusk HSI
# cusk.sp <- read.csv("data/HSI_data/cusk_spring_3mon_BT_BS_grid_abundance.csv")
# cusk.sp <- cusk.sp %>% 
#   filter(!is.na(depth),
#          depth <= 0)
# summary(cusk.sp$depth)
# summary(cusk.sp$mean_bs)
# summary(cusk.sp$mean_bt)
# 
# 
# #use for lobster HSI
# lob.sp <- read.csv("data/HSI_data/lob_spring_3mon_grid_abndance.csv", header = T)
# summary(lob.sp)
# lob.sp <- lob.sp %>%
#   filter(!is.na(depth),
#          depth <= 0)
# summary(lob.sp$depth)
# summary(lob.sp$mean_bs)
# summary(lob.sp$mean_bt)

################################
#load FALL data 
################################
#use for cusk HSI
cusk.fl <- read.csv("data/HSI_data/cusk_fall_3mon_BT_BS_grid_abundance.csv")
cusk.fl <- cusk.fl %>%
  filter(!is.na(depth),
         depth <= 0)
summary(cusk.fl$depth)
summary(cusk.fl$mean_bs)
summary(cusk.fl$mean_bt)


#use for lobster HSI
lob.fl <- read.csv("data/HSI_data/lob_fall_3mon_grid_abndance.csv")
summary(lob.fl)
lob.fl <- lob.fl %>%
  filter(!is.na(depth),
         depth <= 0)
summary(lob.fl$depth)
summary(lob.fl$mean_bs)
summary(lob.fl$mean_bt)


##########################
#----choose data----
##########################

#SET VARIABLES FOR SPRING
# data <- cusk.sp
# PATH_OUT <- "output/cusk_spring/previous_HSI/"
# 
# 
# data <- lob.sp
# PATH_OUT <- "output/lobster_spring/previous_HSI/"
# 
#SET VARIABLES FOR FALL

data <- cusk.fl
PATH_OUT <- "output/cusk_fall/previous_HSI/"


# data <- lob.fl
# PATH_OUT <- "output/lobster_fall/previous_HSI/"
 

#check data
head(data)
summary(data)
 

#----Predicting HSI----
#Build FVCOM data first"
#ts1978=ts1978, ts1979=ts1979,
x <- list(ts1980=ts1980, ts1981=ts1981, ts1982=ts1982,
          ts1983=ts1983, ts1984=ts1984, ts1985=ts1985, ts1986=ts1986, ts1987=ts1987,
          ts1988=ts1988, ts1989=ts1989, ts1990=ts1990, ts1991=ts1991, ts1992=ts1992,
          ts1993=ts1993, ts1994=ts1994, ts1995=ts1995, ts1996=ts1996, ts1997=ts1997,
          ts1998=ts1998, ts1999=ts1999, ts2000=ts2000, ts2001=ts2001, ts2002=ts2002,
          ts2003=ts2003, ts2004=ts2004, ts2005=ts2005, ts2006=ts2006, ts2007=ts2007,
          ts2008=ts2008, ts2009=ts2009, ts2010=ts2010, ts2011=ts2011, ts2012=ts2012,
          ts2013=ts2013)


##########################
#********RUN HSI**********
##########################
for (i in 1:length(x)){
  tstemp = x[[i]]
  data <- data[which(data$depth > min(tstemp$depth) & data$depth < max(tstemp$depth)),]
  tstemp$sediment<-as.factor(tstemp$sediment)
  tstemp <- tstemp[which(tstemp$salinity_b>21.27),]
  tstemp <- tstemp[!is.na(tstemp$depth),]
  # tstemp<-tstemp[which(tstemp$depth < 0 & tstemp$depth > -877),]
  #adjust fvcom data range 
  #sediment
  #par(mfrow=c(1,2))
  summary(data$sediment)
  summary(tstemp$sediment)
  #tstemp = tstemp[ which( ! tstemp$sediment %in% "gr"), ]
  
  #bottom temperature
  summary(data$mean_bt) 
  summary(tstemp$temperature_b)
  #tstemp = tstemp[which(tstemp$temperature_b < 24),]
  #tstemp = tstemp[which(tstemp$temperature_b > 7.5),]
  
  #bottom salinity
  summary(data$mean_bs)
  summary(tstemp$salinity_b)
  # tstemp = tstemp[which(tstemp$temperature_b >24),]
  # tstemp = tstemp[which(tstemp$temperature_b > 7.5),]
  # # 
  #depth
  summary(data$depth) #depth
  summary(tstemp$depth)
  # tstemp = tstemp[!is.na(tstemp$depth),]
  tstemp = tstemp[which(tstemp$depth <= -2),]
  
  #SI_Sediment
  sediment <-  aggregate(mean_abun ~ sediment, data = data, FUN="mean")
  
  colnames(sediment)[1] <-  "sediment"
  colnames(sediment)[2] <-  "abundance_s"
  sediment$SI_sediment <-  ((sediment$abundance_s - min(sediment[,2]))/
                            (max(sediment[,2]) - min(sediment[,2])))
  
  tstemp = merge(tstemp, sediment, by ="sediment", all = TRUE)
  
  #SI_temperature
  temp = tstemp[,"temperature_b"]
  temp = as.numeric(as.character(temp))
  temperature_int = (classIntervals(temp, 10, style = "fisher"))
  temperature_int[[2]][1] = temperature_int[[2]][1]-0.1
  temperature_bins = (cut(temp, breaks = temperature_int$brks))
  summary(temperature_bins)
  summary(data$temperature_b)
  
  temp = data[,"mean_bt"]
  temp = as.numeric(as.character(temp))
  data$temperature_bins = cut(temp, breaks = temperature_int$brks)
  temperature = aggregate(data$mean_abun ~ temperature_bins, data = data, FUN = "mean")
  colnames(temperature)[1] = "temperature_bins"
  colnames(temperature)[2] = "abundance_t"
  temperature$SI_temp = ((temperature$abundance_t - min(temperature[,2]))/
                           (max(temperature[,2]) - min(temperature[,2])))
  
  temp = tstemp[,"temperature_b"]
  temp = as.numeric(as.character(temp))
  tstemp$temperature_bins = cut(temp, breaks = temperature_int$brks)
  tstemp = merge(tstemp, temperature, by = "temperature_bins", all = TRUE)
  
  # # #SI_Salinity
  salinity = tstemp[,"salinity_b"]
  salinity = as.numeric(as.character(salinity))
  salinity_int = (classIntervals(salinity, 10, style = "fisher"))
  salinity_int[[2]][1] = salinity_int[[2]][1]-0.1
  sailinity_bins = (cut(salinity, breaks = salinity_int$brks))
  summary(sailinity_bins)
  summary(data$salinity_b)
  
  salinity = data[,"mean_bs"]
  salinity = as.numeric(as.character(salinity))
  data$salinity_bins = cut(salinity, breaks = salinity_int$brks)
  salinity = aggregate(data$mean_abun ~ salinity_bins, data = data, FUN = "mean")
  colnames(salinity)[1] = "salinity_bins"
  colnames(salinity)[2] = "abundance_s"
  salinity$SI_salinity = ((salinity$abundance_s - min(salinity[,2]))/
                           (max(salinity[,2]) - min(salinity[,2])))
  
  sal = tstemp[,"salinity_b"]
  sal = as.numeric(as.character(sal))
  tstemp$salinity_bins = cut(sal, breaks = salinity_int$brks)
  tstemp = merge(tstemp, salinity, by = "salinity_bins", all = TRUE)
  
  
  #SI_depth
  dep = tstemp[,"depth"]
  dep = as.numeric(as.character(dep))
  depth_int = (classIntervals(dep, 10, style = "fisher"))
  depth_int[[2]][1] = depth_int[[2]][1]-0.1
  depth_bins = (cut(dep, breaks = depth_int$brks))
  summary(depth_bins)
  summary(data$depth)
  
  dep = data[,"depth"]
  dep = as.numeric(as.character(dep))
  data$depth_bins = cut(dep, breaks = depth_int$brks)
  depth = aggregate(data$mean_abun ~ depth_bins, data = data, FUN = "mean")
  colnames(depth)[1] = "depth_bins"
  colnames(depth)[2] = "abundance_d"
  depth$SI_depth = ((depth$abundance_d - min(depth[,2]))/
                      (max(depth[,2]) - min(depth[,2])))
  
  dep = tstemp[,"depth"]
  dep = as.numeric(as.character(dep))
  tstemp$depth_bins = cut(dep, breaks = depth_int$brks)
  tstemp = merge(tstemp, depth, by = "depth_bins", all = TRUE)
  
  
  tstemp$AMM_HSI = (1/4)*(tstemp$SI_temp + tstemp$SI_depth + tstemp$SI_sediment + tstemp$SI_salinity)
  tstemp$GMM_HSI <- (tstemp$SI_temp * tstemp$SI_depth * tstemp$SI_sediment * tstemp$SI_salinity)^(1/4)
  
  tstemp = tstemp[,c("latitude", "longitude", "temperature_b", "salinity_b","depth", "sediment", "abundance", "AMM_HSI", "GMM_HSI")] #
  
  tstemp = tstemp[!is.na(tstemp$AMM_HSI),]
  #setwd("~/Desktop/JR")
  write.csv(tstemp, paste(PATH_OUT,names(x)[i], ".csv", sep = ""))
  
}


##########################
#Cross Validation
##########################
N = nrow(data)
res_amm = matrix(0, ncol = 3, nrow = 100)
colnames(res_amm) = c("Intercept", "Slope", "R.Squared")

res_gmm = matrix(0, ncol = 3, nrow = 100)
colnames(res_gmm) = c("Intercept", "Slope", "R.Squared")

data <- data %>% 
  dplyr::rename(abundance = mean_abun,
                temperature_b = mean_bt,
                salinity_b = mean_bs) %>% 
  select(lat, lon, abundance, sediment, sednum, depth, 
         temperature_b, salinity_b, temperature_bins, 
         salinity_bins, depth_bins) 

par(mfrow = c(1,1))


#AMM

for (i in 1:100){          #Begin loop
  
  sub = sample(1:N, size = N/5)
  train = data[-sub,]
  test = data[sub,]
  
  #SI_Sediment
  sediment_r = aggregate(abundance ~ sediment, data = train, FUN="mean")
  sediment_t = aggregate(abundance ~ sediment, data = test, FUN="mean")
  
  sediment_r <- sediment_r %>% 
    dplyr::rename(abundance_s = abundance) 
  
  sediment_r$SI_sediment_train = ((sediment_r$abundance_s - min(sediment_r[,2]))/
                                    (max(sediment_r[,2]) - min(sediment_r[,2])))
  
  sediment_t <- sediment_t %>% 
    dplyr::rename(abundance_s = abundance) 
  
  sediment_t$SI_sediment_test = ((sediment_t$abundance_s - min(sediment_t[,2]))/
                                   (max(sediment_t[,2]) - min(sediment_t[,2])))
  
  test = full_join(test, sediment_r, by = "sediment")
  test = full_join(test, sediment_t, by = "sediment")
  
  #SI_temperature
  temperature_r = aggregate(train$abundance ~ temperature_bins, data = train, FUN = "mean")
  
  temperature_r <- temperature_r %>% 
    dplyr::rename(abundance_t = "train$abundance") 
  
  temperature_r$SI_temp_train = ((temperature_r$abundance_t - min(temperature_r[,2]))/
                                   (max(temperature_r[,2]) - min(temperature_r[,2])))
  test = full_join(test, temperature_r, by = "temperature_bins")
  
  temperature_t = aggregate(test$abundance ~ temperature_bins, data = test, FUN = "mean")
  
  temperature_t <- temperature_t %>% 
    dplyr::rename(abundance_t = "test$abundance") 
  temperature_t$SI_temp_test = ((temperature_t$abundance_t - min(temperature_t[,2]))/
                                  (max(temperature_t[,2]) - min(temperature_t[,2])))
  test = full_join(test, temperature_t, by = "temperature_bins")
  
  # #SI_depth
  depth_r = aggregate(train$abundance ~ depth_bins, data = train, FUN = "mean")
  
  depth_r <- depth_r %>% 
    dplyr::rename(abundance_d = "train$abundance") 
  
  depth_r$SI_depth_train = ((depth_r$abundance_d - min(depth_r[,2]))/
                              (max(depth_r[,2]) - min(depth_r[,2])))
  test = full_join(test, depth_r, by = "depth_bins")
  
  depth_t = aggregate(test$abundance ~ depth_bins, data = test, FUN = "mean")
  depth_t <- depth_t %>% 
    dplyr::rename(abundance_d = "test$abundance") 
  depth_t$SI_depth_test = ((depth_t$abundance_d - min(depth_t[,2]))/
                             (max(depth_t[,2]) - min(depth_t[,2])))
  test = full_join(test, depth_t, by = "depth_bins")
  
  # #SI_Salinity
  salinity_r = aggregate(train$abundance ~ salinity_bins, data = train, FUN = "mean")
  salinity_r <- salinity_r %>% 
    dplyr::rename(abundance_p = "train$abundance") 
  salinity_r$SI_salinity_train = ((salinity_r$abundance_p - min(salinity_r[,2]))/
                                    (max(salinity_r[,2]) - min(salinity_r[,2])))
  test = full_join(test, salinity_r, by="salinity_bins")
  
  salinity_t = aggregate(test$abundance ~ salinity_bins, data = test, FUN = "mean")
  salinity_t <- salinity_t %>% 
    dplyr::rename(abundance_p = "test$abundance") 
  salinity_t$SI_salinity_test = ((salinity_t$abundance_p - min(salinity_t[,2]))/
                                   (max(salinity_t[,2]) - min(salinity_t[,2])))
  test <- full_join(test, salinity_t, by="salinity_bins")
  
  
  test <- test %>% 
    mutate(Pred_HSI = (1/4)*(SI_temp_train + SI_depth_train + SI_sediment_train + SI_salinity_train)) 
  
  test <- test %>% 
    mutate(Obs_HSI = (1/4)*(SI_temp_test + SI_depth_test + SI_sediment_test + SI_salinity_test))
  
  if (i == 1) {
    
    plot(test$Obs_HSI, test$Pred_HSI, ylab = "Predicted HSI", xlab = "Observed HSI",
         main = "AMM", cex.lab = 1.5, cex.axis = 1.5,
         ylim = c(0,1), xlim = c(0,1), col = pals::parula(100)[i],
         bty = "o",
         pch = 20)
    
  } else {
    
    points(test$Obs_HSI, test$Pred_HSI, ylab = "Predicted HSI", xlab = "Observed HSI",
           ylim = c(0,1), xlim = c(0,1), col = pals::parula(100)[i], pch = 20)
  }
  
  compare = na.omit(data.frame(cbind(obs = test$Obs_HSI, pred = test$Pred_HSI)))
  
  q = lm(Pred_HSI ~ Obs_HSI, data = test)
  
  res_amm[i,] = c(summary(q)$coefficient[,1], summary(q)$r.squared)
  
  
}

legend("topleft",
       cex = 2,
       legend=c(paste0(expression(alpha), " = ", round(mean(res_amm[,1]), digits = 3)),
                paste0(expression(beta), " = ", round(mean(res_amm[,2]), digits = 3)), 
                paste0("R-squared = ", round(mean(res_amm[,3]), digits = 3))),
       lty=c(0,0),
       bty = "n")

abline(0,1, lty = 3, lwd = 2)
abline(a = mean(res_amm[,1]), b = mean(res_amm[,2]), col="black", lwd=5)


###### SAVE PLOT BY HAND ***THIS IS NOT AN AUTOMATIC SAVE** ############

#GMM
for (i in 1:100){          #Begin loop
  
  sub = sample(1:N, size = N/5)
  train = data[-sub,]
  test = data[sub,]
  
  #SI_Sediment
  sediment_r = aggregate(abundance ~ sediment, data = train, FUN="mean")
  sediment_t = aggregate(abundance ~ sediment, data = test, FUN="mean")
  
  sediment_r <- sediment_r %>% 
    dplyr::rename(abundance_s = abundance) 
  
  sediment_r$SI_sediment_train = ((sediment_r$abundance_s - min(sediment_r[,2]))/
                                    (max(sediment_r[,2]) - min(sediment_r[,2])))
  
  sediment_t <- sediment_t %>% 
    dplyr::rename(abundance_s = abundance) 
  
  sediment_t$SI_sediment_test = ((sediment_t$abundance_s - min(sediment_t[,2]))/
                                   (max(sediment_t[,2]) - min(sediment_t[,2])))
  
  test = full_join(test, sediment_r, by = "sediment")
  test = full_join(test, sediment_t, by = "sediment")
  
  #SI_temperature
  temperature_r = aggregate(train$abundance ~ temperature_bins, data = train, FUN = "mean")
  
  temperature_r <- temperature_r %>% 
    dplyr::rename(abundance_t = "train$abundance") 
  
  temperature_r$SI_temp_train = ((temperature_r$abundance_t - min(temperature_r[,2]))/
                                   (max(temperature_r[,2]) - min(temperature_r[,2])))
  test = full_join(test, temperature_r, by = "temperature_bins")
  
  temperature_t = aggregate(test$abundance ~ temperature_bins, data = test, FUN = "mean")
  
  temperature_t <- temperature_t %>% 
    dplyr::rename(abundance_t = "test$abundance") 
  temperature_t$SI_temp_test = ((temperature_t$abundance_t - min(temperature_t[,2]))/
                                  (max(temperature_t[,2]) - min(temperature_t[,2])))
  test = full_join(test, temperature_t, by = "temperature_bins")
  
  # #SI_depth
  depth_r = aggregate(train$abundance ~ depth_bins, data = train, FUN = "mean")
  
  depth_r <- depth_r %>% 
    dplyr::rename(abundance_d = "train$abundance") 
  
  depth_r$SI_depth_train = ((depth_r$abundance_d - min(depth_r[,2]))/
                              (max(depth_r[,2]) - min(depth_r[,2])))
  test = full_join(test, depth_r, by = "depth_bins")
  
  depth_t = aggregate(test$abundance ~ depth_bins, data = test, FUN = "mean")
  depth_t <- depth_t %>% 
    dplyr::rename(abundance_d = "test$abundance") 
  depth_t$SI_depth_test = ((depth_t$abundance_d - min(depth_t[,2]))/
                             (max(depth_t[,2]) - min(depth_t[,2])))
  test = full_join(test, depth_t, by = "depth_bins")
  
  # #SI_Salinity
  salinity_r = aggregate(train$abundance ~ salinity_bins, data = train, FUN = "mean")
  salinity_r <- salinity_r %>% 
    dplyr::rename(abundance_p = "train$abundance") 
  salinity_r$SI_salinity_train = ((salinity_r$abundance_p - min(salinity_r[,2]))/
                                    (max(salinity_r[,2]) - min(salinity_r[,2])))
  test = full_join(test, salinity_r, by="salinity_bins")
  
  salinity_t = aggregate(test$abundance ~ salinity_bins, data = test, FUN = "mean")
  salinity_t <- salinity_t %>% 
    dplyr::rename(abundance_p = "test$abundance") 
  salinity_t$SI_salinity_test = ((salinity_t$abundance_p - min(salinity_t[,2]))/
                                   (max(salinity_t[,2]) - min(salinity_t[,2])))
  test <- full_join(test, salinity_t, by="salinity_bins")
  
  
  test <- test %>% 
    mutate(Pred_HSI = (SI_temp_train * SI_depth_train * SI_sediment_train * SI_salinity_train)^(1/4))
  
  test <- test %>% 
    mutate(Obs_HSI = (SI_temp_test * SI_depth_test * SI_sediment_test * SI_salinity_test)^(1/4))
  
  if (i == 1) {
    
    plot(test$Obs_HSI, test$Pred_HSI, ylab = "Predicted HSI", xlab = "Observed HSI",
         main = "GMM", cex.lab = 1.5, cex.axis = 1.5,
         ylim = c(0,1), xlim = c(0,1), col = pals::parula(100)[i], 
         bty = "o",
         pch = 20)
    
  } else {
    
    points(test$Obs_HSI, test$Pred_HSI, ylab = "Predicted HSI", xlab = "Observed HSI",
           ylim = c(0,1), xlim = c(0,1), col = pals::parula(100)[i], pch = 20)
  }
  
  compare = na.omit(data.frame(cbind(obs = test$Obs_HSI, pred = test$Pred_HSI)))
  u = lm(Pred_HSI ~ Obs_HSI, data = test)
  res_gmm[i,] = c(summary(u)$coefficient[,1], summary(u)$r.squared)
  
  
}

legend("topleft",
       cex = 2,
       legend=c(paste0(expression(alpha), " = ", round(mean(res_gmm[,1]), digits = 3)),
                paste0(expression(beta), " = ", round(mean(res_gmm[,2]), digits = 3)), 
                paste0("R-squared = ", round(mean(res_gmm[,3]), digits = 3))),
       lty=c(0,0),
       bty = "n")

abline(0,1, lty = 3, lwd = 2)
abline(a = mean(res_gmm[,1]), b = mean(res_gmm[,2]), col="black", lwd=5)

###### SAVE PLOT BY HAND ***THIS IS NOT AN AUTOMATIC SAVE** ############