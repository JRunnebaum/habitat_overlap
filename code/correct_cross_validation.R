library(classInt)

# PATH <- "output/cusk_spring/"
# PATH <- "output/cusk_fall/"

PATH <- "output/lobster_spring/"
# PATH <- "output/lobster_fall/"
#####################
#load species X data
#####################
ts1980 = read.csv(file = paste0(PATH,"ts1980.csv"), header = TRUE)
ts1981 = read.csv(file = paste0(PATH,"ts1981.csv"), header = TRUE)
ts1982 = read.csv(file = paste0(PATH,"ts1982.csv"), header = TRUE)
ts1983 = read.csv(file = paste0(PATH,"ts1983.csv"), header = TRUE)
ts1984 = read.csv(file = paste0(PATH,"ts1984.csv"), header = TRUE)
ts1985 = read.csv(file = paste0(PATH,"ts1985.csv"), header = TRUE)
ts1986 = read.csv(file = paste0(PATH,"ts1986.csv"), header = TRUE)
ts1987 = read.csv(file = paste0(PATH,"ts1987.csv"), header = TRUE)
ts1988 = read.csv(file = paste0(PATH,"ts1988.csv"), header = TRUE)
ts1989 = read.csv(file = paste0(PATH,"ts1989.csv"), header = TRUE)
ts1990 = read.csv(file = paste0(PATH,"ts1990.csv"), header = TRUE)
ts1991 = read.csv(file = paste0(PATH,"ts1991.csv"), header = TRUE)
ts1992 = read.csv(file = paste0(PATH,"ts1992.csv"), header = TRUE)
ts1993 = read.csv(file = paste0(PATH,"ts1993.csv"), header = TRUE)
ts1994 = read.csv(file = paste0(PATH,"ts1994.csv"), header = TRUE)
ts1995 = read.csv(file = paste0(PATH,"ts1995.csv"), header = TRUE)
ts1996 = read.csv(file = paste0(PATH,"ts1996.csv"), header = TRUE)
ts1997 = read.csv(file = paste0(PATH,"ts1997.csv"), header = TRUE)
ts1998 = read.csv(file = paste0(PATH,"ts1998.csv"), header = TRUE)
ts1999 = read.csv(file = paste0(PATH,"ts1999.csv"), header = TRUE)
ts2000 = read.csv(file = paste0(PATH,"ts2000.csv"), header = TRUE)
ts2001 = read.csv(file = paste0(PATH,"ts2001.csv"), header = TRUE)
ts2002 = read.csv(file = paste0(PATH,"ts2002.csv"), header = TRUE)
ts2003 = read.csv(file = paste0(PATH,"ts2003.csv"), header = TRUE)
ts2004 = read.csv(file = paste0(PATH,"ts2004.csv"), header = TRUE)
ts2005 = read.csv(file = paste0(PATH,"ts2005.csv"), header = TRUE)
ts2006 = read.csv(file = paste0(PATH,"ts2006.csv"), header = TRUE)
ts2007 = read.csv(file = paste0(PATH,"ts2007.csv"), header = TRUE)
ts2008 = read.csv(file = paste0(PATH,"ts2008.csv"), header = TRUE)
ts2009 = read.csv(file = paste0(PATH,"ts2009.csv"), header = TRUE)
ts2010 = read.csv(file = paste0(PATH,"ts2010.csv"), header = TRUE)
ts2011 = read.csv(file = paste0(PATH,"ts2011.csv"), header = TRUE)
ts2012 = read.csv(file = paste0(PATH,"ts2012.csv"), header = TRUE)
ts2013 = read.csv(file = paste0(PATH,"ts2013.csv"), header = TRUE)


x = list( ts1980=ts1980, ts1981=ts1981, ts1982=ts1982,
          ts1983=ts1983, ts1984=ts1984, ts1985=ts1985, ts1986=ts1986, ts1987=ts1987,
          ts1988=ts1988, ts1989=ts1989, ts1990=ts1990, ts1991=ts1991, ts1992=ts1992,
          ts1993=ts1993, ts1994=ts1994, ts1995=ts1995, ts1996=ts1996, ts1997=ts1997,
          ts1998=ts1998, ts1999=ts1999, ts2000=ts2000, ts2001=ts2001, ts2002=ts2002,
          ts2003=ts2003, ts2004=ts2004, ts2005=ts2005, ts2006=ts2006, ts2007=ts2007,
          ts2008=ts2008, ts2009=ts2009, ts2010=ts2010, ts2011=ts2011, ts2012=ts2012,
          ts2013=ts2013)

# x = dplyr::bind_rows(x)
# x = dplyr::sample_n(x, 1000)

###########
#validation

# N = nrow(data)

res_amm = matrix(0, ncol = 4, nrow = 34)
res_gmm = matrix(0, ncol = 4, nrow = 34)

colnames(res_amm) = c("Intercept", "Slope", "R.Squared", "p_value")
colnames(res_gmm) = c("Intercept", "Slope", "R.Squared", "p_value")

#AMM
for (i in 1:length(x)){
  
  data = x[[i]]
  data = data[,c("latitude","longitude","temperature_b","salinity_b", "depth","sediment","abundance")]
  
  N = nrow(data)
  
  sub = sample(1:N, size = N/4)
  train = data[-sub,]
  test = data[sub,]
  
  #SI_Sediment
  sediment_r = aggregate(abundance ~ sediment, data = train, FUN="mean")
  sediment_t = aggregate(abundance ~ sediment, data = test, FUN="mean")
  
  colnames(sediment_r)[1] = "sediment"
  colnames(sediment_r)[2] = "abundance_s"
  sediment_r$SI_sediment_train = ((sediment_r$abundance_s - min(sediment_r[,2]))/
                              (max(sediment_r[,2]) - min(sediment_r[,2])))
  
  colnames(sediment_t)[1] = "sediment"
  colnames(sediment_t)[2] = "abundance_s"
  sediment_t$SI_sediment_test = ((sediment_t$abundance_s - min(sediment_t[,2]))/
                              (max(sediment_t[,2]) - min(sediment_t[,2])))
  
  test = merge(test, sediment_r[,c(1,3)], by = "sediment", all = TRUE)
  test = merge(test, sediment_t[,c(1,3)], by = "sediment", all = TRUE)
  
  #SI_temperature
  temp = train[,"temperature_b"]
  temp = as.numeric(as.character(temp))
  temperature_int = (classIntervals(temp, 5, style = "fisher"))
  temperature_int[[2]][1] = temperature_int[[2]][1]-0.1
  temperature_bins = (cut(temp, breaks = temperature_int$brks))
  train$temperature_bins = cut(temp, breaks = temperature_int$brks)
  temperature_r = aggregate(train$abundance ~ temperature_bins, data = train, FUN = "mean")
  colnames(temperature_r)[1] = "temperature_bins"
  colnames(temperature_r)[2] = "abundance_t"
  temperature_r$SI_temp_train = ((temperature_r$abundance_t - min(temperature_r[,2]))/(max(temperature_r[,2]) - min(temperature_r[,2])))
  temp = test[,"temperature_b"]
  test$temperature_bins = cut(temp, breaks = temperature_int$brks)
  test = merge(test, temperature_r[,c(1,3)], by = "temperature_bins", all=TRUE)
  
  temp = test[,"temperature_b"]
  temp = as.numeric(as.character(temp))
  temperature_int = (classIntervals(temp, 5, style = "fisher"))
  temperature_int[[2]][1] = temperature_int[[2]][1]-0.1
  temperature_bins = (cut(temp, breaks = temperature_int$brks))
  test$temperature_bins = cut(temp, breaks = temperature_int$brks)
  temperature_t = aggregate(test$abundance ~ temperature_bins, data = test, FUN = "mean")
  colnames(temperature_t)[1] = "temperature_bins"
  colnames(temperature_t)[2] = "abundance_t"
  temperature_t$SI_temp_test = ((temperature_t$abundance_t - min(temperature_t[,2]))/(max(temperature_t[,2]) - min(temperature_t[,2])))
  temp = test[,"temperature_b"]
  temp = as.numeric(as.character(temp))
  test$temperature_bins = cut(temp, breaks = temperature_int$brks)
  test = merge(test, temperature_t[,c(1,3)], by = "temperature_bins", all=TRUE)
  
  #SI_depth
  depth = train[,"depth"]
  depth = as.numeric(as.character(depth))
  depth_int = (classIntervals(depth, 5, style = "fisher"))
  depth_int[[2]][1] = depth_int[[2]][1]-0.1
  depth_bins = (cut(depth, breaks = depth_int$brks))
  train$depth_bins = cut(depth, breaks = depth_int$brks)
  depth_r = aggregate(train$abundance ~ depth_bins, data = train, FUN = "mean")
  colnames(depth_r)[1] = "depth_bins"
  colnames(depth_r)[2] = "abundance_d"
  depth_r$SI_depth_train = ((depth_r$abundance_d - min(depth_r[,2]))/(max(depth_r[,2]) - min(depth_r[,2])))
  depth = test[,"depth"]
  depth = as.numeric(as.character(depth))
  test$depth_bins = cut(depth, breaks = depth_int$brks)
  test = merge(test, depth_r[,c(1,3)], by = "depth_bins", all = TRUE)
  
  depth = test[,"depth"]
  depth = as.numeric(as.character(depth))
  depth_int = (classIntervals(depth, 5, style = "fisher"))
  depth_int[[2]][1] = depth_int[[2]][1]-0.1
  depth_bins = (cut(depth, breaks = depth_int$brks))
  test$depth_bins = cut(depth, breaks = depth_int$brks)
  depth_t = aggregate(test$abundance ~ depth_bins, data = test, FUN = "mean")
  colnames(depth_t)[1] = "depth_bins"
  colnames(depth_t)[2] = "abundance_d"
  depth_t$SI_depth_test = ((depth_t$abundance_d - min(depth_t[,2]))/(max(depth_t[,2]) - min(depth_t[,2])))
  depth = test[,"depth"]
  depth = as.numeric(as.character(depth))
  test$depth_bins = cut(depth, breaks = depth_int$brks)
  test = merge(test, depth_t[,c(1,3)], by = "depth_bins", all = TRUE)
  
  #SI_Salinity
  salinity = train[,"salinity_b"]
  salinity = as.numeric(as.character(salinity))
  salinity_int = (classIntervals(salinity, 5, style = "fisher"))
  salinity_int[[2]][1] =salinity_int[[2]][1]-0.1
  salinity_bins = (cut(salinity, breaks=salinity_int$brks))
  train$salinity_bins = cut(salinity, breaks = salinity_int$brks)
  salinity_r = aggregate(train$abundance ~ salinity_bins, data = train, FUN = "mean")
  colnames(salinity_r)[1] = "salinity_bins"
  colnames(salinity_r)[2] = "abundance_s"
  salinity_r$SI_salinity_train = ((salinity_r$abundance_s - min(salinity_r[,2]))/(max(salinity_r[,2]) - min(salinity_r[,2])))
  salinity = test[,"salinity_b"]
  salinity = as.numeric(as.character(salinity))
  test$salinity_bins = cut(salinity, breaks = salinity_int$brks)
  test <- merge(test, salinity_r[,c(1,3)], by = "salinity_bins", all = TRUE)
  
  salinity = test[,"salinity_b"]
  salinity = as.numeric(as.character(salinity))
  salinity_int = (classIntervals(salinity, 5, style = "fisher"))
  salinity_int[[2]][1] =salinity_int[[2]][1]-0.1
  salinity_bins = (cut(salinity, breaks=salinity_int$brks))
  test$salinity_bins = cut(salinity, breaks = salinity_int$brks)
  salinity_t = aggregate(test$abundance ~ salinity_bins, data = test, FUN = "mean")
  colnames(salinity_t)[1] = "salinity_bins"
  colnames(salinity_t)[2] = "abundance_s"
  salinity_t$SI_salinity_test = ((salinity_t$abundance_s - min(salinity_t[,2]))/(max(salinity_t[,2]) - min(salinity_t[,2])))
  salinity = test[,"salinity_b"]
  salinity = as.numeric(as.character(salinity))
  test$salinity_bins = cut(salinity, breaks = salinity_int$brks)
  test <- merge(test, salinity_t[,c(1,3)], by = "salinity_bins", all = TRUE)
  
  #AMM
  test$Obs_HSI = (1/4)*(test$SI_temp_test + test$SI_depth_test + test$SI_sediment_test + test$SI_salinity_test)
  test$Pred_HSI = (1/4)*(test$SI_temp_train + test$SI_depth_train + test$SI_sediment_train + test$SI_salinity_train)
  
  #GMM
  # test$Obs_HSI = (test$SI_temp_test + test$SI_depth_test + test$SI_sediment_test + test$SI_salinity_test)^(1/4)
  # test$Pred_HSI = (test$SI_temp_train + test$SI_depth_train + test$SI_sediment_train + test$SI_salinity_train)^(1/4)
  
  # test = test[,c("latitude", "longitude","abundance", "HSI")]
  test = test[,c("latitude", "longitude","Pred_HSI", "Obs_HSI")]
  
  
  if (i == 1) {
    plot(test$Obs_HSI, test$Pred_HSI, col = pals::parula(34)[i], main = "AMM",
         ylab = "Predicted HSI", xlab = "Observed HSI", pch = 20,  bty = "o",
         cex.lab = 1.5, cex.axis = 1.5)
    
  }else{
    
    points(test$Obs_HSI, test$Pred_HSI, col = pals::parula(34)[i], pch = 20)

  }
  
  q = lm(Obs_HSI ~ Pred_HSI, data = test)
  res_amm[i,] = c(summary(q)$coefficient[,1], summary(q)$r.squared, summary(q)$coefficient[2,4])

}

t_col = pals::parula(34)
col.labels = c("1980","2013")
plotrix::color.legend(0.01, #xl
                      0.8, #yb
                      0.05, #xr
                      1, #yt
                      col.labels,
                      t_col,
                      align="rb",
                      gradient="y")

abline(0,1, lty = 3, lwd = 5)
abline(a = mean(res_amm[,1]), b = mean(res_amm[,2]), col="black", lwd=5)
text(0.0, 0.75, paste("Intercept = ", round(mean(res_amm[,1]), digits = 2)), cex = 1.5, pos = 4)
text(0.0, 0.7, paste("Slope = ", round(mean(res_amm[,2]), digits = 2)), cex = 1.5, pos = 4)
text(0.0, 0.65, paste("R-squared = ", round(mean(res_amm[,3]), digits = 2)), cex = 1.5, pos = 4)
text(0.0, 0.6, paste(ifelse(mean(res_amm[,4]) < 0.01, 
                             "p value = < 0.01", round(mean(res_amm[,4]), digits = 2))), cex = 1.5, pos = 4)


#GMM
for (i in 1:length(x)){
  
  data = x[[i]]
  data = data[,c("latitude","longitude","temperature_b","salinity_b", "depth","sediment","abundance")]
  
  N = nrow(data)
  
  sub = sample(1:N, size = N/4)
  train = data[-sub,]
  test = data[sub,]
  
  #SI_Sediment
  sediment_r = aggregate(abundance ~ sediment, data = train, FUN="mean")
  sediment_t = aggregate(abundance ~ sediment, data = test, FUN="mean")
  
  colnames(sediment_r)[1] = "sediment"
  colnames(sediment_r)[2] = "abundance_s"
  sediment_r$SI_sediment_train = ((sediment_r$abundance_s - min(sediment_r[,2]))/
                                    (max(sediment_r[,2]) - min(sediment_r[,2])))
  
  colnames(sediment_t)[1] = "sediment"
  colnames(sediment_t)[2] = "abundance_s"
  sediment_t$SI_sediment_test = ((sediment_t$abundance_s - min(sediment_t[,2]))/
                                   (max(sediment_t[,2]) - min(sediment_t[,2])))
  
  test = merge(test, sediment_r[,c(1,3)], by = "sediment", all = TRUE)
  test = merge(test, sediment_t[,c(1,3)], by = "sediment", all = TRUE)
  
  #SI_temperature
  temp = train[,"temperature_b"]
  temp = as.numeric(as.character(temp))
  temperature_int = (classIntervals(temp, 5, style = "fisher"))
  temperature_int[[2]][1] = temperature_int[[2]][1]-0.1
  temperature_bins = (cut(temp, breaks = temperature_int$brks))
  train$temperature_bins = cut(temp, breaks = temperature_int$brks)
  temperature_r = aggregate(train$abundance ~ temperature_bins, data = train, FUN = "mean")
  colnames(temperature_r)[1] = "temperature_bins"
  colnames(temperature_r)[2] = "abundance_t"
  temperature_r$SI_temp_train = ((temperature_r$abundance_t - min(temperature_r[,2]))/(max(temperature_r[,2]) - min(temperature_r[,2])))
  temp = test[,"temperature_b"]
  test$temperature_bins = cut(temp, breaks = temperature_int$brks)
  test = merge(test, temperature_r[,c(1,3)], by = "temperature_bins", all=TRUE)
  
  temp = test[,"temperature_b"]
  temp = as.numeric(as.character(temp))
  temperature_int = (classIntervals(temp, 5, style = "fisher"))
  temperature_int[[2]][1] = temperature_int[[2]][1]-0.1
  temperature_bins = (cut(temp, breaks = temperature_int$brks))
  test$temperature_bins = cut(temp, breaks = temperature_int$brks)
  temperature_t = aggregate(test$abundance ~ temperature_bins, data = test, FUN = "mean")
  colnames(temperature_t)[1] = "temperature_bins"
  colnames(temperature_t)[2] = "abundance_t"
  temperature_t$SI_temp_test = ((temperature_t$abundance_t - min(temperature_t[,2]))/(max(temperature_t[,2]) - min(temperature_t[,2])))
  temp = test[,"temperature_b"]
  temp = as.numeric(as.character(temp))
  test$temperature_bins = cut(temp, breaks = temperature_int$brks)
  test = merge(test, temperature_t[,c(1,3)], by = "temperature_bins", all=TRUE)
  
  #SI_depth
  depth = train[,"depth"]
  depth = as.numeric(as.character(depth))
  depth_int = (classIntervals(depth, 5, style = "fisher"))
  depth_int[[2]][1] = depth_int[[2]][1]-0.1
  depth_bins = (cut(depth, breaks = depth_int$brks))
  train$depth_bins = cut(depth, breaks = depth_int$brks)
  depth_r = aggregate(train$abundance ~ depth_bins, data = train, FUN = "mean")
  colnames(depth_r)[1] = "depth_bins"
  colnames(depth_r)[2] = "abundance_d"
  depth_r$SI_depth_train = ((depth_r$abundance_d - min(depth_r[,2]))/(max(depth_r[,2]) - min(depth_r[,2])))
  depth = test[,"depth"]
  depth = as.numeric(as.character(depth))
  test$depth_bins = cut(depth, breaks = depth_int$brks)
  test = merge(test, depth_r[,c(1,3)], by = "depth_bins", all = TRUE)
  
  depth = test[,"depth"]
  depth = as.numeric(as.character(depth))
  depth_int = (classIntervals(depth, 5, style = "fisher"))
  depth_int[[2]][1] = depth_int[[2]][1]-0.1
  depth_bins = (cut(depth, breaks = depth_int$brks))
  test$depth_bins = cut(depth, breaks = depth_int$brks)
  depth_t = aggregate(test$abundance ~ depth_bins, data = test, FUN = "mean")
  colnames(depth_t)[1] = "depth_bins"
  colnames(depth_t)[2] = "abundance_d"
  depth_t$SI_depth_test = ((depth_t$abundance_d - min(depth_t[,2]))/(max(depth_t[,2]) - min(depth_t[,2])))
  depth = test[,"depth"]
  depth = as.numeric(as.character(depth))
  test$depth_bins = cut(depth, breaks = depth_int$brks)
  test = merge(test, depth_t[,c(1,3)], by = "depth_bins", all = TRUE)
  
  #SI_Salinity
  salinity = train[,"salinity_b"]
  salinity = as.numeric(as.character(salinity))
  salinity_int = (classIntervals(salinity, 5, style = "fisher"))
  salinity_int[[2]][1] =salinity_int[[2]][1]-0.1
  salinity_bins = (cut(salinity, breaks=salinity_int$brks))
  train$salinity_bins = cut(salinity, breaks = salinity_int$brks)
  salinity_r = aggregate(train$abundance ~ salinity_bins, data = train, FUN = "mean")
  colnames(salinity_r)[1] = "salinity_bins"
  colnames(salinity_r)[2] = "abundance_s"
  salinity_r$SI_salinity_train = ((salinity_r$abundance_s - min(salinity_r[,2]))/(max(salinity_r[,2]) - min(salinity_r[,2])))
  salinity = test[,"salinity_b"]
  salinity = as.numeric(as.character(salinity))
  test$salinity_bins = cut(salinity, breaks = salinity_int$brks)
  test <- merge(test, salinity_r[,c(1,3)], by = "salinity_bins", all = TRUE)
  
  salinity = test[,"salinity_b"]
  salinity = as.numeric(as.character(salinity))
  salinity_int = (classIntervals(salinity, 5, style = "fisher"))
  salinity_int[[2]][1] =salinity_int[[2]][1]-0.1
  salinity_bins = (cut(salinity, breaks=salinity_int$brks))
  test$salinity_bins = cut(salinity, breaks = salinity_int$brks)
  salinity_t = aggregate(test$abundance ~ salinity_bins, data = test, FUN = "mean")
  colnames(salinity_t)[1] = "salinity_bins"
  colnames(salinity_t)[2] = "abundance_s"
  salinity_t$SI_salinity_test = ((salinity_t$abundance_s - min(salinity_t[,2]))/(max(salinity_t[,2]) - min(salinity_t[,2])))
  salinity = test[,"salinity_b"]
  salinity = as.numeric(as.character(salinity))
  test$salinity_bins = cut(salinity, breaks = salinity_int$brks)
  test <- merge(test, salinity_t[,c(1,3)], by = "salinity_bins", all = TRUE)
  
  #AMM
  # test$Obs_HSI = (1/4)*(test$SI_temp_test + test$SI_depth_test + test$SI_sediment_test + test$SI_salinity_test)
  # test$Pred_HSI = (1/4)*(test$SI_temp_train + test$SI_depth_train + test$SI_sediment_train + test$SI_salinity_train)
  
  #GMM
  test$Obs_HSI = (test$SI_temp_test * test$SI_depth_test * test$SI_sediment_test * test$SI_salinity_test)^(1/4)
  test$Pred_HSI = (test$SI_temp_train * test$SI_depth_train * test$SI_sediment_train * test$SI_salinity_train)^(1/4)

  # test = test[,c("latitude", "longitude","abundance", "HSI")]
  test = test[,c("latitude", "longitude","Pred_HSI", "Obs_HSI")]
  
  
  if (i == 1) {
    plot(test$Obs_HSI, test$Pred_HSI, col = pals::parula(34)[i], main = "GMM", ylim = c(0,1), xlim = c(0,1),
         ylab = "Predicted HSI", xlab = "Observed HSI", pch = 20,  bty = "o",
         cex.lab = 1.5, cex.axis = 1.5)
    
  }else{
    
    points(test$Obs_HSI, test$Pred_HSI, col = pals::parula(34)[i], pch = 20)
    
  }
  
  q = lm(Obs_HSI ~ Pred_HSI, data = test)
  res_gmm[i,] = c(summary(q)$coefficient[,1], summary(q)$r.squared, summary(q)$coefficient[2,4])
  
}

t_col = pals::parula(34)
col.labels = c("1980","2013")
plotrix::color.legend(0.01, #xl
                      0.8, #yb
                      0.05, #xr
                      1, #yt
                      col.labels,
                      t_col,
                      align="rb",
                      gradient="y")

abline(0,1, lty = 3, lwd = 5)
abline(a = mean(res_gmm[,1]), b = mean(res_gmm[,2]), col="black", lwd=5)
text(0.0, 0.75, paste("Intercept = ", round(mean(res_gmm[,1]), digits = 2)), cex = 1.5, pos = 4)
text(0.0, 0.7, paste("Slope = ", round(mean(res_gmm[,2]), digits = 2)), cex = 1.5, pos = 4)
text(0.0, 0.65, paste("R-squared = ", round(mean(res_gmm[,3]), digits = 2)), cex = 1.5, pos = 4)
text(0.0, 0.6, paste(ifelse(mean(res_gmm[,4]) < 0.01, 
                             "p value = < 0.01", round(mean(res_gmm[,4]), digits = 2))), cex = 1.5, pos = 4)




