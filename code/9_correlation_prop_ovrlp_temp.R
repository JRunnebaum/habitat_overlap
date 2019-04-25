#manuscript figures for overlap and annual habitat related to temp

##################################################################
#load libraries
##################################################################
library(readr)
library(mgcv)
library(dplyr)
library(tseries)
library(tidyr)


##################################################################
#load data
##################################################################

BTbyGrid = read.csv("data/NECOFS_FVCOM/BTbyGrid_1980to2013.csv")

#overlap data
fall <- read.csv("output/overlap_fall/fall_proportion_overlap_hsi_0.1.csv")
spring <- read.csv("output/overlap_spring/spring_proportion_overlap_hsi_0.1.csv")

##################################################################
#calculate spring mean temperature and salinity
##################################################################
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

colnames(Thermo.field.sp) = as.character(1980:2013)

m_t_sp <- apply(Thermo.field.sp, 2, mean) %>% 
  as.data.frame() %>% 
  gather(yr, mean_t) %>% 
  mutate(yr = 1980:2013)

plot(mean_t ~ yr, data = m_t_sp, type = "l")


pr.sp <- merge(spring, m_t_sp, by="yr")
##################################################################
#fall mean temperature and salinity
##################################################################
#fall mean
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
colnames(Thermo.field.fl) = as.character(1980:2013)

m_t_fl <- apply(Thermo.field.fl, 2, mean) %>% 
  as.data.frame() %>% 
  gather(yr, mean_t) %>% 
  mutate(yr = 1980:2013)

plot(mean_t ~ yr, data = m_t_fl, type = "l")

pr.fl <- merge(fall, m_t_fl,by="yr")
##################################################################
#plot SPRING temperature and proportion overplap
##################################################################

jpeg(filename = "figures/spring_proportion_overlap_HSI_0.1.jpeg", width = 9, height = 6, units = "in", res = 600)
par(mar=c(5,5,2,5))
plot(pr.sp$mean_t ~ pr.sp$yr,
     type = "l",lty = 2,lwd = 4,
     # ylim = c(0,12),
     ylab = "Temperature (C)",
     xlab = "Year",main="Spring (HSI > 0.5)", 
     col = "#fc8d59", cex.lab = 2, cex.axis = 2, cex.main = 2)
par(new=T)
plot(pr.sp$prop.ovlp ~ pr.sp$yr, type="l",
     lty=1,lwd=4 ,axes=F, ylab=NA, xlab=NA, col="#7fcdbb")
abline(lm(pr.sp$prop.ovlp~pr.sp$yr),lwd = 2, lty = 2, col = "gray")
axis(side=4,cex.axis = 2)

mtext(side=4,line=3,"Proportion Overlap",cex = 2)
legend("topleft",
       legend=c("Mean BT", "Proportion Overlap"),
       lty=c(2,1),
       col=c("#fc8d59", "#7fcdbb"),
       lwd = 4,
       bty = "n")
    
box(which="plot")
dev.off()

##################################################################
##SPRING change data to times series data for correlation
##################################################################

mean_p_sp = ts(pr.sp$prop.ovlp, start = 1982, end = 2013, frequency = 1)
mean_t_sp = ts(pr.sp$mean_t, start = 1982, end = 2013, frequency = 1)


##################################################################
#SPRING cross-correlation temperature and proportion overlap
##################################################################

jpeg(filename = "figures/spring_correlation_temps.jpg", width = 9, height = 6, units = "in", res = 600)
par(mar = c(4,4,4,1)+1)
spring_ccf <- ccf(mean_t_sp, mean_p_sp,
    # xlim = c(-10,0), ylim = c(-0.3, 0.3),
    cex.lab = 2.5, cex.axis = 2,
    ylab = "Cross-correlation",
    lwd = 2,
    main = "Spring Temperature, HSI > 0.5")
dev.off()

cor(pr.sp$mean_t, pr.sp$prop.ovlp) #-0.06070295


##################################################################
#plot FALL temperature and proportion overplap
##################################################################

jpeg(filename = "figures/fall_proportion_overlap_HSI_0.1.jpeg",  width = 9, height = 6, units = "in", res = 600)
par(mar=c(5,5,2,5))
plot(pr.fl$mean_t ~ pr.fl$yr,
     type = "l",lty = 2,lwd = 4,
     ylab = "Temperature (C)",
     xlab = "Year",main="Fall (HSI > 0.3)", 
     col = "#fc8d59", cex.lab = 2, cex.axis = 2, cex.main = 2)
par(new=T)
plot(pr.fl$prop.ovlp ~ pr.fl$yr,
     type="l", lty=1,lwd=4 ,axes=F, ylab=NA, xlab=NA,col="#2c7fb8")
abline(lm(pr.fl$prop.ovlp~pr.fl$yr),lwd = 2, lty = 2, col = "gray")
axis(side=4,cex.axis = 2)
mtext(side=4,line=3,"Proportion Overlap",cex = 2)
legend("topleft",
       legend=c("Mean BT", "Proportion Overlap"),
       lty=c(2,1),
       col=c("#fc8d59", "#2c7fb8"),
       lwd = 4,
       bty = "n")
dev.off()

##################################################################
##change data to times series data for correlation
##################################################################

mean_p_fl = ts(pr.fl$prop.ovlp, start = 1982, end = 2013, frequency = 1)
mean_t_fl = ts(pr.fl$mean_t, start = 1982, end = 2013, frequency = 1)

##################################################################
#cross-correlation temperature and proportion overlap
##################################################################
jpeg(filename = "figures/fall_correlation_temp_prop_overlp.jpg", width = 9, height = 6, units = "in", res = 600)

par(mar = c(4,4,4,1)+1)
ccf(mean_t_fl, mean_p_fl,
    cex.lab = 2.5, cex.axis = 2, cex.main = 4,
    ylab = "Cross-correlation",
    # xlim = c(-10,0),
    lwd = 2,
    main = "Fall BT, HSI > 0.3", cex = 2)
dev.off()


cor(pr.fl$mean_t, pr.fl$prop.ovlp) #0.031

