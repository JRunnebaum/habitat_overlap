######################
#load libraries
######################
library(sp)
library(maptools)
library(rgdal)
library(gstat)
library(maps)
library(fields)
library(animation)
library(lattice)
library(colorRamps)
library(grid)
library(mgcv)
require(plyr)
library(automap)
library(purrr)

######################
#Define Global variables
######################
#Spring or Fall
# SEASON <- "Spring"
SEASON <- "Fall"


# PATH_X <- "output/cusk_spring/"
# PATH_Y <- "output/lobster_spring/"

PATH_X <- "output/cusk_fall/"
PATH_Y <- "output/lobster_fall/"

#Where to save annual overlap maps
# PATH_OUT <- "output/overlap_spring/"
PATH_OUT <- "output/overlap_fall/"




#####################
#load species X data
#####################
ts1980 = read.csv(file = paste0(PATH_X,"ts1980.csv"), header = TRUE)
ts1981 = read.csv(file = paste0(PATH_X,"ts1981.csv"), header = TRUE)
ts1982 = read.csv(file = paste0(PATH_X,"ts1982.csv"), header = TRUE)
ts1983 = read.csv(file = paste0(PATH_X,"ts1983.csv"), header = TRUE)
ts1984 = read.csv(file = paste0(PATH_X,"ts1984.csv"), header = TRUE)
ts1985 = read.csv(file = paste0(PATH_X,"ts1985.csv"), header = TRUE)
ts1986 = read.csv(file = paste0(PATH_X,"ts1986.csv"), header = TRUE)
ts1987 = read.csv(file = paste0(PATH_X,"ts1987.csv"), header = TRUE)
ts1988 = read.csv(file = paste0(PATH_X,"ts1988.csv"), header = TRUE)
ts1989 = read.csv(file = paste0(PATH_X,"ts1989.csv"), header = TRUE)
ts1990 = read.csv(file = paste0(PATH_X,"ts1990.csv"), header = TRUE)
ts1991 = read.csv(file = paste0(PATH_X,"ts1991.csv"), header = TRUE)
ts1992 = read.csv(file = paste0(PATH_X,"ts1992.csv"), header = TRUE)
ts1993 = read.csv(file = paste0(PATH_X,"ts1993.csv"), header = TRUE)
ts1994 = read.csv(file = paste0(PATH_X,"ts1994.csv"), header = TRUE)
ts1995 = read.csv(file = paste0(PATH_X,"ts1995.csv"), header = TRUE)
ts1996 = read.csv(file = paste0(PATH_X,"ts1996.csv"), header = TRUE)
ts1997 = read.csv(file = paste0(PATH_X,"ts1997.csv"), header = TRUE)
ts1998 = read.csv(file = paste0(PATH_X,"ts1998.csv"), header = TRUE)
ts1999 = read.csv(file = paste0(PATH_X,"ts1999.csv"), header = TRUE)
ts2000 = read.csv(file = paste0(PATH_X,"ts2000.csv"), header = TRUE)
ts2001 = read.csv(file = paste0(PATH_X,"ts2001.csv"), header = TRUE)
ts2002 = read.csv(file = paste0(PATH_X,"ts2002.csv"), header = TRUE)
ts2003 = read.csv(file = paste0(PATH_X,"ts2003.csv"), header = TRUE)
ts2004 = read.csv(file = paste0(PATH_X,"ts2004.csv"), header = TRUE)
ts2005 = read.csv(file = paste0(PATH_X,"ts2005.csv"), header = TRUE)
ts2006 = read.csv(file = paste0(PATH_X,"ts2006.csv"), header = TRUE)
ts2007 = read.csv(file = paste0(PATH_X,"ts2007.csv"), header = TRUE)
ts2008 = read.csv(file = paste0(PATH_X,"ts2008.csv"), header = TRUE)
ts2009 = read.csv(file = paste0(PATH_X,"ts2009.csv"), header = TRUE)
ts2010 = read.csv(file = paste0(PATH_X,"ts2010.csv"), header = TRUE)
ts2011 = read.csv(file = paste0(PATH_X,"ts2011.csv"), header = TRUE)
ts2012 = read.csv(file = paste0(PATH_X,"ts2012.csv"), header = TRUE)
ts2013 = read.csv(file = paste0(PATH_X,"ts2013.csv"), header = TRUE)


x = list( ts1980=ts1980, ts1981=ts1981, ts1982=ts1982,
          ts1983=ts1983, ts1984=ts1984, ts1985=ts1985, ts1986=ts1986, ts1987=ts1987,
          ts1988=ts1988, ts1989=ts1989, ts1990=ts1990, ts1991=ts1991, ts1992=ts1992,
          ts1993=ts1993, ts1994=ts1994, ts1995=ts1995, ts1996=ts1996, ts1997=ts1997,
          ts1998=ts1998, ts1999=ts1999, ts2000=ts2000, ts2001=ts2001, ts2002=ts2002,
          ts2003=ts2003, ts2004=ts2004, ts2005=ts2005, ts2006=ts2006, ts2007=ts2007,
          ts2008=ts2008, ts2009=ts2009, ts2010=ts2010, ts2011=ts2011, ts2012=ts2012,
          ts2013=ts2013)


for (i in 1:length(x)){
  x[[i]] = x[[i]][, c("latitude", "longitude", "AMM_HSI")] 
}

list = list()
mp = numeric(length(x))

#######################
#load species Y
#######################
ts1980 = read.csv(file = paste0(PATH_Y,"ts1980.csv"), header = TRUE)
ts1981 = read.csv(file = paste0(PATH_Y,"ts1981.csv"), header = TRUE)
ts1982 = read.csv(file = paste0(PATH_Y,"ts1982.csv"), header = TRUE)
ts1983 = read.csv(file = paste0(PATH_Y,"ts1983.csv"), header = TRUE)
ts1984 = read.csv(file = paste0(PATH_Y,"ts1984.csv"), header = TRUE)
ts1985 = read.csv(file = paste0(PATH_Y,"ts1985.csv"), header = TRUE)
ts1986 = read.csv(file = paste0(PATH_Y,"ts1986.csv"), header = TRUE)
ts1987 = read.csv(file = paste0(PATH_Y,"ts1987.csv"), header = TRUE)
ts1988 = read.csv(file = paste0(PATH_Y,"ts1988.csv"), header = TRUE)
ts1989 = read.csv(file = paste0(PATH_Y,"ts1989.csv"), header = TRUE)
ts1990 = read.csv(file = paste0(PATH_Y,"ts1990.csv"), header = TRUE)
ts1991 = read.csv(file = paste0(PATH_Y,"ts1991.csv"), header = TRUE)
ts1992 = read.csv(file = paste0(PATH_Y,"ts1992.csv"), header = TRUE)
ts1993 = read.csv(file = paste0(PATH_Y,"ts1993.csv"), header = TRUE)
ts1994 = read.csv(file = paste0(PATH_Y,"ts1994.csv"), header = TRUE)
ts1995 = read.csv(file = paste0(PATH_Y,"ts1995.csv"), header = TRUE)
ts1996 = read.csv(file = paste0(PATH_Y,"ts1996.csv"), header = TRUE)
ts1997 = read.csv(file = paste0(PATH_Y,"ts1997.csv"), header = TRUE)
ts1998 = read.csv(file = paste0(PATH_Y,"ts1998.csv"), header = TRUE)
ts1999 = read.csv(file = paste0(PATH_Y,"ts1999.csv"), header = TRUE)
ts2000 = read.csv(file = paste0(PATH_Y,"ts2000.csv"), header = TRUE)
ts2001 = read.csv(file = paste0(PATH_Y,"ts2001.csv"), header = TRUE)
ts2002 = read.csv(file = paste0(PATH_Y,"ts2002.csv"), header = TRUE)
ts2003 = read.csv(file = paste0(PATH_Y,"ts2003.csv"), header = TRUE)
ts2004 = read.csv(file = paste0(PATH_Y,"ts2004.csv"), header = TRUE)
ts2005 = read.csv(file = paste0(PATH_Y,"ts2005.csv"), header = TRUE)
ts2006 = read.csv(file = paste0(PATH_Y,"ts2006.csv"), header = TRUE)
ts2007 = read.csv(file = paste0(PATH_Y,"ts2007.csv"), header = TRUE)
ts2008 = read.csv(file = paste0(PATH_Y,"ts2008.csv"), header = TRUE)
ts2009 = read.csv(file = paste0(PATH_Y,"ts2009.csv"), header = TRUE)
ts2010 = read.csv(file = paste0(PATH_Y,"ts2010.csv"), header = TRUE)
ts2011 = read.csv(file = paste0(PATH_Y,"ts2011.csv"), header = TRUE)
ts2012 = read.csv(file = paste0(PATH_Y,"ts2012.csv"), header = TRUE)
ts2013 = read.csv(file = paste0(PATH_Y,"ts2013.csv"), header = TRUE)
#ts1978=ts1978, ts1979=ts1979,
y = list( ts1980=ts1980, ts1981=ts1981, ts1982=ts1982,
          ts1983=ts1983, ts1984=ts1984, ts1985=ts1985, ts1986=ts1986, ts1987=ts1987,
          ts1988=ts1988, ts1989=ts1989, ts1990=ts1990, ts1991=ts1991, ts1992=ts1992,
          ts1993=ts1993, ts1994=ts1994, ts1995=ts1995, ts1996=ts1996, ts1997=ts1997,
          ts1998=ts1998, ts1999=ts1999, ts2000=ts2000, ts2001=ts2001, ts2002=ts2002,
          ts2003=ts2003, ts2004=ts2004, ts2005=ts2005, ts2006=ts2006, ts2007=ts2007,
          ts2008=ts2008, ts2009=ts2009, ts2010=ts2010, ts2011=ts2011, ts2012=ts2012,
          ts2013=ts2013)


for (i in 1:length(y)){
  y[[i]] = y[[i]][, c("latitude", "longitude", "AMM_HSI")] 
}

list = list()
mp = numeric(length(y))

#######################
#select color palette to use for overlap
#######################
color.palette = function(steps, n.steps.between=NULL, ...){
  
  if(is.null(n.steps.between)) n.steps.between = rep(0, (length(steps)-1))
  if(length(n.steps.between) != length(steps)-1) stop("Must have one less n.steps.between value than steps")
  
  fill.steps = cumsum(rep(1, length(steps))+c(0,n.steps.between))
  RGB = matrix(NA, nrow=3, ncol=fill.steps[length(fill.steps)])
  RGB[,fill.steps] = col2rgb(steps)
  
  for(i in which(n.steps.between>0)){
    col.start=RGB[,fill.steps[i]]
    col.end=RGB[,fill.steps[i+1]]
    for(j in seq(3)){
      vals = seq(col.start[j], col.end[j], length.out=n.steps.between[i]+2)[2:(2+n.steps.between[i]-1)]  
      RGB[j,(fill.steps[i]+1):(fill.steps[i+1]-1)] = vals
    }
  }
  
  new.steps = rgb(RGB[1,], RGB[2,], RGB[3,], maxColorValue = 255)
  pal = colorRampPalette(new.steps, ...)
  return(pal)
}

steps = csteps = c("#e66101", "#f7f7f7", "#5e3c99")#for looking at overlap
pal = color.palette(steps, space="rgb")
col = pal

trellis.par.get() # need this to enlarge font size on color bar
coast = readOGR(dsn='data/gshhg-shp-2.3.4/GSHHS_shp/i', layer='GSHHS_i_L1')
a = list("sp.polygons", coast)

#######################
#START of LOP to RUN ANNUAL OVERLAP
#######################
yr <- seq(from = 1980,to = 2013, by = 1)
prop.ovlp <- length(yr)

for (i in 1:length(x)){
  ts_cusk = x[[i]]
  ts_lobs = y[[i]]
  
  coordinates(ts_cusk) = ~longitude + latitude
  bubble(ts_cusk, "AMM_HSI", col = 4)
  auto = autofitVariogram(AMM_HSI ~ 1, ts_cusk)
  plot(auto)
  
  coordinates(ts_lobs) = ~longitude + latitude
  bubble(ts_lobs, "AMM_HSI")
  auto = autofitVariogram(AMM_HSI ~ 1, ts_lobs)
  plot(auto)
  
  g = gstat(formula = AMM_HSI ~ 1, model = auto$var_model, data = ts_cusk, maxdist = 0.1)
  
  xrange = range(-71.06969,-65.51969)
  yrange = range(39.61908, 44.83908)
  
  grid= expand.grid(longitude = seq(from = xrange[1], to = xrange[2], by = .05), 
                    latitude = seq(from = yrange[1], to = yrange[2], by = .05))
  gridded(grid) = ~longitude + latitude
  p_cusk = predict(g, newdata = grid) #make prediction
  p_cusk$cusk_HSI.1 <- ifelse(p_cusk$var1.pred > 0.1, p_cusk$var1.pred, NA)
  p_cusk$cusk_HSI.3 <- ifelse(p_cusk$var1.pred > 0.3, p_cusk$var1.pred, NA)
  p_cusk$cusk_HSI.5 <- ifelse(p_cusk$var1.pred > 0.5, p_cusk$var1.pred, NA)
  
  f = gstat(formula = AMM_HSI ~ 1, model = auto$var_model, data = ts_lobs, maxdist = 0.1)
  xrange = range(-71.06969,-65.51969)
  yrange = range(39.61908, 44.83908)
  grid= expand.grid(longitude = seq(from = xrange[1], to = xrange[2], by = .05), 
                    latitude = seq(from = yrange[1], to = yrange[2], by = .05))
  gridded(grid) = ~longitude + latitude
  p_lobs = predict(f, newdata = grid) #make prediction
  p_cusk$lob_HSI.1 <- ifelse(p_lobs$var1.pred > 0.1, p_lobs$var1.pred, NA)
  p_cusk$lob_HSI.3 <- ifelse(p_lobs$var1.pred > 0.3, p_lobs$var1.pred, NA)
  p_cusk$lob_HSI.5 <- ifelse(p_lobs$var1.pred > 0.5, p_lobs$var1.pred, NA)
  
  p_cusk$overlap = (p_cusk$lob_HSI.1 - p_cusk$cusk_HSI.1)
  # p_cusk$overlap = (p_cusk$lob_HSI.3 - p_cusk$cusk_HSI.3)
  # p_cusk$overlap = (p_cusk$lob_HSI.5 - p_cusk$cusk_HSI.5)
  

  mypath <- PATH_OUT
  jpeg(filename = paste(mypath, names(x)[i], ".jpg", sep=''),
       width = 2000, height = 2000, res = 500)
  
  per <- plyr::count(is.na(p_cusk$overlap))
  per.ov <- per[1,]/(per[1,]+per[2,])
  prop <- round(per.ov$freq,4)
  prop.ovlp[i] <- prop
  mytxt <- list("sp.text", c(-70.5,44), prop)
  
  
    xx = spplot(p_cusk, 
                sp.layout = list(c("sp.polygons", coast,first=FALSE, fill = "white"),mytxt),
                at = (-50:50)/100,
                col.regions = col,
                zcol = "overlap",#turn this off when you want both variance and prediction map in one output
                main = paste(names(x)[i], SEASON))
            
              
  print(xx)
  
  dev.off()
  
}

proportion <- cbind(yr,prop.ovlp)
pr <- as.data.frame(proportion)

#####################
#plot spring overlap
#####################
windows()
jpeg(filename = "figures/spring_proportion_overlap_HSI_0.1.jpg", width = 8, height = 7, units = "in", res = 600)
# jpeg(filename = "figures/spring_proportion_overlap_HSI_0.3.jpg", width = 8, height = 7, units = "in", res = 600)
# jpeg(filename = "figures/spring_proportion_overlap_HSI_0.5.jpg", width = 8, height = 7, units = "in", res = 600)
mar.default<-c(4,5,4,2)+0.2
par(mar = mar.default + c(0,1,0,0))
plot(pr$prop.ovlp~pr$yr,type="l", 
     ylab="Proportion of Overlap",xlab="Year",main="Spring Overlap",
     cex.lab = 2, cex.axis =2, cex.main = 2, lwd = 3)
abline(lm(pr$prop.ovlp~pr$yr),lwd = 2, lty = 2, col = "red")
dev.off()

write.csv(pr,"output/overlap_spring/spring_proportion_overlap_hsi_0.1.csv")
# write.csv(pr,"output/overlap_spring/spring_proportion_overlap_hsi_0.3.csv")
# write.csv(pr,"output/overlap_spring/spring_proportion_overlap_hsi_0.5.csv")

######################
#Fall
######################

windows()
jpeg(filename = "figures/fall_proportion_overlap_HSI_0.1.jpg", width = 8, height = 7, units = "in", res = 600)
# jpeg(filename = "figures/fall_proportion_overlap_HSI_0.3.jpg", width = 8, height = 7, units = "in", res = 600)
# jpeg(filename = "figures/fall_proportion_overlap_HSI_0.5.jpg", width = 8, height = 7, units = "in", res = 600)
mar.default<-c(4,5,4,2)+0.2
par(mar = mar.default + c(0,1,0,0))
plot(pr$prop.ovlp~pr$yr,type="l", 
     ylab="Proportion of Overlap",xlab="Year",main="Fall Overlap",
     cex.lab = 2, cex.axis =2, cex.main = 2, lwd = 3)
abline(lm(pr$prop.ovlp~pr$yr),lwd = 2, lty = 2, col = "red")
dev.off()

write.csv(pr,"output/overlap_fall/fall_proportion_overlap_hsi_0.1.csv")
# write.csv(pr,"output/overlap_fall/fall_proportion_overlap_hsi_0.3.csv")
# write.csv(pr,"output/overlap_fall/fall_proportion_overlap_hsi_0.5.csv")

