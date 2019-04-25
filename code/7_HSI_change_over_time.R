########################
#load libraries
########################

library(sp)
library(maptools)
library(rgdal)
library(gstat)
library(maps)
library(fields)
library(animation)
library(lattice)
library(colorRamps)
#library(grid)
library(mgcv)
require(plyr)
library(automap)
library(dplyr)

########################
#define species
########################

PATH <- "output/cusk_spring/previous_HSI/"
# PATH <- "output/lobster_spring/"



# PATH <- "output/cusk_fall/"


# PATH <- "output/lobster_fall/"


########################
#load each years HSI data
########################
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
ts2013 = read.csv(file = paste0(PATH,"ts2013.csv"), header = TRUE)#t,  

x = list(ts1980=ts1980, ts1981=ts1981, ts1982=ts1982,
         ts1983=ts1983, ts1984=ts1984, ts1985=ts1985, ts1986=ts1986, ts1987=ts1987,
         ts1988=ts1988, ts1989=ts1989, ts1990=ts1990, ts1991=ts1991, ts1992=ts1992,
         ts1993=ts1993, ts1994=ts1994, ts1995=ts1995, ts1996=ts1996, ts1997=ts1997,
         ts1998=ts1998, ts1999=ts1999, ts2000=ts2000, ts2001=ts2001, ts2002=ts2002,
         ts2003=ts2003, ts2004=ts2004, ts2005=ts2005, ts2006=ts2006, ts2007=ts2007,
         ts2008=ts2008, ts2009=ts2009, ts2010=ts2010, ts2011=ts2011, ts2012=ts2012,
         ts2013=ts2013)




########################
#select which HSI to use
########################

for (i in 1:length(x)){
  x[[i]] = x[[i]][, c("latitude", "longitude", "AMM_HSI")] #AMM_HSI
}

ts = join_all(x, by = c("latitude", "longitude"))

########################
#calculate median HSI
########################

ts$HSI = apply(ts[-c(1,2)], 1, median, na.rm = TRUE)
med.HSI = ts[-c(3:36)] # for all years

########################
# calculate difference in HSI over time series
########################

betaf = function(vec){
  
  beta = lm(vec ~ seq(1980:2013))$coef[2]
  # p = summary(lm(vec ~ seq(1978:2013)))$ coefficients [2,4]
  return(beta) # beta gives you a slope, if you want p-value, change it to p
  #   return(p) # beta gives you a slope, if you want p-value, change it to p
  
}
res = as.data.frame(apply(ts[, 3:36], 1, betaf))
med.HSI = cbind(ts[,1:2], res)
colnames(med.HSI)[3] = "HSI"
med.HSI<-med.HSI[!is.na(med.HSI$HSI),]


coordinates(med.HSI) = ~longitude + latitude

bubble(med.HSI, "HSI", col = 4)

v = variogram(HSI ~ 1, med.HSI)
auto = autofitVariogram(HSI ~ 1, med.HSI)


g = gstat(formula = HSI ~ 1, model = auto$var_model, data = med.HSI, maxdist = 0.3)# width of the interpolation circle (bigger is smoother, small more fine scale, computation time increases when bigger)

#SELECT RANGE
xrange = range(-71.06969,-65.51969)
yrange = range(39.61908, 44.83908)


grid = expand.grid(longitude = seq(from = xrange[1], to = xrange[2], by = 0.01),
                  latitude = seq(from = yrange[1], to = yrange[2], by = 0.01))
gridded(grid) = ~longitude + latitude

#################################
#Project changes in HSI
#********SELECT ONE**************
#################################

p.cusk <- predict(g, newdata = grid)
p.cusk$slope <- p.cusk$var1.pred


p.lob <- predict(g, newdata = grid)
p.lob$slope <- p.lob$var1.pred



##################################
#Set color palette
##################################

#blue-white-red colors USE to SHOW CHANGE OVER TIME 
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
steps = csteps = c("blue3", "azure", "coral3") #for looking at change in habitat
pal = color.palette(steps, space="rgb")
col = pal

trellis.par.get() # need this to enlarge font size on color bar

##################################
#Load shape files
##################################

#load US coastline, banks, and stat areas
coast <- readOGR(dsn='data/gshhg-shp-2.3.4/GSHHS_shp/i', layer='GSHHS_i_L1')
gom <-  readOGR(dsn = 'data/banks', layer = 'PhysioRegions_WGS84')
# stat.area <- readOGR(dsn = 'data/Statistical_Area', layer="Statistical_Area")


#####################################
#plot changes in Cusk habitat
#####################################

#Plot Changes in cusk SPRING habitat
jpeg(filename = "figures/previous_HSI/spring_cusk_change.jpg", width = 8, height = 7, units = "in", res = 600)
spplot(p.cusk,
       sp.layout = list(c("sp.polygons", coast,
                          first = FALSE, fill = "white"),
                        c("sp.polygons", gom,
                          first = FALSE, fill = "transparent")),
       at = (-10:10)/1000, #for change in HSI over years
       par.settings = list(fontsize = list(text = 20)),
       col.regions = col, 
       zcol = "slope",
       xlim = c(-71, -65.5),
       ylim = c(40, 45),
       scales =list(draw=T), 
       main = list(label="Spring Cusk"), 
       xlab = list(label="Longitude"), 
       ylab = list(label="Latitude"), 
       draw.colorkey = list(draw = T)) 
dev.off()


#Plot Changes in cusk FALL habitat
jpeg(filename = "figures/fall_cusk_change.jpg", width = 8, height = 7, units = "in", res = 600)
spplot(p.cusk,
       sp.layout = list(c("sp.polygons", coast,
                          first = FALSE, fill = "white"),
                        c("sp.polygons", gom,
                          first = FALSE, fill = "transparent")),
       at = (-15:15)/1000, #for change in HSI over years
       par.settings = list(fontsize = list(text = 20)),
       col.regions = col, 
       zcol = "slope",
       xlim = c(-71, -65.5),
       ylim = c(40, 45),
       scales =list(draw=T), 
       main = list(label="Fall Cusk"), 
       xlab = list(label="Longitude"), 
       ylab = list(label="Latitude"), 
       draw.colorkey = list(draw = T)) 
dev.off()



################################
#Plot changes in lobster habitat
################################
#Plot Changes in Lobster SPRING habitat
# jpeg(filename = "figures/spring_lobster_change.jpg", width = 8, height = 7, units = "in", res = 600)
spplot(p.lob,
       sp.layout = list(c("sp.polygons", coast,
                          first = FALSE, fill = "white"),
                        c("sp.polygons", gom,
                          first = FALSE, fill = "transparent")),
       at = (-15:15)/1000, #for change in HSI over years
       par.settings = list(fontsize = list(text = 20)),
       col.regions = col, 
       zcol = "slope",
       xlim = c(-71, -65.5),
       ylim = c(40, 45),
       scales =list(draw=T), 
       main = list(label="Spring Lobster"), 
       xlab = list(label="Longitude"), 
       ylab = list(label="Latitude"), 
       draw.colorkey = list(draw = T)) 
dev.off()

#Plot Changes in Lobster FALL habitat
jpeg(filename = "figures/fall_lobster_change.jpg", width = 8, height = 7, units = "in", res = 600)
spplot(p.lob,
       sp.layout = list(c("sp.polygons", coast,
         first = FALSE, fill = "white"),
       c("sp.polygons", gom,
         first = FALSE, fill = "transparent")),
       at = (-15:15)/1000, #for change in HSI over years
       par.settings = list(fontsize = list(text = 20)),
       col.regions = col, 
       zcol = "slope",
       xlim = c(-71, -65.5),
       ylim = c(40, 45),
       scales =list(draw=T), 
       main = list(label="Fall Lobster"), 
       xlab = list(label="Longitude"), 
       ylab = list(label="Latitude"), 
       draw.colorkey = list(draw = T)) 
dev.off()


