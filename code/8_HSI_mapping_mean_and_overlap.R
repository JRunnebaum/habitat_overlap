####################################
#load libraries
####################################

library(sp)
library(maptools)
library(rgdal)
library(gstat)
library(maps)
library(fields)
library(animation)
library(lattice)
library(colorRamps)
library(mgcv)
require(plyr)
library(automap)
library(dplyr)

####################################
#define species by season
####################################
#run lobster first to add those data to cusk predictions to then evaluate overlap

# PATH <- "output/lobster_spring/"
# PATH <- "output/cusk_spring/"

# PATH <- "output/lobster_spring/previous_HSI/"
# PATH <- "output/cusk_spring/previous_HSI/"



# PATH <- "output/lobster_fall/"
PATH <- "output/cusk_fall/"



####################################
#load each years HSI data 
#for the selected species and season
####################################
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





####################################
#select either AMM or GMM HSI
####################################

for (i in 1:length(x)){
  x[[i]] = x[[i]][, c("latitude", "longitude", "AMM_HSI")] #AMM_HSI
}

ts = join_all(x, by = c("latitude", "longitude"))

####################################
#calculate median HSI
####################################
ts$HSI = apply(ts[-c(1,2)], 1, median, na.rm = TRUE)
med.HSI = ts[-c(3:36)] # for all years


coordinates(med.HSI) = ~longitude + latitude

bubble(med.HSI, "HSI", col = 4)

v = variogram(HSI ~ 1, med.HSI)
auto = autofitVariogram(HSI ~ 1, med.HSI)

####################################
#define grid and stat range
####################################

#.1original 0.05/0.05, 0.3 smooths outs with .1/.1 , .2 .1/.1 good
#.5 .05/.05 best! but takes a long time
g = gstat(formula = HSI ~ 1, model = auto$var_model, data = med.HSI, maxdist = 0.3)# width of the interpolation circle (bigger is smoother, small more fine scale, computation time increases when bigger)
  
#range
xrange = range(-71.06969,-65.51969)
yrange = range(39.61908, 44.83908)

 
grid = expand.grid(longitude = seq(from = xrange[1], to = xrange[2], by = 0.03),
                    latitude = seq(from = yrange[1], to = yrange[2], by = 0.03))
gridded(grid) = ~longitude + latitude
  
####################################
#predict HSI over grid
####################################

p.lob <- predict(g, newdata = grid)
p.lob$lobster <- p.lob$var1.pred #use this if projecting MEAN HSI
p.lob$HSI.1 <- ifelse(p.lob$lobster > 0.1, p.lob$lobster, NA)
p.lob$HSI.3 <- ifelse(p.lob$lobster > 0.3, p.lob$lobster, NA)
p.lob$HSI.5 <- ifelse(p.lob$lobster > 0.5, p.lob$lobster, NA)
summary(p.lob) 

p.cusk<-predict(g, newdata = grid)
p.cusk$cusk <-p.cusk$var1.pred #use this if projecting MEAN HSI
p.cusk$HSI.1 <- ifelse(p.cusk$cusk >= 0.1, p.cusk$cusk, NA)
p.cusk$HSI.3 <- ifelse(p.cusk$cusk >= 0.3, p.cusk$cusk, NA)
p.cusk$HSI.5 <- ifelse(p.cusk$cusk >= 0.5, p.cusk$cusk, NA)

summary(p.cusk)

#add lobster predictions to cusk predictions
p.cusk$lob <- p.lob$lobster
p.cusk$lob.1<-p.lob$HSI.1
p.cusk$lob.3<-p.lob$HSI.3
p.cusk$lob.5<-p.lob$HSI.5
summary(p.cusk$lob.5)
p.cusk$difference.1 <- p.cusk$lob.1 - p.cusk$HSI.1
p.cusk$difference <- p.cusk$lob.3 - p.cusk$HSI.3
p.cusk$difference.5 <- p.cusk$lob.5 -  p.cusk$HSI.5
p.cusk$difference_all <- p.cusk$lob - p.cusk$cusk  
summary(p.cusk$HSI.5)
summary(p.cusk$lob.5)
summary(p.cusk$difference)
summary(p.cusk$difference.5)
  
####################################
##decide color pattern, 
####################################
col = matlab.like(1200) #matlab.like colors
  
  
trellis.par.get() # need this to enlarge font size on color bar
  
####################################
#load US coastline, banks, and stat areas
####################################
coast <-  readOGR(dsn='data/gshhg-shp-2.3.4/GSHHS_shp/i', layer='GSHHS_i_L1')
gom <-  readOGR(dsn = 'data/banks', layer = 'PhysioRegions_WGS84')
stat_area <- readOGR(dsn = 'data/Statistical_Area', layer="Statistical_Area")

  
coast_poly <- list(c("sp.polygons", coast, first = FALSE, fill = "white"))
stat_area_poly <- list(c("sp.polygons", stat_area, first = FALSE, fill = "transparent"))
gom_poly <- list(c("sp.polygons", gom, first = FALSE, fill = "transparent"))

####################################
#plot median SPRING HSI for lobster 
####################################
jpeg(filename = "figures/spring_lobster_hsi.jpg", width = 8, height = 7, units = "in", res = 600)
spplot(p.lob,
       sp.layout = list(coast_poly, gom_poly),
       at = (0:1000)/1000, #for HSI 0-1 scale
       par.settings = list(fontsize = list(text = 20)),
       col.regions = col, 
       zcol = "lobster",
       xlim = c(-71, -65.5),
       ylim = c(40, 45),
       scales =list(draw=T), 
       main = list(label="Spring Lobster"), 
       xlab = list(label="Longitude"), 
       ylab = list(label="Latitude"), 
       draw.colorkey = list(draw = T))
dev.off()

####################################
#plot median SPRING HSI for cusk
####################################
jpeg(filename = "figures/spring_cusk_hsi.jpg", width = 8, height = 7, units = "in", res = 600)
# jpeg(filename = "figures/previous_HSI/spring_cusk_hsi.jpg", width = 8, height = 7, units = "in", res = 600)
spplot(p.cusk,
       sp.layout = list(coast_poly, gom_poly),
             at = (0:1020)/1000, #for HSI 0-1 scale
             par.settings = list(fontsize = list(text = 20)),
             col.regions = col, 
             zcol = "cusk",
             xlim = c(-71, -65.5),
             ylim = c(40, 45),
             scales =list(draw=T), 
             main = list(label="Spring Cusk"), 
             xlab = list(label="Longitude"), 
             ylab = list(label="Latitude"), 
             draw.colorkey = list(draw = T)) 
dev.off()

####################################
#plot median HSI for lobster fall
####################################
jpeg(filename = "figures/fall_lobster_hsi.jpg", width = 8, height = 7, units = "in", res = 600)
spplot(p.lob,
       sp.layout = list(coast_poly, gom_poly),
       at = (0:1000)/1000, #for HSI 0-1 scale
       par.settings = list(fontsize = list(text = 20)),
       col.regions = col, 
       zcol = "lobster",
       xlim = c(-71, -65.5),
       ylim = c(40, 45),
       scales =list(draw=T), 
       main = list(label="Fall Lobster"), 
       xlab = list(label="Longitude"), 
       ylab = list(label="Latitude"), 
       draw.colorkey = list(draw = T))
dev.off()

####################################
#plot median FALL HSI for cusk
####################################

jpeg(filename = "figures/fall_cusk_hsi.jpg", width = 8, height = 7, units = "in", res = 600)
spplot(p.cusk,
       sp.layout = list(coast_poly, gom_poly),
       at = (-30:1030)/1000, #for HSI 0-1 scale
       par.settings = list(fontsize = list(text = 20)),
       col.regions = col, 
       zcol = "cusk",
       xlim = c(-71, -65.5),
       ylim = c(40, 45),
       scales =list(draw=T), 
       main = list(label="Fall Cusk"), 
       xlab = list(label="Longitude"), 
       ylab = list(label="Latitude"), 
       draw.colorkey = list(draw = T)) 
dev.off()






####################################
#color scheme for plotting overlap
####################################

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
# steps = csteps = c("#810f7c", "#edf8fb", "#8c96c6")#for looking at overlap
steps = csteps = c("#abd9e9", "#d7191c", "#ffffbf")#for looking at overlap "#d95f0e", "#2b8cbe"
# steps = csteps = c("purple", "orange", "blue")#for looking at overlap 

pal = color.palette(steps, space="rgb")
col = pal

####################################
#load sea sampling data
####################################
lss <- read.csv("data/HSI_data/LSS_2006_2013_cusk_lob_correct.csv")
lss <- lss %>%
  filter(cusk_quantity > 0)

lss.sp <- lss %>%
  filter(lss$month == 3 | lss$month == 4 | lss$month == 5)

coordinates(lss.sp) = ~ lon + lat
cusk_sp <- list(c("sp.points", lss.sp, zcol = "cusk_quantity",pch = 19))

lss.fl <- lss %>%
   filter(lss$month == 9 | lss$month == 10 | lss$month == 11)

coordinates(lss.fl) = ~ lon + lat
cusk_fl <- list(c("sp.points", lss.fl, zcol = "cusk_quantity", pch = 19))


####################################
#plot SPRING overlap
####################################
jpeg(filename = "figures/spring_overlap_HSI.1.jpg", width = 8, height = 7, units = "in", res = 600)
spplot(p.cusk,
       sp.layout = list(coast_poly, gom, cusk_sp),
       at = (-100:100)/100, #for HSI overlap
       par.settings = list(fontsize = list(text = 20)),
       col.regions = col, 
       zcol = "difference.1",
       xlim = c(-71, -65.5),
       ylim = c(40, 45),
       scales =list(draw=T), 
       main = list(label="Spring Overlap"), 
       xlab = list(label="Longitude"), 
       ylab = list(label="Latitude"), 
       draw.colorkey = list(draw = T))
dev.off()


####################################
#plot FALL overlap
####################################


# jpeg(filename = "figures/log_scale_figures/fall_overlap.jpg", width = 8, height = 7, units = "in", res = 600)
jpeg(filename = "figures/fall_overlap_HSI.1.jpg", width = 8, height = 7, units = "in", res = 600)
spplot(p.cusk,
       sp.layout = list(coast_poly, gom, cusk_fl),
       at = (-100.00:100.0)/100, #for HSI overlap
       par.settings = list(fontsize = list(text = 20)),
       col.regions = col, 
       zcol = "difference.1",
       xlim = c(-71, -65.5),
       ylim = c(40, 45),
       scales =list(draw=T), 
       main = list(label="Fall Overlap"), 
       xlab = list(label="Longitude"), 
       ylab = list(label="Latitude"), 
       draw.colorkey = list(draw = T))
dev.off()




