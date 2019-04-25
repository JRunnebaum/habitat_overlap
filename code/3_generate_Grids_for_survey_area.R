###############################
#original author = Lisha Guan
#revised by = Jocelyn Runnebaum
#last revised = 3/4/2019
################################




######################################################
#load packages
######################################################
library(dplyr)
library(tidyr)
library(gstat)
library(rgdal)
library(raster)
library(rgeos)
library(scales)
library(geosphere)
library(classInt)
library(sp)
library(maptools)
library(colorRamps)
library(ggplot2)

options(stringsAsFactors = FALSE)

######################################################
#load data
######################################################
dat <- read.csv("data/grid/lobster_all.csv",header=T)
depth <- raster("data/Depth raster/ne_atl_crm_v1.asc")
sed <- readOGR("data/Sediment/Sediment.shp")
stat_area <- rgdal::readOGR("data/Statistical_Area/Statistical_Area.shp")


######################################################
#Create the grid to cover the same extent as the survey area
######################################################
#identify min and max lat and lon
summary(dat$lat)
summary(dat$lon)


#lat lon needs to be in dec degree, ID the resolution (i.e. by=)
#spatial resolution for trawl survey:
#0.05 decimal degree in latitude and 0.05 decimal degree in longitude

latitude <-  seq(from = 39.75, to = 45, by = 0.05)
longitude <-  seq(from = - 71, to = - 65.5,  by = 0.05)
N <-  length(latitude) * length(longitude)
grid_id = matrix(0, nr = N, nc = 2)
k = 0
for (i in 1:length(latitude)){
  for (j in 1:length(longitude)){
    k = k + 1
    grid_id[k, ] = c(latitude[i], longitude[j])    
  }
}
head(grid_id)
colnames(grid_id) <- c("lat", "lon")
plot(grid_id[,1]~grid_id[,2])
points(dat$lat ~ dat$lon)

# write survey grid without environmental data
write.csv(grid_id, "data/grid/BTS_gom_gb_grid.csv") 

######################################################
#Extracting environmental data for grid cells
######################################################
latlon <- grid_id %>% 
  as.data.frame() %>% 
  dplyr::select(lat, lon)

coordinates(latlon) <- ~lon + lat
CRS.new <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")  #EPSG:102003
proj4string(latlon) <- CRS.new 
proj4string(sed) <- CRS.new #i get a warning message here that a new CRS was assigned to an object with an existing CRS wtihout reprojecting
proj4string(depth) <- CRS.new
proj4string(stat_area) <- CRS.new #i get a warning message here that a new CRS was assigned to an object with an existing CRS wtihout reprojecting

#extract sediment data
sed_points <- over(latlon, sed, returnList = FALSE) 

grid_id <- as.data.frame(grid_id)

grid <- cbind(grid_id, sed_points) %>% 
  as.data.frame()

#extract depth data
depth_points <- extract(x = depth, y = latlon, fun = mean)

depth_points <- as.data.frame(depth_points)

grid <- cbind(grid, depth_points)

#extract stat area
area <- over(latlon, stat_area, returnList = FALSE) %>% 
  dplyr::select(stat_area = Id)

grid <- cbind(grid, area) %>% 
  as.data.frame()
  

################calculate area for each grid for trawl surveys############

head(grid)

#calculate area of grid for the survey area.

source('functions/find_vertex_based_on_centroid_function.R')
vertex_for_surveygrid = find_vertex_based_on_centroid(x = grid[,2:3])
source('functions/Calculate_grid_area_function.R')
grid$Area_in_survey_km2 = Calculate_grid_area(vertex_for_surveygrid)
head(grid)



grid_no_na <- grid %>% 
  filter(!is.na(SEDNUM),
         !is.na(depth_points),
         !is.na(stat_area)) %>% 
  dplyr::select(Lat = lat, Lon = lon, Area = AREA, PERIMETER, 
                sediment = SEDIMENT, sednum = SEDNUM, 
                depth = depth_points, stat_area, Area_in_survey_km2)
head(grid_no_na)


plot(grid_no_na$lat ~ grid_no_na$lon)
points(dat$lat ~ dat$lon, col = "red")

# write.csv(grid, file = "data/grid/lobster_grid_GB_GOM_BODCdepth_SEDcat_area_swept_stat_area.csv")
save(grid_no_na, file="data/grid/lobster_grid_GB_GOM_BODCdepth_SEDcat_area_swept_strata_noNA.Rda")
