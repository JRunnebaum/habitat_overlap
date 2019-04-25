###############################
#author = Jocelyn Runnebaum
#last revised = 2/27/2019
################################


################################
#load libraries
################################
library(dplyr)
library(classInt)
library(lubridate)
library(tidyverse)
library(tidyselect)
library(sp)
library(maptools)
library(colorRamps)
library(ggplot2)
library(raster)
library(hms)

################################
#load data
################################
#2014 data
LL2014 <- read.csv("data/cusk_data_for_VAST/NMFS_LL/2014_catch.csv",header=T)
LL2014$year <- 2014
station2014 <- read.csv("data/cusk_data_for_VAST/NMFS_LL/2014_station_all.csv",header=T)



#2015 data
LL2015 <- read.csv("data/cusk_data_for_VAST/NMFS_LL/2015_catch.csv",header=T)
LL2015$year <- 2015
station2015 <- read.csv("data/cusk_data_for_VAST/NMFS_LL/2015_station_all.csv",header=T)

#combine catch data
catch <- rbind(LL2014, LL2015) %>% 
  mutate(tow_id = paste(CRUISE, STATION, STATION_ID, sep = ".")) %>% 
  dplyr::select(tow_id, year, catch_n = CATCHNUM)
 
  

#combine station data
station <- rbind(station2014, station2015) 

station <- station %>% 
  mutate(tow_id = paste(CRUISE, STATION, STATION_ID, sep = "."),
         BEGIN_HAUL_DATE = as.Date(BEGIN_HAUL_DATE, format = '%m/%d/%Y'),
         month = month(BEGIN_HAUL_DATE),
         day = day(BEGIN_HAUL_DATE),
         doy = yday(BEGIN_HAUL_DATE),
         survey = "LL",
         BEGIN_HAUL_TIME = parse_hm(BEGIN_HAUL_TIME),
         END_HAUL_TIME = parse_hm(END_HAUL_TIME),
         tow_duration = difftime( END_HAUL_TIME, BEGIN_HAUL_TIME, units = "mins")) %>% 
  dplyr::select(tow_id, survey, year = YEAR, month, day, doy,	
         tow_duration, begin_lat = DECDEG_BEGLAT_SET, begin_lon = DECDEG_BEGLON_SET,	
         end_lat = DECDEG_ENDLAT_SET, end_lon = DECDEG_ENDLON_SET, vessel = VESSEL_NAME, stat_area = AREA)


cusk_ll <- full_join(station, catch)
#find distance between beginning and end of the set
begin_ll <- station %>% 
  dplyr::select(begin_lat, begin_lon) %>% 
  SpatialPoints()


end_ll <- station %>% 
  dplyr::select(end_lat, end_lon) %>% 
  SpatialPoints()

distance <- pointDistance(begin_ll, end_ll, lonlat=T)

cusk_ll <- cusk_ll %>% 
  mutate(catch_n = replace_na(catch_n, 0),
         distance_km = distance/1000,
         AREA_SWEPT_25mx5m = distance_km*.025*.005, ## area swept 25mx5m
         AREA_SWEPT_50mx10m = distance_km*.05*.01, ## area swept 50mx10m
         AREA_SWEPT_100mx20m = distance_km*.1*.02, ## area swept 100mx20m
         AREA_SWEPT_200mx40m = distance_km*.2*.04, ## area swept 200mx40m
         AREA_SWEPT_500mx100m = distance_km*.5*.1, ## area swept 500mx100m
         AREA_SWEPT = distance_km*1.4)  ##area swept 1.4km

write.csv(cusk_ll, "data/cusk_data_for_VAST/NMFS_cusk_longline.csv")



