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


################################
#load data
################################


station <- read.csv("data/cusk_data_for_VAST/NMFS_BTS/1972-2015-Offshore_Strata_1-40_Station_All_Tows_original.csv")
cusk <- read.csv("data/cusk_data_for_VAST/NMFS_BTS/Cusk_Spring_Fall_BTS_Station_Catch.csv")
statarea <- rgdal::readOGR("data/Statistical_Area/Statistical_Area.shp")

################################
#tidy data
################################
station <- station %>% 
  mutate(tow_id = paste(CRUISE, STATION, STRATUM, TOW, sep = ".")) %>% 
  filter(YEAR >= 1980) %>% 
  droplevels() 
  

cusk <- cusk %>% 
  mutate(tow_id = paste(CRUISE, STATION, STRATUM, TOW, sep = "."))  %>% 
  filter(YEAR >= 1980) %>% 
  droplevels() 

################################
#cusk catch by year
################################
cusk <- cusk %>% 
  group_by(tow_id,YEAR) %>% 
  summarise(catch_n = sum(EXPANDED_CATCH_NUMBER)) 

################################
#combine data together
################################

nmfs <- full_join(station, cusk) %>% 
  mutate(begin_date = as.Date(BEGIN_TOW_DATE, format = '%m/%d/%Y'),
         month = month(begin_date),
         day = day(begin_date),
         doy = yday(begin_date)) %>% 
  filter(OPERATION_STATUS == "Representative") %>% 
  mutate(catch_n = replace_na(catch_n, 0),
         survey = "BTS",
         area_swept = case_when(VESSEL == "AL" ~ 0.038, #assumes that catches were standarized to albatoross by JR
                                VESSEL == "DE" ~ 0.038,
                                VESSEL == "HB" ~ 0.024)) %>% 
  select(tow_id, survey, year = YEAR, month, day, doy,	
         tow_duration = TOW_DURATION..min.., lat = DECDEG_BEGLAT, lon = DECDEG_BEGLON,	
         catch_n, vessel = VESSEL, area_swept) %>% 
  distinct() %>% 
  as.data.frame()


################################
#add stat area
################################
latlon <- nmfs %>% 
  select(lat, lon)

coordinates(latlon) <- ~lon + lat
CRS.new <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")  #EPSG:102003
proj4string(latlon) <- CRS.new 
proj4string(statarea) <- CRS.new #i get a warning message here that a new CRS was assigned to an object with an existing CRS wtihout reprojecting

plot(statarea)
points(latlon$lat~latlon$lon)

area <- over(latlon, statarea, returnList = FALSE) %>% 
  select(stat_area = Id)

nefsc <- cbind(area, nmfs) %>% 
    filter(stat_area == 464 | stat_area == 465 | stat_area == 466 | stat_area == 467 |
           stat_area == 511 | stat_area == 512 | stat_area == 513 | stat_area == 514 | 
           stat_area == 515 | stat_area == 521 | stat_area == 522 | stat_area == 525 | 
           stat_area == 526 | stat_area == 551 | stat_area == 561 | stat_area == 552 |
           stat_area == 562) %>%
  as.data.frame()

qplot(nefsc$lon, nefsc$lat, color = as.character(nefsc$stat_area))
qplot(nefsc$lon, nefsc$lat, color = log(nefsc$catch_n))

write_csv(nefsc, "data/cusk_data_for_VAST/NMFS_cusk_bts.csv")





