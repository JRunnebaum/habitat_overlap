###############################
#original author = Kisei Tanaka
#revised by = Jocelyn Runnebaum
#last revised = 2/19/2019
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

lobster <- read.csv('data/lobster_survey_data/MASS/MA_DMF_TRAWL_301_catch_length_data_update_82-16.csv', header=TRUE)
station_w_fvcom <- read.csv('data/lobster_survey_data/MASS/MA_Station_with_FVCOM.csv', header=TRUE)
statarea <- rgdal::readOGR("data/Statistical_Area/Statistical_Area.shp")


################################
#generate unique id 
#aggregate by unique id
################################
lobster <- lobster %>% 
  mutate(EXPANDED_NUMBER_AT_LENGTH = as.numeric(EXPANDED_NUMBER_AT_LENGTH)) %>% 
  mutate(tow_id = paste(CRUISE_CODE, STRATUM, STATION, sep="."),
         catch_n_m = ifelse(SEX == 1, EXPANDED_NUMBER_AT_LENGTH, 0),
         catch_n_f = ifelse(SEX == 2, EXPANDED_NUMBER_AT_LENGTH, 0),
         catch_n_a = ifelse(LENGTH.CM. > 6.0, EXPANDED_NUMBER_AT_LENGTH, 0),#THERE IS SOMETHING WRONG HERE OR ON THE NEXT LINE
         catch_n_j = ifelse(c(LENGTH.CM.<= 6.0 & LENGTH.CM. > 0), EXPANDED_NUMBER_AT_LENGTH, 0)) %>% 
  group_by(tow_id, LAT, LON, YEAR) %>% 
  summarise(catch_n = sum(EXPANDED_NUMBER_AT_LENGTH),
            catch_n_m = sum(catch_n_m),
            catch_n_f = sum(catch_n_f),
            catch_n_a = sum(catch_n_a),
            catch_n_j = sum(catch_n_j))

################################
#combine catch and station data together 
################################
station_w_fvcom <- station_w_fvcom %>% 
  mutate(tow_id = paste(station_w_fvcom$CRUISE_CODE, station_w_fvcom$STRATUM, station_w_fvcom$STATION, sep=".")) %>%
  group_by(tow_id) %>%
  mutate(fvcom_depth = mean(FVCOM_DEPTH),
         fvcom_temp = mean(FVCOM_TEMP),
         fvcom_salt = mean(FVCOM_SALT)) %>% 
  select(tow_id, fvcom_depth, AVGDEPTH.M., fvcom_temp, BOTTOM_TEMP, fvcom_salt, BEGIN_EST_TOWDATE, TOW_DURATION.min., NUMBER, YEAR, LAT, LON)
  

mass <- full_join(station_w_fvcom, lobster, by = c("tow_id", "YEAR","LAT", "LON")) %>% 
  mutate(BEGIN_EST_TOWDATE = dmy(BEGIN_EST_TOWDATE),
         month = month(BEGIN_EST_TOWDATE),
         day = day(BEGIN_EST_TOWDATE),
         doy = yday(BEGIN_EST_TOWDATE), 
         area_swept = ifelse(TOW_DURATION.min. < 20, 0.00065*20, 0.00065*TOW_DURATION.min.), #where did this tow duration calculation come from?
         survey = "MA",
         vessel = "MA",
         catch_n = replace_na(catch_n, 0),
         catch_n_m = replace_na(catch_n_m, 0),
         catch_n_f = replace_na(catch_n_f, 0),
         catch_n_a = replace_na(catch_n_a, 0),
         catch_n_j = replace_na(catch_n_j, 0)) %>% 
  select(tow_id, survey, year = YEAR, month, day, doy,	
         tow_duration = TOW_DURATION.min., lat = LAT, lon = LON,	
         catch_n , catch_n_f,	catch_n_m, catch_n_a,	catch_n_j,
         vessel, area_swept) %>% 
  as.data.frame()


################################
#add stat area
################################
latlon <- mass %>% 
  select(lat, lon)

coordinates(latlon) <- ~lon+lat
CRS.new <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")  #EPSG:102003
proj4string(latlon) <- CRS.new 
proj4string(statarea) <- CRS.new #i get a warning message here that a new CRS was assigned to an object with an existing CRS wtihout reprojecting

area <- over(latlon, statarea, returnList = FALSE) %>% 
  select(stat_area = Id)

ma <- cbind(area, mass)

qplot(ma$lon, ma$lat, color = as.character(ma$stat_area))
qplot(ma$lon, ma$lat, color = log(ma$catch_n))

write_csv(ma, "data/lobster_survey_data/MASS/MA.csv")

