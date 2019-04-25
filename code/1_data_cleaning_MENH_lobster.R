###############################
#original author = Kisei Tanaka
#revised by = Jocelyn Runnebaum
#last revised = 2/20/2019
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
#included columns and order vary by file, have to load one at a time to reorder and select data 
#there are warnings when using dplyr to select columns
mn1 <- read_csv('data/lobster_survey_data/ME_NH/variable_years/MENHLobsIIFL15_16.csv', col_names = T) 
mn1 <- mn1[,c( "DMR_TRIP_IDENTIFIER", "DMR_EFFORT_IDENTIFIER", "EFFORT_START_DATE", "LENGTH_TOW_TIME", "START_LATITUDE", "START_LONGITUDE", "REPORTED_QUANTITY", "SAMPLE_LENGTH", "FREQUENCY", "SEX", "WATER_TEMP_C", "SALINITY", "TOW_LENGTH_NM")]
colnames(mn1) = c( "DMR_TRIP_IDENTIFIER", "DMR_EFFORT_IDENTIFIER", "EFFORT_START_DATE", "LENGTH_TOW_TIME", "START_LATITUDE", "START_LONGITUDE", "REPORTED_QUANTITY", "SAMPLE_LENGTH", "FREQUENCY", "SEX", "WATER_TEMP_C", "SALINITY", "TOW_LENGTH_NM")

mn2 <- read_csv('data/lobster_survey_data/ME_NH/variable_years/MENHLobsIISP15_16.csv', col_names = TRUE)
mn2 <- mn2[,c( "DMR_TRIP_IDENTIFIER", "DMR_EFFORT_IDENTIFIER", "EFFORT_START_DATE", "LENGTH_TOW_TIME", "START_LATITUDE", "START_LONGITUDE", "REPORTED_QUANTITY", "SAMPLE_LENGTH", "FREQUENCY", "SEX", "WATER_TEMP_C", "SALINITY", "TOW_LENGTH_NM")]
colnames(mn2) = c( "DMR_TRIP_IDENTIFIER", "DMR_EFFORT_IDENTIFIER", "EFFORT_START_DATE", "LENGTH_TOW_TIME", "START_LATITUDE", "START_LONGITUDE", "REPORTED_QUANTITY", "SAMPLE_LENGTH", "FREQUENCY", "SEX", "WATER_TEMP_C", "SALINITY", "TOW_LENGTH_NM")

mn3 <- read_csv('data/lobster_survey_data/ME_NH/variable_years/MENHLobsZFL00_05.csv', col_names = TRUE)
mn3 <- mn3[,c( "DMR_TRIP_IDENTIFIER", "DMR_EFFORT_IDENTIFIER", "EFFORT_START_DATE", "LENGTH_TOW_TIME", "START_LATITUDE", "START_LONGITUDE", "REPORTED_QUANTITY", "SAMPLE_LENGTH", "FREQUENCY", "SEX", "WATER_TEMP_C", "SALINITY", "TOW_LENGTH_NM")]
colnames(mn3) = c( "DMR_TRIP_IDENTIFIER", "DMR_EFFORT_IDENTIFIER", "EFFORT_START_DATE", "LENGTH_TOW_TIME", "START_LATITUDE", "START_LONGITUDE", "REPORTED_QUANTITY", "SAMPLE_LENGTH", "FREQUENCY", "SEX", "WATER_TEMP_C", "SALINITY", "TOW_LENGTH_NM")

mn4 <- read_csv('data/lobster_survey_data/ME_NH/variable_years/MENHLobsZFL06_10.csv', col_names = TRUE)
mn4 <- mn4[,c( "DMR_TRIP_IDENTIFIER", "DMR_EFFORT_IDENTIFIER", "EFFORT_START_DATE", "LENGTH_TOW_TIME", "START_LATITUDE", "START_LONGITUDE", "REPORTED_QUANTITY", "SAMPLE_LENGTH", "FREQUENCY", "SEX", "WATER_TEMP_C", "SALINITY", "TOW_LENGTH_NM")]
colnames(mn4) = c( "DMR_TRIP_IDENTIFIER", "DMR_EFFORT_IDENTIFIER", "EFFORT_START_DATE", "LENGTH_TOW_TIME", "START_LATITUDE", "START_LONGITUDE", "REPORTED_QUANTITY", "SAMPLE_LENGTH", "FREQUENCY", "SEX", "WATER_TEMP_C", "SALINITY", "TOW_LENGTH_NM")

mn5 <- read_csv('data/lobster_survey_data/ME_NH/variable_years/MENHLobsZFL11_14.csv', col_names = TRUE)
mn5 <- mn5[,c( "DMR_TRIP_IDENTIFIER", "DMR_EFFORT_IDENTIFIER", "EFFORT_START_DATE", "LENGTH_TOW_TIME", "START_LATITUDE", "START_LONGITUDE", "REPORTED_QUANTITY", "SAMPLE_LENGTH", "FREQUENCY", "SEX", "WATER_TEMP_C", "SALINITY", "TOW_LENGTH_NM")]
colnames(mn5) = c( "DMR_TRIP_IDENTIFIER", "DMR_EFFORT_IDENTIFIER", "EFFORT_START_DATE", "LENGTH_TOW_TIME", "START_LATITUDE", "START_LONGITUDE", "REPORTED_QUANTITY", "SAMPLE_LENGTH", "FREQUENCY", "SEX", "WATER_TEMP_C", "SALINITY", "TOW_LENGTH_NM")

mn6 <- read_csv('data/lobster_survey_data/ME_NH/variable_years/MENHLobsZSP01_05.csv', col_names = TRUE)
mn6 <- mn6[,c( "DMR_TRIP_IDENTIFIER", "DMR_EFFORT_IDENTIFIER", "EFFORT_START_DATE", "LENGTH_TOW_TIME", "START_LATITUDE", "START_LONGITUDE", "REPORTED_QUANTITY", "SAMPLE_LENGTH", "FREQUENCY", "SEX", "WATER_TEMP_C", "SALINITY", "TOW_LENGTH_NM")]
colnames(mn6) = c( "DMR_TRIP_IDENTIFIER", "DMR_EFFORT_IDENTIFIER", "EFFORT_START_DATE", "LENGTH_TOW_TIME", "START_LATITUDE", "START_LONGITUDE", "REPORTED_QUANTITY", "SAMPLE_LENGTH", "FREQUENCY", "SEX", "WATER_TEMP_C", "SALINITY", "TOW_LENGTH_NM")

mn7 <- read_csv('data/lobster_survey_data/ME_NH/variable_years/MENHLobsZSP06_10.csv', col_names = TRUE)
mn7 <- mn7[,c( "DMR_TRIP_IDENTIFIER", "DMR_EFFORT_IDENTIFIER", "EFFORT_START_DATE", "LENGTH_TOW_TIME", "START_LATITUDE", "START_LONGITUDE", "REPORTED_QUANTITY", "SAMPLE_LENGTH", "FREQUENCY", "SEX", "WATER_TEMP_C", "SALINITY", "TOW_LENGTH_NM")]
colnames(mn7) = c( "DMR_TRIP_IDENTIFIER", "DMR_EFFORT_IDENTIFIER", "EFFORT_START_DATE", "LENGTH_TOW_TIME", "START_LATITUDE", "START_LONGITUDE", "REPORTED_QUANTITY", "SAMPLE_LENGTH", "FREQUENCY", "SEX", "WATER_TEMP_C", "SALINITY", "TOW_LENGTH_NM")

mn8 <- read_csv('data/lobster_survey_data/ME_NH/variable_years/MENHLobsZSP11_14.csv', col_names = TRUE)
mn8 <- mn8[,c( "DMR_TRIP_IDENTIFIER", "DMR_EFFORT_IDENTIFIER", "EFFORT_START_DATE", "LENGTH_TOW_TIME", "START_LATITUDE", "START_LONGITUDE", "REPORTED_QUANTITY", "SAMPLE_LENGTH", "FREQUENCY", "SEX", "WATER_TEMP_C", "SALINITY", "TOW_LENGTH_NM")]
colnames(mn8) = c( "DMR_TRIP_IDENTIFIER", "DMR_EFFORT_IDENTIFIER", "EFFORT_START_DATE", "LENGTH_TOW_TIME", "START_LATITUDE", "START_LONGITUDE", "REPORTED_QUANTITY", "SAMPLE_LENGTH", "FREQUENCY", "SEX", "WATER_TEMP_C", "SALINITY", "TOW_LENGTH_NM")

mn <- rbind(mn1, mn2, mn3, mn4, mn5, mn6, mn7, mn8)

mn <- as.data.frame(mn)

statarea <- rgdal::readOGR("data/Statistical_Area/Statistical_Area.shp")

rm(mn1, mn2, mn3, mn4, mn5, mn6, mn7, mn8)

#########################################
#add the necessary columns
#########################################
lobster <- mn %>% 
  mutate(EFFORT_START_DATE = as.Date(EFFORT_START_DATE, format = '%m/%d/%Y'),
         month = month(EFFORT_START_DATE),
         day = day(EFFORT_START_DATE),
         year = year(EFFORT_START_DATE),
         doy = yday(EFFORT_START_DATE),
         tow_id = paste(DMR_TRIP_IDENTIFIER, DMR_EFFORT_IDENTIFIER, sep="."),
         SAMPLE_LENGTH = replace(SAMPLE_LENGTH, SAMPLE_LENGTH == -99, NA),
         SAMPLE_LENGTH = replace(SAMPLE_LENGTH, SAMPLE_LENGTH == 999, NA),
         SEX = case_when(SEX == "M/F"~ "NA",
                         SEX == "Unknown" ~ "NA",
                         SEX == "Gynandromorph" ~ "NA",
                         SEX == "Female w/Eggs" ~ "Female",
                         SEX == "Female" ~ "Female",
                         SEX == "Male" ~ "Male")) %>% 
  filter(!is.na(SAMPLE_LENGTH),
         !is.na(SEX)) %>% 
  mutate(catch_n_m = ifelse(SEX == "Male", FREQUENCY, 0),
         catch_n_f = ifelse(SEX == "Female", FREQUENCY, 0),
         catch_n_a = ifelse(SAMPLE_LENGTH > 60, FREQUENCY, 0),#THERE IS SOMETHING WRONG HERE OR ON THE NEXT LINE
         catch_n_j = ifelse(c(SAMPLE_LENGTH <= 60 & SAMPLE_LENGTH > 0), FREQUENCY, 0)) %>% 
  group_by(tow_id, START_LATITUDE, START_LONGITUDE, year) %>% 
  summarise(catch_n = sum(FREQUENCY),
            catch_n_m = sum(catch_n_m),
            catch_n_f = sum(catch_n_f),
            catch_n_a = sum(catch_n_a),
            catch_n_j = sum(catch_n_j))

station <- mn %>% 
  mutate(EFFORT_START_DATE = as.Date(EFFORT_START_DATE, format = '%m/%d/%Y'),
        month = month(EFFORT_START_DATE),
        day = day(EFFORT_START_DATE),
        year = year(EFFORT_START_DATE),
        doy = yday(EFFORT_START_DATE),
        tow_id = paste(DMR_TRIP_IDENTIFIER, DMR_EFFORT_IDENTIFIER, sep="."),
        duration = as.numeric(LENGTH_TOW_TIME)/60,
        area_swept = duration*TOW_LENGTH_NM,
        survey = "ME",
        vessel = "ME")
        
me <-full_join(lobster, station, by = c("tow_id", "START_LATITUDE", "START_LONGITUDE", "year")) %>% 
  mutate(catch_n = replace_na(catch_n, 0),
         catch_n_m = replace_na(catch_n_m, 0),
         catch_n_f = replace_na(catch_n_f, 0),
         catch_n_a = replace_na(catch_n_a, 0),
         catch_n_j = replace_na(catch_n_j, 0)) %>% 
  select(tow_id, survey, year, month, day, doy,	
         tow_duration = duration, lat = START_LATITUDE, lon = START_LONGITUDE,	
         catch_n , catch_n_f,	catch_n_m, catch_n_a,	catch_n_j,
         vessel, area_swept) %>% 
  distinct() %>% 
  as.data.frame()

################################
#add stat area
################################
latlon <- me %>% 
  select(lat, lon)

coordinates(latlon) <- ~lon + lat
CRS.new <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")  #EPSG:102003
proj4string(latlon) <- CRS.new 
proj4string(statarea) <- CRS.new #i get a warning message here that a new CRS was assigned to an object with an existing CRS wtihout reprojecting

plot(statarea)
points(latlon$lat~latlon$lon)

area <- over(latlon, statarea, returnList = FALSE) %>% 
  select(stat_area = Id)

me_nh <- cbind(area, me) %>% 
  as.data.frame()

qplot(me_nh$lon, me_nh$lat, color = as.character(me_nh$stat_area))
qplot(me_nh$lon, me_nh$lat, color = log(me_nh$catch_n))

write_csv(me_nh, "data/lobster_survey_data/ME_NH/ME.csv")


