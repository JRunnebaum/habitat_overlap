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

length_a <- read.csv ('data/lobster_survey_data/NMFS/length1.csv', header = TRUE) #2016 data only with different stations that in sheet 1
length_b <- read.csv ('data/lobster_survey_data/NMFS/length2.csv', header = TRUE) #1964 to 2016
conv <-  read.csv("data/lobster_survey_data/NMFS/AlbatrossBigelowConv.csv", header = T)
station <- read.csv ('data/lobster_survey_data/NMFS/Lobster_Spring_Fall_BTS_Station_Catch_with_zero_tows.csv', header = TRUE) #with zero tows
statarea <- rgdal::readOGR("data/Statistical_Area/Statistical_Area.shp")

################################
#tidy data
################################
length <- rbind(length_a, length_b) %>% 
  mutate(tow_id = paste(CRUISE, STATION, STRATUM, TOW, sep = ".")) %>% 
  filter(YEAR >= 1980,
         CATCHSEX == "Female" | CATCHSEX == "Male") %>% 
  droplevels()

conv <- conv %>% 
  select(LENGTH_CM, rho, cv)

station <- station %>% 
  mutate(tow_id = paste(cruise, station, stratum, tow, sep = ".")) %>% 
  filter(year >= 1980) %>% 
droplevels()

#####################################
#Apply Conversion Coefficient
#there is the potential that the conversion coefficient hasn't 
#been applied to thses data to account for catch differences
#related to survey changes needs to be applied to these data
#####################################

length <- left_join(length, conv) %>% 
  mutate(conv_expanded_num_at_length = ifelse(VESSEL == "HB", 
                                            EXPANDED_NUMBER_AT_LENGTH/rho, 
                                            EXPANDED_NUMBER_AT_LENGTH)) 
  

# length = length[which(length$LENGTH_CM > 5.2),]  #why would i do this? it would remove juveniles

#plot converted and unconverted data and plot together to see the difference
#********************BASED ON THIS PLOT I WOULD GUESS THEY SHOULDN'T BE CONVERTED???*********************
plot(EXPANDED_NUMBER_AT_LENGTH~LENGTH_CM, data = length, pch = 16, cex = 0.5, ylim = c(0,45))
points(conv_expanded_num_at_length~LENGTH_CM, data = length, pch = 16, cex = 0.5, col = 2)

################################
#create different catch categories
################################
length <- length %>% 
  mutate(EXPANDED_NUMBER_AT_LENGTH = as.numeric(EXPANDED_NUMBER_AT_LENGTH)) %>% 
  mutate(catch_n_m = ifelse(CATCHSEX == "Male", EXPANDED_NUMBER_AT_LENGTH, 0),
         catch_n_f = ifelse(CATCHSEX == "Female", EXPANDED_NUMBER_AT_LENGTH, 0),
         catch_n_a = ifelse(LENGTH_CM > 6.0, EXPANDED_NUMBER_AT_LENGTH, 0),#THERE IS SOMETHING WRONG HERE OR ON THE NEXT LINE
         catch_n_j = ifelse(c(LENGTH_CM <= 6.0 & LENGTH_CM > 0), EXPANDED_NUMBER_AT_LENGTH, 0)) %>% 
  group_by(tow_id,YEAR) %>% 
  summarise(catch_n = sum(EXPANDED_NUMBER_AT_LENGTH),
            catch_n_m = sum(catch_n_m),
            catch_n_f = sum(catch_n_f),
            catch_n_a = sum(catch_n_a),
            catch_n_j = sum(catch_n_j)) 

 
#############################################
#combine lobster length data with station data
##############################################

nmfs <- full_join(station, length, by = "tow_id") %>% 
  mutate(begin_date = as.Date(begin_date, format = '%m/%d/%Y'),
         month = month(begin_date),
         day = day(begin_date),
         doy = yday(begin_date)) %>% 
  filter(operation_status == "Representative") %>% 
  mutate(area_swept = case_when(vessel == "AL" ~ 0.038, #assumes that catches were standarized to albatoross by JR
                                vessel == "DE" ~ 0.038,
                                vessel == "HB" ~ 0.024),
         survey = "NMFS",
         catch_n = replace_na(catch_n, 0),
         catch_n_m = replace_na(catch_n_m, 0),
         catch_n_f = replace_na(catch_n_f, 0),
         catch_n_a = replace_na(catch_n_a, 0),
         catch_n_j = replace_na(catch_n_j, 0))  %>% 
  select(tow_id, survey, year, month, day, doy,	
         tow_duration, lat = decdeg_beglat, lon = decdeg_beglon,	
         catch_n , catch_n_f,	catch_n_m, catch_n_a,	catch_n_j,
         vessel, area_swept) %>% 
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
  as.data.frame()

qplot(nefsc$lon, nefsc$lat, color = as.character(nefsc$stat_area))
qplot(nefsc$lon, nefsc$lat, color = log(nefsc$catch_n))

write_csv(nefsc, "data/lobster_survey_data/NMFS/NMFS.csv")


