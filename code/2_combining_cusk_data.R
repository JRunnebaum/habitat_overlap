###############################
#author = Jocelyn Runnebaum
#last revised = 2/28/2019
################################

############################
#load libraries
############################
library(dplyr)
library(sp)
library(maptools)
library(rgdal)
library(gstat)
library(maps)
library(fields)
library(lattice)
library(PBSmapping)

############################
#load data
############################

#number of lobster not converted with conversion coefficients (estimate differences in catchability before and after survey change in VAST)
#use uncalibrated data to test differences in catchability estimates in VAST (different paper than this)
# nmfs<-read.csv("data/lobster_survey_data/NMFS/NEFSC_uncalibrated_w_strata.csv") 
bts <- read.csv("data/cusk_data_for_VAST/NMFS_cusk_bts.csv") #used calibrated data for overlap
ll <- read.csv("data/cusk_data_for_VAST/NMFS_cusk_longline.csv")


summary(bts$catch_n)
summary(ll$catch_n)


############################
# NMFS bts data
#add c
############################

bts <- bts %>% 
    mutate(survey = case_when(vessel == "AL" ~ "BTS", #assumes that catches were standarized to albatoross by JR
                     vessel == "DE" ~ "BTS",
                     vessel == "HB" ~ "BTS2")) 
 

############################
# NMFS longline data
#reduce columns to area swept used in VAST
############################
ll <- ll %>% 
  dplyr::select(stat_area, tow_id, survey, year, month, day, doy,  tow_duration,
         lat = begin_lat, lon = begin_lon, catch_n, vessel, area_swept = AREA_SWEPT)

############################
#combine surveys
############################

all <- rbind(bts, ll) %>% 
    filter(stat_area == 464 | stat_area == 465 | stat_area == 466 | stat_area == 467 |
           stat_area == 511 | stat_area == 512 | stat_area == 513 | stat_area == 514 | 
           stat_area == 515 | stat_area == 521 | stat_area == 522 | stat_area == 525 | 
           stat_area == 526 | stat_area == 551 | stat_area == 561 | stat_area == 552 |
           stat_area == 562)

############################
#subset data for seasons
#using Northern Meteorological Seasons
#spring = april, may, june
#fall is defined as september, october, november
#not all surveys have data in all these months
############################

table(all$month)

spring <- all %>% 
  filter(month == 4 | month == 5 | month == 6)
  
fall <- all %>% 
  filter(month == 9 | month == 10 | month == 11)


##########################
# save data
##########################
write.csv(spring, file = "data/cusk_data_for_VAST/spring_cusk_all.csv")

write.csv(fall, file = "data/cusk_data_for_VAST/fall_cusk_all.csv")

write.csv(all, file = "data/cusk_data_for_VAST/cusk_all.csv" )


