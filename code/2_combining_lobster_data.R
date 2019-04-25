###############################
#author = Jocelyn Runnebaum
#last revised = 2/24/2019
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
library(ggplot2)
############################
#load data
############################

#number of lobster not converted with conversion coefficients (estimate differences in catchability before and after survey change in VAST)
#use uncalibrated data to test differences in catchability estimates in VAST (different paper than this)
# nmfs<-read.csv("data/lobster_survey_data/NMFS/NEFSC_uncalibrated_w_strata.csv") 
nmfs <- read.csv("data/lobster_survey_data/NMFS/NMFS.csv") #used calibrated data for overlap
me <- read.csv("data/lobster_survey_data/ME_NH/ME.csv")
mass <- read.csv("data/lobster_survey_data/MASS/MA.csv")
coast <- readOGR(dsn='data/gshhg-shp-2.3.4/GSHHS_shp/i', layer='GSHHS_i_L1')

summary(nmfs$catch_n)
summary(me$catch_n)
summary(mass$catch_n)

############################
# NMFS data
#reduce NMFS data to lobster GOM and GB stock units
############################


  
############################
#combine surveys
############################

all <- rbind(me, mass, nmfs)

all <- all %>% 
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
write.csv(spring, file = "data/lobster_survey_data/spring_lobster_all.csv")

write.csv(fall, file = "data/lobster_survey_data/fall_lobster_all.csv")

write.csv(all, file = "data/lobster_survey_data/lobster_all.csv" )

  
