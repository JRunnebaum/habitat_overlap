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
library(cowplot)
library(RColorBrewer)
library(ggthemes)

##########################
#set figure attributes
##########################
# windowsFonts(Times=windowsFont("Calibri"))
theme_sleek <- function(base_size = 12, base_family = "Calibri") {
  half_line <- base_size/2
  theme_light(base_size = 12, base_family = "Calibri") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "black"),
      strip.text.y = element_text(colour = "black"),
      # axis.text = element_text(colour = "grey30"),
      #axis.title = element_text(colour = "grey30"),
      #legend.title = element_text(colour = "grey30"),#, size = rel(0.9)
      legend.title = element_blank(),
      panel.border = element_rect(fill = NA),#, colour = "grey70", size = 1),
      legend.key.size = unit(0.9, "lines"),
      #legend.text = element_text(size = rel(0.7)),#, colour = "grey30"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA)#,
      #plot.title = element_text(colour = "grey30"),#, size = rel(1),
      #plot.subtitle = element_text(colour = "grey30")#, size = rel(.85),
      
      
    )
}
theme_set(theme_sleek())
options(scipen = 999) #turns off scientific notation


# Depends on dplyr
tickr <- function(
  data, # dataframe
  var, # column of interest
  to # break point definition 
){
  
  VAR <- enquo(var) # makes VAR a dynamic variable
  
  data %>% 
    distinct(!!VAR) %>%
    ungroup(!!VAR) %>% 
    mutate(labels = ifelse(!!VAR %in% seq(to * round(min(!!VAR) / to), max(!!VAR), to),
                           !!VAR, "")) %>%
    select(breaks = UQ(VAR), labels)
}

##########################
#plot all survey data
##########################

cusk_ll <- read.csv("data/cusk_data_for_VAST/NMFS_LL/NMFS_cusk_longline.csv")
spring_ll <- cusk_ll %>% 
  filter(month == 4 | month == 5 | month == 6)

fall_ll <- cusk_ll %>% 
  filter(month == 9 | month == 10 | month == 11)


lob_fall <- read.csv("data/lobster_survey_data/fall_lobster_all.csv") %>% 
  mutate(survey = dplyr::recode_factor(survey, "NMFS" = "NEFSC"))

lob_spring <- read.csv("data/lobster_survey_data/spring_lobster_all.csv") %>% 
  mutate(survey = dplyr::recode_factor(survey, "NMFS" = "NEFSC"))

coast <- readOGR(dsn='data/gshhg-shp-2.3.4/GSHHS_shp/i', layer='GSHHS_i_L1')
coast_df <- fortify(coast)

stat_area <- rgdal::readOGR("data/Statistical_Area/Statistical_Area.shp")
stat_area_df <- fortify(stat_area)

gom_gb <- subset(stat_area, Id == 464 | Id == 465 | Id == 466 | Id == 467 |
                   Id == 511 | Id == 512 | Id == 513 | Id == 514 | 
                   Id == 515 | Id == 521 | Id == 522 | Id == 525 | 
                   Id == 526 | Id == 551 | Id == 561 | Id == 552 |
                   Id == 562)


gom_gb_df <- fortify(gom_gb, region = "Id")

##########################
#color palete
##########################
#https://commfish.github.io/special_topics_20180906.html
# pal <- ggthemes::canva_pal("Warm and cool")(4)
# pal <- ggthemes::canva_pal("Crisp and Dramatic")(4)
# Some different options on canva.com
# pal <- ggthemes::canva_pal("Watery blue-greens")(4)
# pal <- ggthemes::canva_pal("Surf and turf")(4)
# pal <- ggthemes::canva_pal("Warm and cool")(4)
pal <- ggthemes::canva_pal("Pool party")(4)
# pal <- ggthemes::canva_pal("Trendy and metropolitan")(4)
# pal <- ggthemes::canva_pal("Fresh and bright")(4)

##########################
#plot lobster survey
##########################
idList <- unique(gom_gb$Id)
centroids.df <-  as.data.frame(coordinates(gom_gb))
names(centroids.df) <- c("long", "lat")
stat_area_id <- data.frame(id = idList,  centroids.df)

fall_plot <- ggplot() +
  geom_point(data = lob_fall, aes(lon, lat, color = survey)) +
  geom_point(data = fall_ll, aes(begin_lon, begin_lat, color = survey)) +
  geom_polygon(data = coast_df , aes(long, lat, group = group),
               fill = NA, colour = "black") + 
  coord_map(xlim = c(-71.5,-65),  ylim = c(39.5,45.5)) +
  geom_polygon(data = gom_gb_df, aes(long, lat, group = group),
               fill = NA, colour = "black") + 
  geom_text(data = stat_area_id, aes(label = id, x = long, y = lat), size = 5, fontface='bold') +
  theme(legend.text = element_text(size = 16))+
  scale_y_continuous(breaks = round(seq(39.5, 45.5, length.out = 10),0), 
                     labels = round(seq(39.5, 45.5, length.out = 10),0)) +
  xlab("Longitude") + ylab("Latitude") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16)) +
  ggtitle("Fall") +
  scale_colour_manual(values = colorRampPalette(pal)(n_distinct(diamonds$cut)))

spring_plot <- ggplot() +
  geom_point(data = lob_spring, aes(lon, lat, color = survey)) +
  geom_point(data = spring_ll, aes(begin_lon, begin_lat, color = survey)) +
  geom_polygon(data = coast_df , aes(long, lat, group = group),
               fill = NA, colour = "black") + 
  coord_map(xlim = c(-71.5,-65),  ylim = c(39.5,45.5)) +
  geom_polygon(data = gom_gb_df, aes(long, lat, group = group),
               fill = NA, colour = "black") + 
  geom_text(data = stat_area_id, aes(label = id, x = long, y = lat), size = 5, fontface='bold') +
  
  theme(legend.text = element_text(size = 16)) +
  scale_y_continuous(breaks = round(seq(39.5, 45.5, length.out = 10),0), 
                     labels = round(seq(39.5, 45.5, length.out = 10),0)) +
  xlab("Longitude") + ylab("Latitude") +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16)) +
  ggtitle("Spring") +
  scale_colour_manual(values = colorRampPalette(pal)(n_distinct(diamonds$cut)))


legend <- get_legend(fall_plot) 

both <- cowplot::plot_grid(spring_plot + theme(legend.position="none"),
                           fall_plot + theme(legend.position="none"), 
                           align = 'vh',
                           # labels = c("A", "B"),
                           hjust = -5,
                           nrow = 1)
                            
p <- plot_grid(both, legend, rel_widths = c(1.8, 0.3))
p
ggsave(plot = p, file = paste0("figures/both_surveys.png"), height = 10, width = 10)

 


