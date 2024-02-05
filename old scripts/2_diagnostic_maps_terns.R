###############################################################################
#ARTE gps data - diagnostic maps
#2023-12-13 code from Ariel Lenske
###############################################################################

#packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(sf)
library(viridis)
library(ggpubr)


#load data
gpslocs <- readRDS("data_clean/arte_gps_data_no_tripID.rds")

#list of deployIDs
blist <- gpslocs %>% select(deployID) %>% distinct() %>% pull()

#setup basic things for map 
east_shr

#plot each bird separately
for(i in 1:length(blist)){
  
  bird <- gpslocs %>% filter(deployID == blist[i]) 
  
  sdate <- as.character(as.Date(bird$deployTime[1]))
  edate <- as.character(as.Date(bird$retrievalTime[1]) + 1)
  
  stime <- as.POSIXct(paste0(sdate, " 00:00:00"))
  etime <- as.POSIXct(paste0(edate, " 00:00:00"))
  
  lims <- c(stime, etime)
  
  xmin <- min(min(bird$lon, na.rm = TRUE), min(bird$location_lon, na.rm = TRUE)) - 0.1
  xmax <- max(max(bird$lon, na.rm = TRUE), max(bird$location_lon, na.rm = TRUE)) + 0.1
  ymin <- min(min(bird$lat, na.rm = TRUE), min(bird$location_lat, na.rm = TRUE)) - 0.1
  ymax <- max(max(bird$lat, na.rm = TRUE), max(bird$location_lat, na.rm = TRUE)) + 0.1
  
  
  #map
  p1 <- ggplot() +
    #geom_polygon(east_shr) +
    #geom_sf(fill = "grey90", color = "grey90") +
    #geom_sf(data = bccoast, color = "black") +
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
    geom_point(data = bird,
               aes(lon, lat, color = ts), alpha = 0.8, size = 1) +
    geom_path(data = bird,
              aes(lon, lat, color = ts), alpha = 0.8, size = 0.75) +
    geom_point(data = bird,
               aes(location_lon, location_lat), color = "red", size = 2) +
    scale_color_gradientn(name = "timestamp",
                          colors = viridis(40, direction = -1, begin = 0, end = 1),
                          trans = "time") +
    theme(legend.position = "top") +
    guides(colour = guide_colourbar(barwidth=25)) +
    ylab("Latitude") + xlab("Longitude") +
    ggtitle(paste0(bird$Species[1], " ", bird$location,": tag",
                   bird$Tag[1], " band", bird$metalBand[1],
                   " year:", bird$deployYear))
  
  #lat
  p2 <- ggplot(data = bird, aes(x = ts, y = lat, color = ts)) +
    geom_point() +
    geom_path() +
    geom_point(aes(x = retrievalTime, y = location_lat), color = "red", size = 2) +
    geom_point(aes(x = deployTime, y = location_lat), color = "red", size = 2) +
    scale_color_gradientn(name = "timestamp",
                          colors = viridis(40, direction = -1, begin = 0, end = 1),
                          trans = "time") +
    scale_x_datetime(date_breaks = "12 hour",
                     date_minor_breaks = "2 hour",
                     date_labels = "%d-%b",
                     limits = lims,
                     timezone = "America/Curacao",
                     expand = c(0, 0)) +
    geom_hline(yintercept = bird$location_lat[1], color = "red") +
    theme(legend.position = "none",
          panel.grid.minor.x = element_line(colour="lightgrey", size=0.5),
          panel.grid.major.x = element_line(colour="lightgrey", size=0.5)) +
    guides(colour = guide_colourbar(barwidth=25)) +
    ylab("Latitude") + xlab("Time")
  
  #lon
  p3 <- ggplot(data = bird, aes(x = ts, y = lon, color = ts)) +
    geom_point() +
    geom_path() +
    geom_point(aes(x = retrievalTime, y = location_lon), color = "red", size = 2) +
    geom_point(aes(x = deployTime, y = location_lon), color = "red", size = 2) +
    scale_color_gradientn(name = "timestamp",
                          colors = viridis(40, direction = -1, begin = 0, end = 1),
                          trans = "time") +
    scale_x_datetime(date_breaks = "12 hour",
                     date_minor_breaks = "2 hour",
                     date_labels = "%d-%b",
                     limits = lims,
                     timezone = "America/Vancouver",
                     expand = c(0, 0)) +
    geom_hline(yintercept = bird$location_lon[1], color = "red") +
    theme(legend.position = "none",
          panel.grid.minor.x = element_line(colour="lightgrey", size=0.5),
          panel.grid.major.x = element_line(colour="lightgrey", size=0.5)) +
    guides(colour = guide_colourbar(barwidth=25)) +
    ylab("Longitude") + xlab("Time") 
  
  
  png(filename = paste0("figs/diagnostic-individual-bird-plots/",
                        bird$species[1], "-", bird$site,"-tag",
                        bird$tagID[1], "-band", bird$metalBand[1], "-",
                        bird$deployNestStage[1], "_", bird$deployYear, ".png"),
      width=9, height=9, units="in", res=300) 
  
  print(ggarrange(p1, p2, p3,                                                 
                  ncol = 1,
                  heights = c(2, 0.4, 0.4)))
  
  dev.off()
  
}




