###############################################################################
#project: TERN gps data
#2. map raw track data for each bird
#date: Dec 16, 2023
#author: Ariel Lenske
###############################################################################

#packages
library(tidyverse)
library(ggplot2)
library(sf)
library(viridis)
library(patchwork)
library(lubridate)

#source functions
source("scripts from Ariel Lenske/functions/set_ggplot_custom_theme.R")
source("scripts from Ariel Lenske/functions/outputs_loc.R")
source("scripts from Ariel Lenske/functions/localgd_loc.R")
source("scripts from Ariel Lenske/functions/latLonDist.R")

#set data output base path to the projects google drive output folder
#outputbasepath <- outputs_loc("TERNgps_outputs")

#set path to map_data folder
#mapdatapath <- localgd_loc("map_data")

#load gps location data and deployment info
gps <- readRDS("data_clean/tern_gps_data_no_tripID.rds")
deploys <- readRDS("data_clean/tern_gps_metadata.rds")

names(gps)
names(deploys)

#check num of unique birds, tags, and deployments
length(unique(deploys$metalBand)) #53
length(unique(deploys$tag_id)) #36
length(unique(deploys$deployID)) #53

#check deploy num of unique deploy locations
length(unique(deploys$location_lat)) #1
length(unique(deploys$location_lon)) #1
length(unique(deploys$location)) #1

#list of deployment ids
dlist <- deploys %>% dplyr::select(BirdID) %>% distinct() %>% pull()

#setup basic things for map 
#land <- st_read(file.path(mapdatapath, "land_polygon", "ne_10m_land.shp")) 

#add distance to colony in km for each fix
gps <- left_join(gps, 
                 deploys %>% dplyr::select(BirdID, location_lat, location_lon)) %>%
  mutate(colonyDist = latLonDist(location_lat, location_lon, lat, lon)/1000) 

##note: 2 birds have no gps data (CT82572, CT82570)
#remove those birds from dlist
dlist <- deploys %>% 
  dplyr::select(BirdID) %>% 
  dplyr::filter(!BirdID %in% c("CT82572", "CT82570")) %>% 
  distinct() %>% 
  pull()

#plot each bird separately
for(i in 1:length(dlist)){
  
  bird <- gps %>% filter(BirdID == dlist[i]) 
  dep <- deploys %>% filter(BirdID == dlist[i]) 
  
  stime <- dep$deployTime - hours(4)
  etime <- max(bird$ts) + hours(4)
  
  lims <- c(stime, etime)
  
  xmin <- min(min(bird$lon, na.rm = TRUE), min(dep$location_lon, na.rm = TRUE)) - 0.1
  xmax <- max(max(bird$lon, na.rm = TRUE), max(dep$location_lon, na.rm = TRUE)) + 0.1
  ymin <- min(min(bird$lat, na.rm = TRUE), min(dep$location_lat, na.rm = TRUE)) - 0.1
  ymax <- max(max(bird$lat, na.rm = TRUE), max(dep$location_lat, na.rm = TRUE)) + 0.1
  
  
  #map; I removed Ariel's code for eadding the land basemap
  p1 <- ggplot() +
    #geom_sf(color = "black", fill = "grey") +
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
    geom_point(data = bird,
               aes(lon, lat, color = ts), alpha = 0.8, size = 1) +
    geom_path(data = bird,
              aes(lon, lat, color = ts), alpha = 0.8, linewidth = 0.75) +
    geom_point(data = dep,
               aes(location_lon, location_lat), color = "red", size = 2) +
    scale_color_gradientn(name = "timestamp",
                          colors = viridis(40, direction = -1, begin = 0, end = 1),
                          trans = "time") +
    theme(legend.position = "top") +
    guides(colour = guide_colourbar(barwidth=25)) +
    ylab("Latitude") + xlab("Longitude") +
    ggtitle(paste0(dep$species, " tag",
                   dep$tag_id, " band", dep$metalBand,
                   " year:", dep$deployYear))
  
  #dist to colony
  p2 <- ggplot(data = bird, aes(x = ts, y = colonyDist, color = ts)) +
    geom_point() +
    geom_path() +
    scale_color_gradientn(name = "timestamp",
                          colors = viridis(40, direction = -1, begin = 0, end = 1),
                          trans = "time") +
    scale_x_datetime(date_breaks = "12 hour",
                     date_minor_breaks = "2 hour",
                     date_labels = "%d-%b",
                     limits = lims,
                     timezone = "America/Curacao",
                     expand = c(0, 0)) +
    geom_point(data = dep, aes(x = retrievalTime, y = 0), color = "red", size = 2) +
    geom_point(data = dep, aes(x = deployTime, y = 0), color = "red", size = 2) +
    geom_hline(yintercept = 0, color = "red") +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    theme(legend.position = "none",
          panel.grid.minor.x = element_line(colour="lightgrey", linewidth = 0.5),
          panel.grid.major.x = element_line(colour="lightgrey", linewidth = 0.5)) +
    guides(colour = guide_colourbar(barwidth=25)) +
    ylab("Distance (km)") + xlab("Time")
  
  
  png(filename = paste0("figs/diagnostic-individual-bird-plots/", dep$species,"-tag",
                                  dep$tag_id, "-", 
                                  dep$deployYear, ".png"),
      width=10, height=9, units="in", res=300,
      type = "cairo") 
  
  print(p1/p2 +
          plot_layout(heights = c(2, 1)))
  
  dev.off()
  
  
}
rm(i)



