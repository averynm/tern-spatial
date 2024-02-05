###############################################################################
#project: TERN gps data
#3. identify and define trips within deployments
#date: Dec 19, 2023
#author: Ariel Lenske
###############################################################################

#packages
library(tidyverse)
library(lubridate)
library(viridis)
library(suncalc)
library(sf)
library(patchwork)

#source functions
source("scripts from Ariel Lenske/functions/outputs_loc_ANM.R")
source("scripts from Ariel Lenske/functions/localgd_loc.R")
source("scripts from Ariel Lenske/functions/latLonDist.R")
source("scripts from Ariel Lenske/functions/set_ggplot_custom_theme.R")

#set data output base path to the projects google drive output folder
outputbasepath <- outputs_loc("Trip split outputs")
                                  
#set path to map_data folder
#mapdatapath <- localgd_loc("map_data")

#1.0 load gps location data and deployment info####
gps <- readRDS("scripts from Ariel Lenske/data_working/tern_gps_data_no_tripID.rds")
deploys <- readRDS("data_clean/tern_gps_metadata.rds")

names(gps)
names(deploys)

#2.0 make list of deployment ids####
##note: 2 birds have no gps data (CT82572, CT82570)
#remove those birds from dlist
dlist <- deploys %>% 
  dplyr::select(BirdID) %>% 
  dplyr::filter(!BirdID %in% c("CT82572", "CT82570")) %>% 
  distinct() %>% 
  pull()


#3.0 make colony df####
colony <- data.frame(studySite = deploys$location,
                     colonyLon = deploys$location_lon,
                     colonyLat = deploys$location_lat) %>%
  distinct()


#4.0. add distance (in meters) to colony for all fixes####
gps <- gps %>%
  mutate(colonyDist = round(latLonDist(colony$colonyLat, colony$colonyLon, lat, lon))) 


#5.0 look at distribution of gaps between fixes (fixLenth) 

#5.1 calculate the relevant info
sc <- gps %>%
  group_by(BirdID) %>%
  arrange(ts) %>%
  mutate(tslag = lag(ts),
         fixLength = round(as.numeric(difftime(ts, tslag, units = "mins")))) %>% 
  ungroup() %>%
  dplyr::select(BirdID, ts, colonyDist, fixLength, Species) %>%
  dplyr::filter(!is.na(fixLength))

#5.2 get fixLength summary stats
sc %>%
  summarise(min = min(fixLength, na.rm = TRUE), #2 min
            max = max(fixLength, na.rm = TRUE), #45 hours
            mean = mean(fixLength, na.rm = TRUE)) #8 mins

#5.3 plot dist of fixLengths (before removing long gaps)
brx <- seq(0, max(sc$fixLength), by = 100)

fix_dist <- ggplot() +
  geom_histogram(data = sc,
                 aes(fixLength, fill = Species),
                 color = "black", breaks = brx) +
  scale_x_continuous(breaks = round(seq(0, max(sc$fixLength), by = 100), 0)) +
  scale_fill_viridis_d(begin = 0.1, end = 0.7) +
  xlab("time between fixes in mins") +
  facet_wrap(~Species, ncol = 1)

plot(fix_dist)
#5.4 plot dist of fixLengths (after removing long gaps)
#filter out obs with fixLength > 20 mins
sc <- sc %>%
  dplyr::filter(fixLength <= 20)
#plot
brx <- seq(0, max(sc$fixLength), by = 1)

ggplot() +
  geom_histogram(data = sc,
                 aes(fixLength, fill = Species),
                 color = "black", breaks = brx) +
  scale_x_continuous(breaks = round(seq(0, max(sc$fixLength), by = 1), 0)) +
  scale_fill_viridis_d(begin = 0.1, end = 0.7) +
  xlab("time between fixes in mins") +
  facet_wrap(~Species, ncol = 1)

#6.0 plot dist of colonyDist for fix locations within 5 km of the colony####
#filter dataset
gps5000 <- gps %>% dplyr::filter(colonyDist <= 5000)

#plot
brx <- seq(0, max(gps5000$colonyDist), by = 100)

ggplot() +
  geom_histogram(data = gps5000,
                 aes(colonyDist, fill = Species),
                 color = "black", breaks = brx) +
  scale_x_continuous(breaks = round(seq(0, max(gps5000$colonyDist), by = 100), 0)) +
  scale_fill_viridis_d(begin = 0.1, end = 0.7) +
  geom_vline(xintercept = 500, linetype = "dashed") +
  xlab("distance to the colony (m)") +
  facet_wrap(~Species, ncol = 1)


#7.0 get trip start and end fixes####
# a) classify locations within 500 m of the colony as 'colony' = 1 and everything else as colony = 0
# b)use lead & lag of 'colonyDist' column to identify trip start and end fixes:
# - tripStart fix => the last location classified as at the colony (within a 500m buffer) before a 
#   group of fixes that are classified as away from the colony
# - tripEnd fix => the first location classified as at the colony (within a 500m buffer) after a 
#   group of fixes that are classified as away from the colony
# - if a fix is identified as both a tripStart and tripEnd, then it is re-classified as part of
#   the trips on either side of it (this makes the assumption that the bird didn't actually go to
#   the colony/was there very briefly since only one fix was recorded within 500m of the colony)

gps <- gps %>%
  mutate(colony = ifelse(colonyDist <= 500, 1, 0), #add column classifying each fix as within 500m of the colony or not
         leadcolony = lead(colony),
         lagcolony = lag(colony),
         tripCat = ifelse(colony == 1 & lagcolony == 0 & leadcolony == 0, "tripMiddle", #add trip classification 
                          ifelse(colony == 1 & lagcolony == 0, "tripEnd", 
                                 ifelse(colony == 1 & leadcolony == 0, "tripStart", 
                                        ifelse(colony == 0 , "tripMiddle", NA))))) %>%
  dplyr::filter(!is.na(tripCat)) %>% #remove fixes from between trips
  dplyr::select(-leadcolony, -lagcolony, -colony) #remove extra columns



#9.0 add trip IDs based on trip start and end fixes identifed above

#9.1 trip ID function
add_tripID <- function(bird){
  
  bird <- bird[with(bird, order(ts)), ]
  
  tripCat <- bird$tripCat
  tripID <- rep(0, nrow(bird))
  tripID[1] <- 1
  
  for(i in 2:nrow(bird)){
    
    if(tripCat[i] == "tripStart" & tripCat[i-1] == "tripStart"){
      
      tripID[i] <- tripID[i-1] + 1
      
    } else if( tripCat[i] == "tripEnd" & tripCat[i-1] == "tripEnd"){
      
      tripID[i] <- tripID[i-1]
      
    } else if(tripCat[i] == "tripMiddle" & tripCat[i-1] == "tripStart" |
              tripCat[i] == "tripMiddle" & tripCat[i-1] == "tripMiddle" |
              tripCat[i] == "tripEnd" & tripCat[i-1] == "tripMiddle"){
      
      tripID[i] <- tripID[i-1]
      
      
    } else if(tripCat[i] == "tripStart" & tripCat[i-1] == "tripMiddle" |
              tripCat[i] == "tripMiddle" & tripCat[i-1] == "tripEnd" |
              tripCat[i] == "tripStart" & tripCat[i-1] == "tripEnd" ){
      
      tripID[i] <- tripID[i-1] + 1
    }
    
  }
  
  bird <- bird %>% mutate(tripID = tripID)
  
}

#9.2 add tripIDs using above function
gps <- gps %>%
  group_by(BirdID) %>%
  do(add_tripID(.)) %>%
  ungroup() 

#9.3 add fixLength column and remove any trips with fix any gaps lasting more than 20 minutes
gps <- gps %>%
  group_by(BirdID, tripID) %>%
  arrange(ts) %>%
  mutate(tslag = lag(ts),
         fixLength = round(as.numeric(difftime(ts, tslag, units = "mins"))),
         rm = ifelse(any(fixLength > 20), 1, NA)) %>%
  ungroup() %>%
  dplyr::filter(is.na(rm)) %>%
  dplyr::select(-tslag, -fixLength, -rm)

#9.4 identify and remove any trips with < 3 fixes
gps <- gps %>%
  group_by(BirdID, tripID) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  dplyr::filter(n >= 3)

#10.0 add day/night column
#calculated based on location of each fix using the suncalc package
#each fix assigned as day or night using the getSunlightTimes function (sunset and sunrise times used as cutoffs)
gps <- gps %>%
  mutate(date = as.Date(ts, tz = "America/Curacao"))

sun <- suncalc::getSunlightTimes(data = gps, tz = "America/Curacao",
                                 keep = c("sunrise", "sunset")) %>%
  distinct()

gps <- left_join(gps, sun) %>%
  mutate(int.day = lubridate::interval(sunrise, sunset),
         tod = c("night", "day")[(ts %within% int.day) + 1]) %>%
  dplyr::select(-sunrise, -sunset, -int.day)

rm(sun)

#11.0 categorize trips as complete or not
gps <- gps %>%
  group_by(BirdID, tripID) %>%
  arrange(ts) %>%
  mutate(completeTrip = ifelse(any(tripCat == "tripStart") & any(tripCat == "tripEnd"), 1, 0)) %>%
  ungroup() 

######################################
#12. plot all the trips to check trip splitting worked

#plot each bird separately
for(i in 1:length(dlist)){
  
  bird <- gps %>% filter(BirdID == dlist[i]) 
  birdic <- gps %>% filter(BirdID == dlist[i]) %>%
    dplyr::filter(completeTrip == 0)
    
  dep <- deploys %>% filter(BirdID == dlist[i]) 
  
  stime <- dep$deployTime - hours(4)
  etime <- max(bird$ts) + hours(4)
  
  lims <- c(stime, etime)
  
  cest <- bird %>% dplyr::filter(tripCat %in% c("tripStart", "tripEnd"))
  
  bird.night <- bird %>% dplyr::filter(tod == "night")
  
  #dist to colony
  p1 <- ggplot(data = bird, aes(x = ts, y = colonyDist, color = ts)) + #plot distance to colony as a function of time, color by time
    geom_hline(yintercept = 0, color = "red") + #add red line at 0 m from the colony
    geom_hline(yintercept = 500, linetype = "dashed") + #add dashed line at 500 m from the colony
    geom_point() +
    geom_path(data = bird, aes(x = ts, y = colonyDist, color = ts, group = tripID)) +
    geom_point(data = birdic, aes(x = ts, y = colonyDist), color = "grey50") + #plot incomplete trips as grey dots and lines
    geom_path(data = birdic, aes(x = ts, y = colonyDist, group = tripID), color = "grey50") +
    scale_color_gradientn(name = "timestamp", #color scale formatting
                          colors = viridis(40, direction = -1, begin = 0, end = 1),
                          trans = "time") +
    scale_x_datetime(date_breaks = "1 day", #x axis formatting
                     date_minor_breaks = "1 hour",
                     date_labels = "%d-%b",
                     limits = lims,
                     timezone = "America/Curacao",
                     expand = c(0, 0)) +
    geom_point(data = bird.night, #add a black outline to fixes occurring at night
               aes(x = ts, y = colonyDist), size = 1.1, shape = 16, color = "white") + #color fixes occurring at night white
    geom_point(data = cest, #color trip start and end fixes red
               aes(x = ts, y = colonyDist), size = 1.1, shape = 16, color = "red") +
    geom_point(data = bird.night, 
               aes(x = ts, y = colonyDist), size = 1.1, shape = 1, color = "black") +
    geom_point(data = dep, aes(x = retrievalTime, y = 0), color = "red", size = 2) + #plot retrieval time as a large red dot
    geom_point(data = dep, aes(x = deployTime, y = 0), color = "red", size = 2) + #plot deploy time as a large red dot
    theme(legend.position = "top", #more plot formatting
          panel.grid.minor.x = element_line(colour="lightgrey", linewidth = 0.5),
          panel.grid.major.x = element_line(colour="lightgrey", linewidth = 0.5)) +
    guides(colour = guide_colourbar(barwidth=25)) +
    ylab("Distance from Colony (m)") + xlab("Time")
  
  png(filename = file.path("C:/Users/agnag/My Drive/MSc/Data and Analyses/Tern spatial data analysis/figs/trip-splitting-plots",
                           paste0(dep$species, "_",
                                  dep$deployYear, "_birdID_",
                                  dep$BirdID, ".png")),
      width=10, height=6, units="in", res=300,
      type = "cairo") 
  
  
  print(p1)
  
  dev.off()
  
  
}
rm(i)


