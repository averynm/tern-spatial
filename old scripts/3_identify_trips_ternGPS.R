###############################################################################
#ARTE gps data - identify trips
#2023-11-12
###############################################################################

#packages
library(tidyverse)
library(lubridate)
library(fuzzyjoin)
library(hms)
library(viridis)

#source functions

source("scripts from Ariel Lenske/latLonDist.R")
###############################################################################
#load data
gps <- readRDS("data_clean/arte_gps_data_no_tripID.rds")

#1. classify tracks as partial/complete (last location < 0.4 from the colony)
#exclude points 400 m from colony (radius determined based on visual examination of tracking data, and observation that
# many terns make short trips to nearshore waters < 1 km away from island) 
#calculate distances to colony for each gps location in km and classify locations as:
#tripStart, tripEnd or tripMiddle
gps <- gps %>%
  mutate(islandDist = round(latLonDist(location_lat, location_lon, lat, lon))/1000) %>%
  group_by(deployID) %>%
  mutate(firstDetect = min(ts),
         lastDetect = max(ts)) %>%
  mutate(tripCat = ifelse(ts == firstDetect, "tripStart",
                          ifelse(ts == lastDetect, "tripEnd", "tripMiddle"))) %>%
  ungroup()

# exclude points <400 m from island centre
ct <- gps %>%
  dplyr::select(deployID, tripCat, islandDist) %>%
  dplyr::filter(tripCat == "tripEnd") %>%
  mutate(completeTrack = ifelse(islandDist > 0.4, 0, 1)) %>%
  dplyr::select(deployID, completeTrack)
gps <- left_join(gps, ct)
rm(ct)