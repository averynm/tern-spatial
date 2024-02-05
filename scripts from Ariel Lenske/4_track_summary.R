###############################################################################
#STPE gps data - summary info stpe gps tracks
#2021-11-08
###############################################################################

#packages
library(tidyverse)
library(lubridate)
library(sf)
library(ggplot2)

#source functions
source("R_scripts/functions/set_ggplot_custom_theme.R")

###############################################################################
#1. read in data
gps <- readRDS("data_clean/stpe_gps_data_with_tripID.rds")
head(as.data.frame(gps))

#2. summary info

#num of birds
nrow(gps %>% select(metalBand) %>% distinct()) #111

#num of tracks
nrow(gps %>% select(deployID, tripID) %>% distinct()) #167

### num of complete/incomplete trips by island, species and deployNestStage
knitr::kable(gps %>% 
               filter(!is.na(lat)) %>%
               select(deployID, location, species, tripID, deployNestStage, completeTrack) %>%
               distinct() %>%
               group_by(location, species, deployNestStage, completeTrack) %>%
               mutate(completeTrack = ifelse(completeTrack == 0, "no", "yes")) %>%
               summarise(n = n()) %>%
               arrange(desc(completeTrack)))

### gps fix success across deployments/trips (complete tracks only)
knitr::kable(gps %>% 
               filter(completeTrack == 1) %>%
               mutate(fix = ifelse(is.na(lat), "unable_to_get_fix", "fix_obtained")) %>%
               group_by(deployID, location, species, tripID, fix) %>%
               summarise(n = n()) %>%
               pivot_wider(names_from = fix, values_from = n) %>%
               mutate(unable_to_get_fix = replace_na(unable_to_get_fix, 0),
                      proportion_fixes_successful = round(fix_obtained/(fix_obtained+unable_to_get_fix), digits = 2)) %>%
               as.data.frame())


### Settle time
#### approximate time between tag deployment/attachment and a bird's departure from the colony
#(departure time estimated based on first successful fix of a deployment)
ttd <- gps %>% select(deployID, ts, deployTime, deployNestStage) %>%
  group_by(deployID) %>%
  arrange(ts) %>%
  slice_head() %>%
  mutate(timetoDepart = as.numeric(round(difftime(ts, deployTime, units = "hours"), digits = 2)))

brx <- seq(0, max(ttd$timetoDepart), by = 2)

ggplot() +
  geom_histogram(data = ttd, aes(timetoDepart, fill = deployNestStage),
                 color = "black", breaks = brx) +
  scale_x_continuous(breaks = round(seq(0, max(ttd$timetoDepart), by = 10), 0)) +
  scale_fill_viridis_d(begin = 0.1, end = 0.7) +
  xlab("time between tagging and departure (hours)")

### Colony departure and arrival times 
#### times estimated based on first and last fix of each trip
tda <- gps %>%
  dplyr::select(ts, tripCat, completeTrack) %>%
  dplyr::filter(tripCat %in% c("tripStart", "tripEnd") & completeTrack == 1) %>%
  mutate(hms = as.POSIXct(format(ts, format = "%H:%M:%S"), format = "%H:%M:%S", tz = "America/Vancouver"))


ggplot(tda) +
  geom_histogram(aes(x = hms, fill = tripCat), binwidth = 60*60,  color = "black") +
  scale_x_datetime(date_breaks = "4 hour",
                   date_labels = "%H:%M:%S",
                   timezone = "America/Vancouver",
                   expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 80), expand = c(0,0)) +
  xlab("time") +
  scale_fill_viridis_d(begin = 0.2, end = 1) 

### Trip time and maximum distance traveled away from the colony
#calculate trip time
tt <- gps %>% 
  filter(!is.na(lat) & !is.na(deployNestStage) & completeTrack == 1) %>%
  dplyr::select(location, deployID, tripID, species, deployNestStage, completeTrack, firstDetect, lastDetect) %>%
  distinct() %>%
  mutate(tripTime = round(difftime(lastDetect, firstDetect, units = "days"), digits = 2),
         deployIDt = paste0(deployID, " (t", tripID,")")) 

# %>%
#   arrange(deployNestStage, location, tripTime) %>%
#   mutate(deployIDt = factor(deployIDt, levels = deployIDt))

## calculate maximum distance traveled from the colony
mdist <- gps %>% 
  filter(!is.na(deployNestStage) & completeTrack == 1) %>%
  group_by(deployID, tripID, location, species, deployNestStage) %>%
  summarise(maxdist = max(islandDist, na.rm = TRUE)) %>%
  mutate(deployIDt = paste0(deployID, " (t", tripID,")")) 

# %>%
#   arrange(deployNestStage, location, maxdist) %>%
#   mutate(deployIDt = factor(deployIDt, levels = deployIDt))

#### Trip time 
ggplot() +
  geom_histogram(data = tt, aes(x = tripTime, fill = location), color = "black", binwidth = 1) +
  facet_grid(.~deployNestStage) +
  scale_y_continuous(limits = c(0, 25), expand = c(0, 0)) +
  xlab("trip time (days)") +
  ylab("# of trips") +
  scale_fill_viridis_d()

#### Maximum distance from colony
ggplot() +
  geom_histogram(data = mdist, aes(x = maxdist, fill = location), color = "black", binwidth = 100) +
  facet_grid(.~deployNestStage) +
  scale_y_continuous(limits = c(0, 18), expand = c(0, 0)) +
  xlab("max distance from colony (km)") +
  ylab("# of trips") +
  scale_fill_viridis_d()

### Plots of trip time and max distance for individual trips
#### Total trip time for each trip
ggplot() +
  geom_segment(data = tt, aes(x = tripTime, xend = 0, y = deployIDt, yend = deployIDt, color = location), size = 3) +
  facet_grid(deployNestStage~., scales = "free_y") +
  scale_x_continuous(limits = c(0, 9), expand = c(0, 0)) +
  xlab("trip time (days)") +
  ylab("deploy ID (trip #)") +
  scale_color_viridis_d()

#### Furthest distance from colony for each trip
ggplot() +
  geom_segment(data = mdist, aes(x = maxdist, xend = 0, y = deployIDt, yend = deployIDt, color = location), size = 3) +
  facet_grid(deployNestStage~., scales = "free_y") +
  scale_x_continuous(limits = c(0, 1500), expand = c(0, 0)) +
  xlab("max distance from colony (km)") +
  ylab("deploy ID (trip #)") +
  scale_color_viridis_d()
