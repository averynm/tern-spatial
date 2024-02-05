###############################################################################
#project: TERN gps data
#0.5. load and format data 
#date: Dec 16, 2023
#authors: Avery Nagy-MacArthur (and Ariel Lenske)
###############################################################################

#packages
library(tidyverse)
library(lubridate)
library(readxl)

###############################################################################
# #1. gps tagging metadata (not run)
# #load data
# tagmeta <- read_xlsx("C:/Users/agnag/My Drive/MSc/Avery_tern_tracking_2018-2019_meta.xlsx", sheet = "capture", na = "-")
# sites <- read_xlsx("C:/Users/agnag/My Drive/MSc/Avery_tern_tracking_2018-2019_meta.xlsx", sheet = "sites", na = "-")
# 
# # clean up
# tagmeta <- tagmeta[,-c(37, 38, 39, 40)]
# 
# #check dfs
# head(as.data.frame(tagmeta))
# str(tagmeta)
# 
# #fix time columns <- double check this is doing what your want it to
# tagmeta <- tagmeta %>%
#   mutate(captureTime = force_tz(as.POSIXct(paste0(Capture_Date, " ", format(Capture_Time, "%H:%M:%S", tz = "UTC")),
#                                            format = "%Y-%m-%d %H:%M:%S"), "America/Curacao"))
# #re check tagmeta df
# head(as.data.frame(tagmeta))
# str(tagmeta)
# 
# #pull out GPS data, reformat and pull out data for retrieved tags
# ## could add my fix frequency to original spreadsheet; don't need to do this though as all tags fixed on same frequency
# gpsmeta <- tagmeta %>% dplyr::filter(tag_type == "GPS_Pathtrack")
# deploy <- gpsmeta %>% dplyr::filter(TagDeployStatus == "NEW") %>%
#   dplyr::select(Colony, species, metalBand, tag_id, BirdID,
#                 deployYear = CaptureYear, deployDate = Capture_Date, deployTime = captureTime,
#                 deployMass = mass_bird, deployComments = notes)
# 
# retrieval <- gpsmeta %>% dplyr::filter(TagDeployStatus == "RECAP") %>%
#   dplyr::select(Colony, species, metalBand, tag_id, BirdID, TagDeployStatus,
#                 retrievalYear = CaptureYear, retrievalDate = Capture_Date, retrievalTime = captureTime,
#                 retrievalMass = mass_bird, retrievalComments = notes)
# 
# #join deployment and retrieval data
# gpsmeta <- left_join(deploy, retrieval) %>% dplyr::filter(TagDeployStatus == "RECAP")
# 
# #add mass change & deployID columns
# gpsmeta <- gpsmeta %>%
#   mutate(massChange = retrievalMass - deployMass,
#          deployID = paste0(BirdID, "-", tag_id, "-", deployYear)) %>% rename(location = Colony)
# 
# #join colony site info with tag metadata
# gpsmeta <- left_join(gpsmeta, sites %>% 
#                        dplyr::select(location, location_lat = latitude, location_lon = longitude))
# 
# 
# #organize dataframe
# gpsmeta <- gpsmeta %>%
#   dplyr::select(location, location_lat, location_lon, species, metalBand, BirdID, tag_id, deployID,
#                 TagDeployStatus, deployYear,
#                 deployDate, deployTime, retrievalYear, retrievalDate, retrievalTime,
#                 deployMass, retrievalMass, massChange,
#                 deployComments, retrievalComments)
# 
# #save dataframe
# saveRDS(gpsmeta, "data_clean/tern_gps_metadata.rds")
# 
# rm(deploy, retrieval, sites, tagmeta)


###############################################################################
#2. tag data
#load gps data
alltracks.raw <- read_csv(file = file.path("data_raw", "tern_tracks_ALL_datetime.csv"))

#load metadata
deploys <- readRDS(file.path(outputbasepath, "data_processed", "tern_gps_metadata.rds"))

# #using test file of 4 tern tracks
# alltracks.raw <- read.csv(file = "testterns.csv")

#format
# need to fix "ts" column
gpsdata <- alltracks.raw %>%
  mutate(tag_id = as.character(Tag),
         yearDeploy = as.numeric(Year),
         lon = ifelse(Longitude == 0, NA, Longitude),
         lat = ifelse(Latitude == 0, NA, Latitude),
         ts = as.POSIXct(parse_date_time(timestamp,
                                         "%m/%d/%Y %H:%M", tz = "UTC")),
         ts = with_tz(ts, "America/Curacao"),
         ts = floor_date(ts, unit = "minute"),
         date = as.POSIXct(make_date(Year, Month, Day), format = "%Y-%m-%d"),
         date = ceiling_date(date, "day"),
         N_sat = N.Satellites) %>%
  select(-Year, -Day, -Month, -Hour, -Minute, -Second, -Altitude, -Clock.Offset, -N.Satellites,
         -timestamp, -Longitude, -Latitude)

#remove missed fixes OR any with <5 satellites OR accuracy > 0.00003
gpsdata <- gpsdata %>% 
  dplyr::filter(!is.na(lat)) %>%
  dplyr::filter(N_sat >= 5) %>%
  dplyr::filter(Accuracy <= 0.00003)

###############################################################################
#3. combine movement and tag metadata
gps <- left_join(gpsdata,
                 deploys %>% dplyr::select(BirdID, deployYear, deployTime, retrievalTime))

#remove any locations recorded before deployment or after retrieval
gps <- gps %>% filter(ts > deployTime & ts < retrievalTime)


#save dataframe
saveRDS(gps, file.path(outputbasepath, "data_working", "tern_gps_data_no_tripID.rds"))
