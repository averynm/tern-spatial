###############################################################################
#track quick plot
#2021-07-21
###############################################################################
#packages
library(tidyverse)
library(lubridate)
library(sf)
library(viridis)

#source functions
source("R_scripts/functions/set_ggplot_custom_theme.R")
source("R_scripts/functions/latLonDist.R")

###############################################################################
#load tag data
files <- list.files(path = "data_raw/stpe_gps_csvs/", pattern = "2022.csv", full.names = TRUE)
file.names <- list.files(path = "data_raw/stpe_gps_csvs/", pattern = "2022.csv")

#format
gpsdata <- tibble(file = file.names) %>%
  separate(col = file, into = c("species","location","tagID","metalBand","year"), sep = "[_]") %>%
  mutate(tagID = parse_number(tagID),
         metalBand = parse_number(metalBand)) %>%
  dplyr::select(-year) %>%
  mutate(Data = lapply(files, read_csv)) %>%
  unnest(Data) %>%
  na_if(9999.999) %>%
  mutate(lat = ifelse(is.na(accuracy_indicator), NA, lat),
         lon = ifelse(is.na(accuracy_indicator), NA, lon),
         height = ifelse(is.na(accuracy_indicator), NA, height),
         date = as.Date(paste0(day,"-", month,"-", year), format = "%d-%m-%y"),
         ts = as.POSIXct(paste0(day,"-", month,"-", year, " ", hour,":", min, ":", second),
                         format = "%d-%m-%y %H:%M:%S", tz = "UTC"),
         ts = with_tz(ts, "America/Vancouver"),
         ts = floor_date(ts, unit = "minute"),
         date = as.Date(ts),
         birdID = paste0("band", metalBand, "-tag", tagID)) %>%
  dplyr::select(-year, -day, -month, -hour, -min, -second, -seconds_after_midnight, -accuracy_indicator)

#add BC Albers x and y coordinates
tgpsdata <- gpsdata %>% 
  mutate(latitude = lat,
         longitude = lon) %>%
  filter(!is.na(lat))
tgpsdata <- st_as_sf(tgpsdata, coords = c("longitude", "latitude"), crs = 4326)
tgpsdata <- st_transform(x = tgpsdata, crs = 3153)
# add coordinates to dataframe
tgpsdata$x <- st_coordinates(tgpsdata)[,1] # get coordinates
tgpsdata$y <- st_coordinates(tgpsdata)[,2]
#convert back to dataframe
tgpsdata <- as.data.frame(tgpsdata) %>%
  dplyr::select(lat, lon, x, y) %>% distinct()

gpsdata <- left_join(gpsdata, tgpsdata)

# #deal with cases of inexact rounding
# gpsdata <- gpsdata %>% 
#   mutate(ts = replace(ts, ts == "2019-07-05 01:17:00" & metalBand == 115186873, "2019-07-05 01:16:00"),
#          ts = replace(ts, ts == "2019-08-04 01:29:00" & metalBand == 170115963, "2019-08-04 01:28:00"))
# 
nrow(gpsdata %>% filter(!is.na(lat))) #3439

#add rows for missing locations
fltimes <- gpsdata %>%
  dplyr::select(birdID, ts, location, tagID, metalBand, species) %>%
  group_by(birdID) %>%
  arrange(ts) %>%
  slice(c(1,n())) %>%
  mutate(category = rep(c("startt", "endt"), n()/2)) %>%
  pivot_wider(names_from = category, values_from = ts) %>%
  mutate(ts = list(seq(startt, endt, by = "2 hours"))) %>%
  unnest(cols = ts) %>%
  dplyr::select(-startt, -endt) %>%
  mutate(date = as.Date(ts)) %>%
  ungroup()

gpsdata <- left_join(fltimes, gpsdata) 

#check joined correctly 
nrow(gpsdata %>% filter(!is.na(lat))) #2275

#save dataframe
saveRDS(gpsdata, "data_clean/stpe_gps_tag_data_2022.rds") 

#plot##########################################################################

#select relevant columns
gpslocs <- gpsdata %>% dplyr::select(location, species, tagID, metalBand, birdID,
                              date, ts,lat, lon, x, y, num_satellites,
                              height, clock_offset) 


#list of birdIDs
blist <- gpslocs %>% dplyr::select(birdID) %>% distinct() %>% pull()

#setup basic things for map 
bccoast = st_read("map_data/coastline_BC/british_columbia_coastline.shp") 
eez = st_read("map_data/eez/eez.shp")
tbccoast <- st_transform(x = bccoast, crs = 3153)
teez <- st_transform(x = eez, crs = 3153)

# #plot all birds together
# xmin <- min(min(gpslocs$x, na.rm = TRUE)) - 50000
# xmax <- max(max(gpslocs$x, na.rm = TRUE)) + 50000
# ymin <- min(min(gpslocs$y, na.rm = TRUE)) - 50000
# ymax <- max(max(gpslocs$y, na.rm = TRUE)) + 50000
# 
# png(filename = paste0("figs/STPE_2021_2.png"),
#     width=11, height=8.5, units="in", res=600)
# 
# ggplot(data = teez) +
#   geom_sf(fill = "grey90", color = "grey90") +
#   geom_sf(data = tbccoast, color = "black") +
#   coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
#   geom_point(data = gpslocs,
#              aes(x, y, color = birdID), alpha = 0.5, size = 0.75) +
#   geom_path(data = gpslocs,
#             aes(x, y, color = birdID), alpha = 0.5, size = 0.75) +
#   scale_color_viridis_d()
# 
# dev.off()

###########################################################################
#plot Hippa 2022 birds only

#select relevant columns
gpsHippa <- gpsdata %>% dplyr::select(location, species, tagID, metalBand, birdID,
                              date, ts,lat, lon, x, y, num_satellites,
                              height, clock_offset) %>%
  filter(location == "Hippa")


#list of birdIDs
blist <- gpsHippa %>% dplyr::select(birdID) %>% distinct() %>% pull()

#setup basic things for map 
bccoast = st_read("map_data/coastline_BC/british_columbia_coastline.shp") 
eez = st_read("map_data/eez/eez.shp")
tbccoast <- st_transform(x = bccoast, crs = 3153)
teez <- st_transform(x = eez, crs = 3153)

#plot all birds together
xmin <- min(min(gpsHippa$x, na.rm = TRUE)) - 50000
xmax <- max(max(gpsHippa$x, na.rm = TRUE)) + 50000
ymin <- min(min(gpsHippa$y, na.rm = TRUE)) - 50000
ymax <- max(max(gpsHippa$y, na.rm = TRUE)) + 50000

png(filename = paste0("figs/STPE_2022_Hippa.png"),
    width=14, height=6, units="in", res=900)

ggplot(data = teez) +
  geom_sf(fill = "grey90", color = "grey90") +
  geom_sf(data = tbccoast, color = "black") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  geom_point(data = gpsHippa,
             aes(x, y, color = birdID), alpha = 0.5, size = 0.75) +
  geom_path(data = gpsHippa,
            aes(x, y, color = birdID), alpha = 0.5, size = 0.75) +
  scale_color_viridis_d() +
  facet_wrap(~species, nrow = 1) 

dev.off()

###########################################################################
#plot Hippa 2022 birds only - individual facets
png(filename = paste0("figs/STPE_2022_Hippa_individual_facets.png"),
    width=12, height=10, units="in", res=600)

ggplot(data = teez) +
  geom_sf(fill = "grey90", color = "grey90") +
  geom_sf(data = tbccoast, color = "black") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  geom_point(data = gpsHippa,
             aes(x, y, color = species), alpha = 0.5, size = 0.75) +
  geom_path(data = gpsHippa,
            aes(x, y, color = species), alpha = 0.5, size = 0.75) +
  scale_color_viridis_d() +
  facet_wrap(~birdID, nrow = 5)

dev.off()



# ###########################################################################
# #plot Storm 2021 birds only
# 
# #select relevant columns
# gpsStorm <- gpsdata %>% dplyr::select(location, species, tagID, metalBand, birdID,
#                                       date, ts,lat, lon, x, y, num_satellites,
#                                       height, clock_offset) %>%
#   filter(location == "Storm")
# 
# 
# #list of birdIDs
# blist <- gpsStorm %>% dplyr::select(birdID) %>% distinct() %>% pull()
# 
# #setup basic things for map 
# bccoast = st_read("map_data/coastline_BC/british_columbia_coastline.shp") 
# eez = st_read("map_data/eez/eez.shp")
# tbccoast <- st_transform(x = bccoast, crs = 3153)
# teez <- st_transform(x = eez, crs = 3153)
# 
# #plot all birds together
# xmin <- min(min(gpsStorm$x, na.rm = TRUE)) - 50000
# xmax <- max(max(gpsStorm$x, na.rm = TRUE)) + 50000
# ymin <- min(min(gpsStorm$y, na.rm = TRUE)) - 50000
# ymax <- max(max(gpsStorm$y, na.rm = TRUE)) + 50000
# 
# png(filename = paste0("figs/STPE_2021_Storm2.png"),
#     width=14, height=6, units="in", res=600)
# 
# ggplot(data = teez) +
#   geom_sf(fill = "grey90", color = "grey90") +
#   geom_sf(data = tbccoast, color = "black") +
#   coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
#   geom_point(data = gpsStorm,
#              aes(x, y, color = birdID), alpha = 0.5, size = 0.75) +
#   geom_path(data = gpsStorm,
#             aes(x, y, color = birdID), alpha = 0.5, size = 0.75) +
#   scale_color_viridis_d() +
#   facet_wrap(~species, nrow = 1)
# 
# dev.off()



################################################################################
#multi-year

###############################################################################
#load tag data
files <- list.files(path = "data_raw/stpe_gps_csvs/", full.names = TRUE)
file.names <- list.files(path = "data_raw/stpe_gps_csvs/")

#format
gpsdata <- tibble(file = file.names) %>%
  separate(col = file, into = c("species","location","tagID","metalBand","year"), sep = "[_]") %>%
  mutate(tagID = parse_number(tagID),
         metalBand = parse_number(metalBand)) %>%
  dplyr::select(-year) %>%
  mutate(Data = lapply(files, read_csv)) %>%
  unnest(Data) %>%
  na_if(9999.999) %>%
  mutate(lat = ifelse(is.na(accuracy_indicator), NA, lat),
         lon = ifelse(is.na(accuracy_indicator), NA, lon),
         height = ifelse(is.na(accuracy_indicator), NA, height),
         date = as.Date(paste0(day,"-", month,"-", year), format = "%d-%m-%y"),
         ts = as.POSIXct(paste0(day,"-", month,"-", year, " ", hour,":", min, ":", second),
                         format = "%d-%m-%y %H:%M:%S", tz = "UTC"),
         ts = with_tz(ts, "America/Vancouver"),
         ts = floor_date(ts, unit = "minute"),
         date = as.Date(ts),
         birdID = paste0("band", metalBand, "-tag", tagID)) %>%
  dplyr::select(-year, -day, -month, -hour, -min, -second, -seconds_after_midnight, -accuracy_indicator) %>%
  mutate(year = lubridate::year(date))

#add BC Albers x and y coordinates
tgpsdata <- gpsdata %>% 
  mutate(latitude = lat,
         longitude = lon) %>%
  filter(!is.na(lat))
tgpsdata <- st_as_sf(tgpsdata, coords = c("longitude", "latitude"), crs = 4326)
tgpsdata <- st_transform(x = tgpsdata, crs = 3153)
# add coordinates to dataframe
tgpsdata$x <- st_coordinates(tgpsdata)[,1] # get coordinates
tgpsdata$y <- st_coordinates(tgpsdata)[,2]
#convert back to dataframe
tgpsdata <- as.data.frame(tgpsdata) %>%
  dplyr::select(lat, lon, x, y) %>% distinct()

gpsdata <- left_join(gpsdata, tgpsdata)

#deal with cases of inexact rounding
gpsdata <- gpsdata %>% 
  mutate(ts = replace(ts, ts == "2019-07-05 01:17:00" & metalBand == 115186873, "2019-07-05 01:16:00"),
         ts = replace(ts, ts == "2019-08-04 01:29:00" & metalBand == 170115963, "2019-08-04 01:28:00"))


nrow(gpsdata %>% filter(!is.na(lat))) #5779

#add rows for missing locations
fltimes <- gpsdata %>%
  dplyr::select(birdID, ts, location, tagID, metalBand, species) %>%
  group_by(birdID) %>%
  arrange(ts) %>%
  slice(c(1,n())) %>%
  mutate(category = rep(c("startt", "endt"), n()/2)) %>%
  pivot_wider(names_from = category, values_from = ts) %>%
  mutate(ts = list(seq(startt, endt, by = "2 hours"))) %>%
  unnest(cols = ts) %>%
  dplyr::select(-startt, -endt) %>%
  mutate(date = as.Date(ts)) %>%
  ungroup()

gpsdata <- left_join(fltimes, gpsdata) 


#check joined correctly 
nrow(gpsdata %>% filter(!is.na(lat))) #4945

#plot##########################################################################

#select relevant columns
gpslocs <- gpsdata %>% dplyr::select(location, species, tagID, metalBand, birdID,
                              year, date, ts,lat, lon, x, y, num_satellites,
                              height, clock_offset) 

# gps2019 <- gpsdata %>% filter(year == 2019)
# gps2021 <- gpsdata %>% filter(year == 2021)
# gps2022 <- gpsdata %>% filter(year == 2022)

#setup basic things for map 
bccoast = st_read("map_data/coastline_BC/british_columbia_coastline.shp") 
eez = st_read("map_data/eez/eez.shp")
tbccoast <- st_transform(x = bccoast, crs = 3153)
teez <- st_transform(x = eez, crs = 3153)

#plot all birds together
xmin <- min(min(gpslocs$x, na.rm = TRUE)) - 50000
xmax <- max(max(gpslocs$x, na.rm = TRUE)) + 50000
ymin <- min(min(gpslocs$y, na.rm = TRUE)) - 50000
ymax <- max(max(gpslocs$y, na.rm = TRUE)) + 50000

png(filename = paste0("figs/STPE_allyears_2019-2022.png"),
    width=20, height=8.5, units="in", res=600)

ggplot(data = teez) +
  geom_sf(fill = "grey90", color = "grey90") +
  geom_sf(data = tbccoast, color = "black") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  geom_point(data = gpslocs,
             aes(x, y, color = as.factor(year)), alpha = 0.5, size = 0.5) +
  geom_path(data = gpslocs,
            aes(x, y, color = as.factor(year)), alpha = 0.5, size = 0.5) +
  scale_color_viridis_d() +
  facet_grid(species~location)

dev.off()

##########
#Plot Hippa birds
#plot all birds together
xmin <- min(min(gpslocs$x, na.rm = TRUE)) - 50000
xmax <- max(max(gpslocs$x, na.rm = TRUE)) + 50000
ymin <- min(min(gpslocs$y, na.rm = TRUE)) - 50000
ymax <- max(max(gpslocs$y, na.rm = TRUE)) + 50000

png(filename = paste0("figs/STPE_Hippa_allyears_2019-2022.png"),
    width=14, height=6, units="in", res=900)

ggplot(data = teez) +
  geom_sf(fill = "grey90", color = "grey90") +
  geom_sf(data = tbccoast, color = "black") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  geom_point(data = gpslocs %>% filter(location == "Hippa"),
             aes(x, y, color = as.factor(year)), alpha = 0.5, size = 0.75) +
  geom_path(data = gpslocs %>% filter(location == "Hippa"),
            aes(x, y, color = as.factor(year)), alpha = 0.5, size = 0.75) +
  scale_color_viridis_d() +
  facet_wrap(~species)

dev.off()

#####
#battery info
ggplot(data = gpsdata, aes(x = ts, y = battery, color = num_satellites)) +
  geom_point() +
  facet_wrap(~birdID)


################################
##animate
###############################################################################
#MoveVis animated GPS tracks####
#2020-05-28
###############################################################################

###Hippa animation#################################################################
###################################################################################
#packages
library(moveVis)
library(tidyverse)
library(raster)
library(ggplot2)
library(ncdf4)
library(viridis)

# #source functions
source("R_scripts/functions/set_ggplot_custom_theme.R")

#set time zone
Sys.setenv(TZ = "America/Toronto")

#load data
gps <- gpslocs 
gebco <- raster("map_data/GEBCO_2020/gebco_2020_n56.0_s38.0_w-148.0_e-122.0.nc")

#set up basemap
#create color scheme for raster map background
plot(gebco)

# Function to calculate colour break points
# x = raster, b1 & b2 = number of divisions for each sequence, r1 & r2 = rounding value
colbr <- function(x, b1=50, b2=50, r1=-2, r2=-2) {
  # Min/max values of the raster (x)
  mi <- cellStats(x, stat="min")-100
  ma <- cellStats(x, stat="max")+100
  # Create sequences, but only use unique numbers
  s1 <- unique(round(seq(mi, 0, 0-mi/b1),r1))
  s2 <- unique(round(seq(0, ma, ma/b2),r2))
  # Combine sequence for our break points, removing duplicate 0
  s3 <- c(s1, s2[-1])
  # Create a list with the outputs
  # [[1]] = length of the first sequence minus 1 (water)
  # [[2]] = length of the second sequence minus 1 (land)
  # [[3]] = The break points
  x <- list(length(s1)-1, length(s2)-1, s3)
}

mapbr <- colbr(x = gebco, b1 = 12, b2 = 12)

#colors
ocean.col <- colorRampPalette(c("dodgerblue", "lightblue1"))
land.col <- colorRampPalette(c("black", "gray70"))

plot(gebco, col=c(ocean.col(mapbr[[1]]), land.col(mapbr[[2]])), breaks = mapbr[[3]])

gebcoRGB <- RGB(gebco, col=c(ocean.col(mapbr[[1]]), land.col(mapbr[[2]])), breaks = mapbr[[3]])

plotRGB(gebcoRGB)
res(gebcoRGB)
ncell(gebcoRGB)

# # write the geotiff
# writeRaster(test,"rgbRaster.tif","GTiff", overwrite=TRUE)
# testRGB <- stack("rgbRaster.tif")
# plotRGB(testRGB)
# gplot(gebco, maxpixels = 5e5) +
#   geom_tile(aes(fill = value)) +
#   scale_fill_gradientn(breaks = mapbr[[3]], colors = c(ocean.col(mapbr[[1]]), land.col(mapbr[[2]])))

#format gps data
gps <- gps %>%
  dplyr::select(location, species, birdID,
                ts, lat, lon) %>%
  mutate(id = birdID,
         legend = birdID)

idlist <- gps %>% dplyr::select(legend) %>% distinct() %>% pull()

colpal <- data.frame(colour = viridis_pal()(length(idlist)),
                     legend = idlist)

gps <- left_join(gps, colpal)


tcols <- gps %>% dplyr::select(id, legend) %>%
  distinct() %>% left_join(., colpal) %>% dplyr::select(colour) %>%
  pull()

# bird <- gps %>% filter(id %in% c(idlist[1:1]))
# col1 <- tcols[1:1]

birdmoveHippa <- df2move(gps,
                    proj = "+proj=longlat", 
                    x = "lon", y = "lat", time = "ts", track_id = "id") %>%
  align_move(res = 1, unit = "hours") 


#make lists for frames_spatial function
#time list
timelist <- list()
timelist[[1]] <- birdmoveHippa@timestamps[1]

#raster list
rastlist <- list()
rastlist[[1]] <- gebcoRGB

framesSTPE_Hippa <- frames_spatial(birdmoveHippa,
                             path_colours = tcols,
                             r_list = rastlist[[1]],
                             r_type = "RGB",
                             r_times = timelist[[1]],
                             map_res = 1,
                             crop_raster = TRUE,
                             path_size = 2,
                             fade_raster = FALSE,
                             path_legend = FALSE,
                             tail_length = 5,
                             trace_show = T,
                             trace_colour = "white",
                             maxpixels = ncell(rastlist[[1]])/10) %>%
  add_labels(x = "Longitude", y = "Latitude", 
             title = "Storm-Petrel GPS tracks 2022 Hippa") %>%
  add_timestamps(type = "label", x = -143, y = 55, size = 5) 


framesSTPE_Hippa[[50]] #display one frame to preview it. 

animate_frames(frames = framesSTPE_Hippa,
               out_file = "animated_stpe_2022_Hippa.gif",
               overwrite = TRUE,
               fps = 5)

###Storm animation#################################################################
###################################################################################
#packages
#packages
library(moveVis)
library(tidyverse)
library(raster)
library(ggplot2)
library(ncdf4)
library(viridis)

# #source functions
source("R_scripts/functions/set_ggplot_custom_theme.R")

#set time zone
Sys.setenv(TZ = "America/Toronto")

#load data
gps <- gpsStorm %>% filter(!is.na(lat))
gebco <- raster("map_data/GEBCO_2020/gebco_2020_n56.0_s38.0_w-148.0_e-122.0.nc")

#set up basemap
#create color scheme for raster map background
plot(gebco)

# Function to calculate colour break points
# x = raster, b1 & b2 = number of divisions for each sequence, r1 & r2 = rounding value
colbr <- function(x, b1=50, b2=50, r1=-2, r2=-2) {
  # Min/max values of the raster (x)
  mi <- cellStats(x, stat="min")-100
  ma <- cellStats(x, stat="max")+100
  # Create sequences, but only use unique numbers
  s1 <- unique(round(seq(mi, 0, 0-mi/b1),r1))
  s2 <- unique(round(seq(0, ma, ma/b2),r2))
  # Combine sequence for our break points, removing duplicate 0
  s3 <- c(s1, s2[-1])
  # Create a list with the outputs
  # [[1]] = length of the first sequence minus 1 (water)
  # [[2]] = length of the second sequence minus 1 (land)
  # [[3]] = The break points
  x <- list(length(s1)-1, length(s2)-1, s3)
}

mapbr <- colbr(x = gebco, b1 = 12, b2 = 12)

#colors
ocean.col <- colorRampPalette(c("dodgerblue", "lightblue1"))
land.col <- colorRampPalette(c("black", "gray70"))

plot(gebco, col=c(ocean.col(mapbr[[1]]), land.col(mapbr[[2]])), breaks = mapbr[[3]])

gebcoRGB <- RGB(gebco, col=c(ocean.col(mapbr[[1]]), land.col(mapbr[[2]])), breaks = mapbr[[3]])

plotRGB(gebcoRGB)
res(gebcoRGB)
ncell(gebcoRGB)

# # write the geotiff
# writeRaster(test,"rgbRaster.tif","GTiff", overwrite=TRUE)
# testRGB <- stack("rgbRaster.tif")
# plotRGB(testRGB)
# gplot(gebco, maxpixels = 5e5) +
#   geom_tile(aes(fill = value)) +
#   scale_fill_gradientn(breaks = mapbr[[3]], colors = c(ocean.col(mapbr[[1]]), land.col(mapbr[[2]])))

#format gps data
gps <- gps %>%
  dplyr::select(location, species, birdID,
                ts, lat, lon) %>%
  mutate(id = birdID,
         legend = birdID)

idlist <- gps %>% dplyr::select(legend) %>% distinct() %>% pull()

colpal <- data.frame(colour = viridis_pal()(length(idlist)),
                     legend = idlist)

gps <- left_join(gps, colpal)


tcols <- gps %>% dplyr::select(id, legend) %>%
  distinct() %>% left_join(., colpal) %>% dplyr::select(colour) %>%
  pull()

# bird <- gps %>% filter(id %in% c(idlist[1:1]))
# col1 <- tcols[1:1]

birdmoveStorm <- df2move(gps,
                    proj = "+proj=longlat", 
                    x = "lon", y = "lat", time = "ts", track_id = "id") %>%
  align_move(res = 2, unit = "hours") 


#make lists for frames_spatial function
#time list
timelist <- list()
timelist[[1]] <- birdmoveStorm@timestamps[1]

#raster list
rastlist <- list()
rastlist[[1]] <- gebcoRGB

framesSTPE_Storm <- frames_spatial(birdmoveStorm,
                             path_colours = tcols,
                             r_list = rastlist[[1]],
                             r_type = "RGB",
                             r_times = timelist[[1]],
                             map_res = 1,
                             crop_raster = TRUE,
                             path_size = 2,
                             fade_raster = FALSE,
                             path_legend = FALSE,
                             trace_show = T,
                             trace_colour = "white",
                             tail_length = 5,
                             maxpixels = ncell(rastlist[[1]])/10) %>%
  add_labels(x = "Longitude", y = "Latitude", 
             title = "Storm-Petrel GPS tracks 2021 Storm") %>%
  add_timestamps(type = "label", x = -143, y = 55, size = 5) 


framesSTPE_Storm[[50]] #display one frame to preview it. 

animate_frames(frames = framesSTPE_Storm,
               out_file = "animated_stpe_2021_Storm.gif",
               overwrite = TRUE,
               fps = 5)
