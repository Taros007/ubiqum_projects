# Load data ---------------------------------------------------------------
source('./R/data_loading.R')

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(rgdal)

# Remove high value observation for user 6 --------------------------------
remove_userID6_weird_signals <- F
if (remove_userID6_weird_signals) {
  wifiData[wifiData$USERID==6,] <- bind_cols(wifiData %>% filter(USERID == 6) %>% select(contains("WAP")) %>% replace(. > -25, -200),
                                             wifiData %>% filter(USERID == 6) %>% select(-contains("WAP")))
}

remove_phoneID_weird_signals <- T
if (remove_userID6_weird_signals) {
  wifiData[wifiData$PHONEID == 7,] <- bind_cols(wifiData %>% filter(PHONEID == 7) %>% select(contains("WAP")) %>% replace(. > -25, -200),
                                             wifiData %>% filter(PHONEID == 7) %>% select(-contains("WAP")))
  wifiData[wifiData$PHONEID == 19,] <- bind_cols(wifiData %>% filter(PHONEID == 19) %>% select(contains("WAP")) %>% replace(. > -25, -200),
                                                wifiData %>% filter(PHONEID == 19) %>% select(-contains("WAP")))
}

# #Remove WAPs that lead to faulty predictions
# wifiData %<>% select(-(c("WAP086", "WAP202", "WAP296", "WAP445", "WAP487")))
#wifiData %<>% select(-(c("WAP035", "WAP036", "WAP057", "WAP058")))
# Average all RSSI signals per floor per building -------------------------

#first the plot
# wifiData %>%
#   unite(col = coordinates, LATITUDE, LONGITUDE, FLOOR, BUILDINGID, sep = "x") %>%
#   replace(. == -200, NA) %>%
#   group_by(coordinates) %>% 
#   summarize(count = n()) %>% 
#   ggplot(aes(x = count)) +
#   geom_histogram(binwidth = 5) +
#   bbc_style() +
#   labs(title = "Observations with identical coordinates", subtitle = "LATxLNGxBUILDINGIDxFLOOR")

wifiData %<>%
  unite(col = coordinates, LATITUDE, LONGITUDE, FLOOR, BUILDINGID, sep = "x") %>%
  replace(. == -200, NA) %>%
  group_by(coordinates) %>%
  summarize_at(vars(contains("WAP")), mean) %>%
  separate(coordinates, sep = "x", into = c("LATITUDE", "LONGITUDE", "FLOOR", "BUILDINGID")) %>%
  mutate(LATITUDE = as.numeric(LATITUDE),
         LONGITUDE = as.numeric(LONGITUDE),
         FLOOR = as.numeric(FLOOR),
         BUILDINGID = as.numeric(BUILDINGID))

# Create WAPlocations to be filled ----------------------------------------
WAPlocations <- wifiData %>% 
  select(contains("WAP")) %>% 
  colnames() %>% 
  as_tibble() %>%
  rename(WAP = value)

WAPlocations$LAT <- NA
WAPlocations$LNG <- NA
WAPlocations$FLOOR <- NA
WAPlocations$BUILDINGID <- NA

#For every wap, take all RSSI values that are not NA, then RSSI * LAT (or LNG) / total RSSIum
i <- 1
n <- 1
S <- -70
for (wap in WAPlocations$WAP) {
  selected_wap <- wifiData %>% 
    select(!!wap, LATITUDE, LONGITUDE, FLOOR, BUILDINGID) %>% 
    filter_at(vars(contains("WAP")), any_vars(. != -200 & . > S)) %>% 
    rename(rssi = wap) %>% 
    mutate(FLOOR = FLOOR * 4,
           BUILDINGID = BUILDINGID * 20)
  if (nrow(selected_wap) != 0) {
    selected_wap$weight = 10^(selected_wap$rssi / 10 * n)
    WAPlocations[i,2] <- sum(selected_wap$weight * selected_wap$LATITUDE) / sum(selected_wap$weight)
    WAPlocations[i,3] <- sum(selected_wap$weight * selected_wap$LONGITUDE) / sum(selected_wap$weight)
    WAPlocations[i,4] <- sum(selected_wap$weight * selected_wap$FLOOR) / sum(selected_wap$weight)
    WAPlocations[i,5] <- sum(selected_wap$weight * selected_wap$BUILDINGID) / sum(selected_wap$weight)
  }
  i <- i + 1
}

WAPlocations <- na.omit(WAPlocations)

#Add latitude and longitude EPSG4236 for plotting on leaflet/map
cord.EPSG3857 <- SpatialPoints(cbind(WAPlocations$LNG, WAPlocations$LAT), proj4string = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"))
cord.EPSG4326 <- spTransform(cord.EPSG3857, CRS = "+proj=longlat +datum=WGS84 +no_defs")
WAPlocations$mapLAT <- cord.EPSG4326@coords[,2]
WAPlocations$mapLNG <- cord.EPSG4326@coords[,1]
remove(cord.EPSG3857, cord.EPSG4326)

