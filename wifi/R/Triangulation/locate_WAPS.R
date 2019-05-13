# Load data ---------------------------------------------------------------
source('./R/data_loading.R')

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(rgdal)

replace_RSSI_strongweak <- T
remove_userID6_weird_signals <- T
normal_scaling <- T

# Replace variables with RSSI >90, <30 with -200 -----------------------------

if (replace_RSSI_strongweak) {
  wifiData <- bind_cols(wifiData %>% select(contains("WAP")) %>% replace(. > -10 | . < -90, -200),
                        wifiData %>% select(-contains("WAP")))
}

# Remove high value observation for user 6 --------------------------------
if (remove_userID6_weird_signals) {
  wifiData[wifiData$USERID==6,] <- bind_cols(wifiData %>% filter(USERID == 6) %>% select(contains("WAP")) %>% replace(. > -25, -200),
                                             wifiData %>% filter(USERID == 6) %>% select(-contains("WAP")))
}


# Normalize RSSI values ---------------------------------------------------
if (normal_scaling) {
  wifiData <- cbind(as.data.frame(t(scale(t(wifiData %>% select(contains("WAP")))))),
                    wifiData %>% select(-contains("WAP"))
                    )
}

#Remove WAPs that lead to faulty predictions
wifiData %<>% select(-(c("WAP335", "WAP067", "WAP279", "WAP068")))

# Create WAPlocations to be filled ----------------------------------------
WAPlocations <- wifiData %>% 
  select(contains("WAP")) %>% 
  colnames() %>% 
  as_tibble() %>%
  rename(WAP = value)

WAPlocations$LAT <- NA
WAPlocations$LNG <- NA

#For every wap, take all RSSI values that are not NA, then RSSI * LAT (or LNG) / total RSSIum

i <- 1
for (wap in WAPlocations$WAP) {
  selected_wap <- wifiData %>% 
    select(!!wap, LATITUDE, LONGITUDE) %>% 
    filter_all(all_vars(. != -200)) %>% 
    rename(rssi = wap) %>% 
    mutate(rssi = 1 / rssi)
  if (nrow(selected_wap) != 0) {
    total_rssi <- sum(selected_wap %>% select(rssi))
    WAPlocations[i,2] <- sum(selected_wap %>% select(rssi) * selected_wap$LATITUDE) / total_rssi
    WAPlocations[i,3] <- sum(selected_wap %>% select(rssi) * selected_wap$LONGITUDE) / total_rssi
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
