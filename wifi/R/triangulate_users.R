# Load data ---------------------------------------------------------------
source('./R/data_loading.R')
source('./R/data_loading_verification.R')
source('./R/locate_WAPS.R')

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(rgdal)

wifiVerification$PredictionsLAT <- NA
wifiVerification$PredictionsLNG <- NA
wifiVerification$Distance <- NA

for (user in 1:nrow(wifiVerification)) {
  selected_user <- wifiVerification[user,] %>% 
    select(contains("WAP")) %>% 
    rownames_to_column %>% #transpose tibble
    gather(var, value, -rowname) %>% #transpose tibble
    spread(rowname, value) %>% #transpose tibble
    filter_all(all_vars(. != -200)) %>% #remove WAPs without signal
    rename("WAP" = var, "RSSI" = '1') %>% 
    left_join(., WAPlocations, by = "WAP") %>% #add WAP locations (not to be confused with user location)
    na.omit()
  if (nrow(selected_user) != 0) {
    total_rssi <- sum(selected_user$RSSI)
    wifiVerification$PredictionsLAT[user] <- sum(selected_user$RSSI * selected_user$LAT) / total_rssi
    wifiVerification$PredictionsLNG[user] <- sum(selected_user$RSSI * selected_user$LNG) / total_rssi
    #Calculate prediction error using Pythagoras
    wifiVerification$Distance <- sqrt((wifiVerification$PredictionsLAT - wifiVerification$LATITUDE)^2 + 
      (wifiVerification$PredictionsLNG - wifiVerification$LONGITUDE)^2)
  }
}

#Add latitude and longitude EPSG4236 for plotting on leaflet/map
cord.EPSG3857 <- SpatialPoints(cbind(wifiVerification$PredictionsLNG, wifiVerification$PredictionsLAT), proj4string = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"))
cord.EPSG4326 <- spTransform(cord.EPSG3857, CRS = "+proj=longlat +datum=WGS84 +no_defs")
wifiVerification$PredictionsmapLAT <- cord.EPSG4326@coords[,2]
wifiVerification$PredictionsmapLNG <- cord.EPSG4326@coords[,1]
remove(cord.EPSG3857, cord.EPSG4326)

#Calculate prediction metrics
library(caret)
postResample(wifiVerification$PredictionsLNG, wifiVerification$LONGITUDE)
postResample(wifiVerification$PredictionsLAT, wifiVerification$LATITUDE)

# Plot density graph for distance
wifiVerification %>% 
  ggplot(aes(x=Distance)) + 
  geom_density() +
  labs(title = "Position prediction error", 
       subtitle = "for verification data, in m")
