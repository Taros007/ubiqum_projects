
# Load data ---------------------------------------------------------------
source('./R/data_loading_verification.R')
source('./R/Triangulation/locate_WAPS_v2.R')

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(rgdal)

#Remove WAPs that lead to faulty predictions
#wifiVerification %<>% select(-(c("WAP335", "WAP067", "WAP279", "WAP068")))

wifiVerification$PredictionsLAT <- NA
wifiVerification$PredictionsLNG <- NA
wifiVerification$Distance <- NA
wifiVerification$PredictionsFLOOR <- NA
wifiVerification$PredictionsBUILDINGID <- NA

n <- 1
S <- -1000
for (user in 1:nrow(wifiVerification)) {
  selected_user <- wifiVerification[user,] %>%
    select(contains("WAP")) %>%
    rownames_to_column %>% #transpose tibble
    gather(var, value, -rowname) %>% #transpose tibble
    spread(rowname, value) #transpose tibble

  if (dim(selected_user %>% filter_all(all_vars(. != -200 & . > S)))[1] == 0 |
      dim(selected_user %>%
          filter_all(all_vars(. != -200 & . > S)) %>%
          rename("WAP" = var, "RSSI" = '1') %>%
          left_join(., WAPlocations, by = "WAP") %>% 
          na.omit())[1] == 0) {
    selected_user %<>%
      filter_all(all_vars(. != -200)) %>%
      rename("WAP" = var, "RSSI" = '1') %>%
      left_join(., WAPlocations, by = "WAP") %>% #add WAP locations (not to be confused with user location)
      na.omit()
  } else {
    selected_user %<>%
      filter_all(all_vars(. != -200 & . > S)) %>%
      rename("WAP" = var, "RSSI" = '1') %>%
      left_join(., WAPlocations, by = "WAP") %>% #add WAP locations (not to be confused with user location)
      na.omit()
  }
  
  if (nrow(selected_user) != 0) {
    selected_user$weight = 10^(selected_user$RSSI / (10 * n))
    wifiVerification$PredictionsLAT[user] <- sum(selected_user$weight * selected_user$LAT) / sum(selected_user$weight)
    wifiVerification$PredictionsLNG[user] <- sum(selected_user$weight * selected_user$LNG) / sum(selected_user$weight)
    wifiVerification$PredictionsFLOOR[user] <- sum(selected_user$weight * selected_user$FLOOR) / sum(selected_user$weight)
    wifiVerification$PredictionsBUILDINGID[user] <- sum(selected_user$weight * selected_user$BUILDINGID) / sum(selected_user$weight)
  }
}

wifiVerification$PredictionsFLOOR <- round(wifiVerification$PredictionsFLOOR / 4)
wifiVerification$PredictionsBUILDINGID <- round(wifiVerification$PredictionsBUILDINGID / 20)

#Calculate prediction error using Pythagoras
wifiVerification$Distance <- sqrt((wifiVerification$PredictionsLAT - wifiVerification$LATITUDE)^2 + 
                                    (wifiVerification$PredictionsLNG - wifiVerification$LONGITUDE)^2)
wifiVerification$DistanceFlag <- ifelse(wifiVerification$Distance > 15, T, F)

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
postResample(wifiVerification$PredictionsFLOOR, wifiVerification$FLOOR)
postResample(wifiVerification$PredictionsBUILDINGID, wifiVerification$BUILDINGID)

# Plot density graph for distance
wifiVerification %>% 
  ggplot(aes(x=Distance)) + 
  geom_density() +
  labs(title = "Position prediction error", 
       subtitle = "for verification data, in m") +
  bbc_styl

#Analyse whether errors are related to floor
wifiVerification %>% 
  ggplot(aes(x = FLOOR, fill = DistanceFlag)) +
  geom_bar(position = "dodge") +
  facet_grid((.~BUILDINGID))

#Analyse whether errors are related to building
wifiVerification %>% 
  ggplot(aes(x = BUILDINGID, fill = DistanceFlag)) +
  geom_bar(position = "dodge")

#Analyse whether errors are related to WAP
wifiVerification %>%
  mutate_at(vars(contains("WAP")), function(x) ifelse(x == -200, 0, 1)) %>% 
  group_by(DistanceFlag) %>% 
  summarize_at(vars(contains("WAP")), sum) %>% 
  gather(key = "WAP", value = "Frequency", -DistanceFlag) %>% 
  ggplot(aes(x = WAP, y = Frequency, fill = DistanceFlag)) +
  geom_col()

wifiVerification %>%
  mutate_at(vars(contains("WAP")), function(x) ifelse(x == -200, 0, 1)) %>% 
  group_by(DistanceFlag) %>% 
  summarize_at(vars(contains("WAP")), sum) %>% 
  rownames_to_column %>% #transpose tibble
  gather(var, value, -rowname) %>% #transpose tibble
  spread(rowname, value) %>% #transpose tibble
  tail(-1) %>% 
  mutate("Percentage_wrong" = .$'2' / .$'1') %>% 
  arrange(desc(Percentage_wrong)) %>% 
  View()

#Check avg. signal strength for FLAGGED DISTANCE obs
y <- wifiVerification %>% 
  mutate_at(vars(contains("WAP")), function(x) ifelse(x == -200, NA, x)) %>% 
  group_by(DistanceFlag) %>% 
  summarize_at(vars(contains("WAP")), funs(mean(., na.rm = TRUE))) %>% 
  rownames_to_column %>% #transpose tibble
  gather(var, value, -rowname) %>% #transpose tibble
  spread(rowname, value) %>% #transpose tibble
  mutate("Difference" = .$'2' - .$'1')
  
#Analyze flagged coordinates, as well as wrong floor predictions
wifiVerification %>% 
  mutate("WrongFloorFlag" = FLOOR != PredictionsFLOOR) %>% 
  filter(WrongFloorFlag == T) %>% 
  ggplot(aes(x = PHONEID)) +
  geom_histogram(stat="count")

#Plot in which building floor is wrong predicted the most
wifiVerification %>% 
  mutate("WrongFloorFlag" = FLOOR != PredictionsFLOOR) %>% 
  filter(WrongFloorFlag == T) %>% 
  ggplot(aes(x = BUILDINGID, fill = PHONEID)) +
  geom_histogram(stat="count")

#Plot which floor is wrong predicted the most
wifiVerification %>% 
  mutate("WrongFloorFlag" = FLOOR != PredictionsFLOOR) %>% 
  filter(WrongFloorFlag == T) %>% 
  ggplot(aes(x = FLOOR, fill = PHONEID)) +
  geom_histogram(stat="count") +
  facet_wrap(~BUILDINGID)

#Plot wrong floor per wap
wifiVerification %>% 
  mutate("WrongFloorFlag" = FLOOR != PredictionsFLOOR) %>% 
  mutate_at(vars(contains("WAP")), function(x) ifelse(x == -200, 0, 1)) %>% 
  group_by(WrongFloorFlag) %>% 
  summarize_at(vars(contains("WAP")), sum) %>% 
  gather(key = "WAP", value = "Frequency", -WrongFloorFlag) %>% 
  ggplot(aes(x = WAP, y = Frequency, fill = WrongFloorFlag)) +
  geom_col()

#Plot how many WAPs have signal for wrong and correct floor
wifiVerification %>% 
  mutate("WrongFloorFlag" = FLOOR != PredictionsFLOOR) %>%
  mutate_at(vars(contains("WAP")), function(x) ifelse(x == -200, 0, x)) %>% 
  filter(BUILDINGID == 0 & FLOOR == 1) %>% 
  group_by(WrongFloorFlag) %>% 
  summarize_at(vars(contains("WAP")), mean) %>% #also tried var instead of mean
  rownames_to_column %>% #transpose tibble
  gather(var, value, -rowname) %>% #transpose tibble
  spread(rowname, value) %>% #transpose tibble
  rename("WAP" = var, "RightFloor" = '1', "WrongFloor" = '2') %>% #, "NA" = '3') %>% 
  #filter(RightFloor > 0 | WrongFloor > 0) %>% 
  #filter(abs(WrongFloor - RightFloor) > 40) %>% 
  ggplot(aes(x = WAP, y = (WrongFloor - RightFloor))) +
  geom_point()
  
conf <- wifiVerification %>%
  mutate(PredictionsFLOOR = droplevels(factor(PredictionsFLOOR)),
         FLOOR = droplevels(factor(FLOOR)))
confusionMatrix(conf$FLOOR, conf$PredictionsFLOOR)

