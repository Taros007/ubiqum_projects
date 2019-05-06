# Qualitative analysis ----------------------------------------------------

# • It covers a surface of 108703m2 including 3 buildings
# with 4 or 5 floors depending on the building.
# • The number of different places (reference points)
# appearing in the database is 933.
# • 21049 sampled points have been captured: 19938 for
# training/learning and 1111 for validation/testing.
# • Dataset independence has been assured by taking
# Validation (or testing) samples 4 months after Training
# ones.
# • The number of different wireless access points (WAPs)
# appearing in the database is 520.
# • Data were collected by more than 20 users using 25
# different models of mobile devices (some users used
# more than one model).
# 
# 001-520 RSSI levels: -100 dBm equals very weak signal, 0 dBm very strong, +100 dBm dummy value for WAP not detected
# 521-523 Real world coordinates of the sample points
# 524 BuildingID
# 525 SpaceID: 0 TI, 1 TD, 2 TC
# 526 Relative position with respect to SpaceID: 0 in the room, 1 outside on corridor
# 527 UserID
# 528 PhoneID
# 529 Timestamp
# 
# Localization: EPSG:3857 WGS 84 
# 
# Ideas:
# Localizations without any WAP detections have not been removed
# Filter all observations >45dBm, as this is only 1.8% of all obserations
# Important for RSSI signal strength: hardware and Android version, way device is held and location
# 
# Problems:
# The way a device is carried, as well as objects in the room affect RSSI strength

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(magrittr)
library(lubridate)
library(rgdal)

# Load data ---------------------------------------------------------------
suppressMessages(wifiVerification <- read_csv('./input/validationData.csv'))

# Adjust variables --------------------------------------------------------
wifiVerification %<>% mutate(
  FLOOR = as.factor(FLOOR),
  BUILDINGID = as.factor(BUILDINGID),
  SPACEID = as.factor(SPACEID),
  RRELATIVEPOSITION = as.factor(RELATIVEPOSITION),
  USERID = as.factor(USERID),
  PHONEID = as.factor(PHONEID),
  TIMESTAMP = as_datetime(TIMESTAMP, tz = "Europe/Madrid")
)

# Adjust RSSI values ------------------------------------------------------

#Hence, transform values with 100 into -200

wifiVerification %<>% mutate_at(vars(WAP001:WAP520), ~(if_else(. == 100, -200, .)))

# Create map coordinates ------------------------------------------------

#Add coordinates from EPSG3857 (in meters) to EPSG4326 (in degrees) so plotting 
#in Google Maps and OSM is possible.
#Note that EPSG3857 is in meters, so use that one for distance calculations
#https://epsg.io/3857
#https://epsg.io/4326

cord.EPSG3857 <- SpatialPoints(cbind(wifiVerification$LONGITUDE, wifiVerification$LATITUDE), proj4string = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"))
cord.EPSG4326 <- spTransform(cord.EPSG3857, CRS = "+proj=longlat +datum=WGS84 +no_defs")
wifiVerification$mapLAT <- cord.EPSG4326@coords[,2]
wifiVerification$mapLNG <- cord.EPSG4326@coords[,1]
remove(cord.EPSG3857, cord.EPSG4326)

# Save RDS ----------------------------------------------------------------
saveRDS(wifiVerification, './output/wifiDataVerification.RDS')
