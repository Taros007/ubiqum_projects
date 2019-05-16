#Leaflet, interactive (zooming)
library(leaflet)

#Add IDs for easy marker identification
wifiVerification$ID = seq.int(nrow(wifiVerification))

studentIcon = iconList(student = makeIcon('./resources/student.png'))

studentIcon <- makeIcon(
  iconUrl = 'https://image.flaticon.com/icons/svg/67/67902.svg',
  iconWidth = 19, iconHeight = 47
)

wifiIcon <- makeIcon(
  iconUrl = 'https://image.flaticon.com/icons/svg/53/53524.svg',
  iconWidth = 19, iconHeight = 47
)

leaflet() %>%
  setView(lng = -0.06734, lat = 39.99266, zoom = 17) %>% 
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=checkexceptions$mapLNG, lat=checkexceptions$mapLAT) %>% 
  addMarkers(lng=checkexceptions$mapPredictionsLNG, lat = checkexceptions$mapPredictionsLAT, icon = studentIcon)
  #addMarkers(lng=WAPlocations$mapLNG, lat = WAPlocations$mapLAT, popup="", icon = wifiIcon)
  #addMarkers(lng=wifiVerification$PredictionsmapLNG, lat = wifiVerification$PredictionsmapLAT, icon = studentIcon, popup=as.character(wifiVerification$ID))
