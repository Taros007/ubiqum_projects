#Leaflet, interactive (zooming)
library(leaflet)

#Add IDs for easy marker identification
wifiVerification$ID = seq.int(nrow(wifiVerification))

studentIcon = iconList(student = makeIcon('./resources/student.png'))
leaflet() %>%
  setView(lng = -0.06734, lat = 39.99266, zoom = 17) %>% 
  addTiles() %>%  # Add default OpenStreetMap map tiles
  #addMarkers(lng=checkexceptions$mapLNG, lat=checkexceptions$mapLAT, popup="") #%>% 
  #addMarkers(lng=checkexceptions$mapPredictionsLNG, lat = checkexceptions$mapPredictionsLAT)
  #addMarkers(lng=WAPlocations$mapLNG, lat = WAPlocations$mapLAT, popup="")
  addMarkers(lng=wifiVerification$PredictionsmapLNG, lat = wifiVerification$PredictionsmapLAT, popup=as.character(wifiVerification$ID))
