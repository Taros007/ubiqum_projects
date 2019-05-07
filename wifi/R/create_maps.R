#Leaflet, interactive (zooming)
library(leaflet)
leaflet() %>%
  setView(lng = -0.06734, lat = 39.99266, zoom = 17) %>% 
  addTiles() %>%  # Add default OpenStreetMap map tiles
  #addMarkers(lng=checkexceptions$mapLNG, lat=checkexceptions$mapLAT, popup="") #%>% 
  #addMarkers(lng=checkexceptions$mapPredictionsLNG, lat = checkexceptions$mapPredictionsLAT)
  #addMarkers(lng=WAPlocations$mapLNG, lat = WAPlocations$mapLAT, popup="")
  addMarkers(lng=wifiVerification$PredictionsmapLNG, lat = wifiVerification$PredictionsmapLAT, popup="")
