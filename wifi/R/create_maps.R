#Leaflet, interactive (zooming)
library(leaflet)
leaflet() %>%
  setView(lng = -0.06734, lat = 39.99266, zoom = 17) %>% 
  addTiles() # Add default OpenStreetMap map tiles
  #addMarkers(lng=cord.dec@coords[,1], lat=cord.dec@coords[,2], popup="")
