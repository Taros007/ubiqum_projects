# Sample map (can take long time!) ----------------------------------------

#Leaflet, interactive (zooming), but slow
library(leaflet)
leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=cord.dec@coords[,1], lat=cord.dec@coords[,2], popup="")

#GGmaps. If using Google Maps, an API (creditcard required) is needed.
#See: https://www.littlemissdata.com/blog/maps
library(ggmap)

university <- get_map(c(lon = 39.99339, lat = -0.06837128), 
                      zoom = 13, 
                      source = "stamen", 
                      maptype = "toner-lite")

#[to be implemented]