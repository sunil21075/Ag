
rm(list=ls())
library(leaflet)
leaflet() %>%
  addTiles() %>%
  addRectangles(lat1 = 37.3858, lng1 = -122.0595,
                lat2 = 37.3890, lng2 = -122.0625)


