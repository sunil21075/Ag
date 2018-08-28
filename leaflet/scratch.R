library(leaflet)

my_map = leaflet(options = leafletOptions(minZoom = 0, maxZoom = 3)) %>% addTiles() 
my_map

