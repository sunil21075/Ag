rm(list=ls())
library(leaflet)
# my_map = lealet()
# my_map = addTile(my_map)

# my_map = leaflet() %>% addTiles()
# 
# # my_map = addMarkers(my_map, lat = 39.2980803, lng = -76.5898801)
# my_map = my_map %>% addMarkers(lat = 39.2980803, 
#                                lng = -76.5898801, 
#                                popup = "Jeff Leek's Office")
# 
# my_map

my_map = leaflet() %>% 
         addTiles() %>% 
         addMarkers(lat = 39.2980803, 
                    lng = -76.5898801, 
                    popup = "Jeff Leek's Office")

my_map

