rm(list=ls())
library(leaflet)

# chull: computes convex hull of the data
outline = quakes[chull(quakes$long, quakes$lat),]


# head(outline)
# one of my favorite tiles
# m %>% addProviderTiles(providers$Esri.NatGeoWorldMap)

map = leaflet(quakes) %>%
  # Base groups
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat Geo") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  # Overlay groups
  addCircles(lng = ~long, lat = ~lat, radius = ~10^mag/5, stroke = F, group = "Quakes") %>%
  addPolygons(data = outline, lng = ~long, lat = ~lat,
              fill = FALSE, weight = 5, color = "#FFFFCC", group = "Outline") %>%

  # addMarkers(data = outline[1:20,], lng = ~long, lat = ~lat, popup = ~as.character(mag), label = ~as.character(mag))
 
 # Layers control
  addLayersControl(
    baseGroups = c("OSM (default)", "Nat Geo", "Toner Lite"),
    overlayGroups = c("Quakes", "Outline"),
    options = layersControlOptions(collapsed = F)
  )
map

