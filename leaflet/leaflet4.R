rm(list=ls())
library(leaflet)
## part 1 - Clsuter close landmarks
df1 = data.frame(lat = runif(500, min = 39.25, max = 39.35),
                 lng = runif(500, min = -76.65, max = -76.55))
df1 %>% 
  leaflet() %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions())

#### part 2 - Cirlce Markers
rm(list=ls())
df = data.frame(lat = runif(20, min = 39.25, max = 39.35),
                lng = runif(20, min = -76.65, max = -76.55))

df %>% leaflet() %>% addTiles() %>% addCircleMarkers()
 

### customize their color, radius, stroke, opacity, etc.
# Create a palette that maps factor levels to colors
rm(list=ls())
df_ship = sp::SpatialPointsDataFrame(
  cbind(
    (runif(20) - .5) * 10 - 90.620130,  # lng
    (runif(20) - .5) * 3.8 + 25.638077  # lat
    ),
  data.frame(type = factor(
    ifelse(runif(20) > 0.75, "pirate", "ship"),
    c("ship", "pirate")
    ))
)

pal = colorFactor(c("navy", "red"), domain = c("ship", "pirate"))

leaflet(df_ship) %>% 
  addTiles() %>%
  addCircleMarkers(
    radius = ~ifelse(type == "ship", 6, 10),
    color = ~pal(type),
    stroke = F, fillOpacity = .7
    )

