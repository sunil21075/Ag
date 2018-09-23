library(leaflet)
rm(list=ls())
set.seed(206-04-25)
df = data.frame(lat = runif(20, min=39.2, max = 39.3),
                lng = runif(20, min = -76.6, max = -76.5))

# print (head(df))
print (head(df))
df %>% leaflet() %>% addTiles() %>% addMarkers()

