### legend

rm(list=ls())
df = data.frame(lat = runif(20, min = 39.25, max = 39.35),
                lng = runif(20, min = -76.65, max = -76.55),
                col = sample(c("red", "blue", "green"), 20, replace = TRUE),
                stringsAsFactors = F)

df %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(color = df$col) %>%
  addLegend(labels = LETTERS[1:3], colors = c("blue", "red", "green"))

