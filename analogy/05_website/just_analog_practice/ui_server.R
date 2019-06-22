library(shiny)
library(leaflet)
library(rgdal)
library(dplyr)

ui <- fluidPage(
  )



dir <- "/Users/hn/Documents/GitHub/Kirti/analogy/05_website/just_analog_practice/"

counties <- readOGR( paste0(dir, "/tl_2017_us_county/tl_2017_us_county.shp"),
                     layer = "tl_2017_us_county", GDAL1_integer64_policy = TRUE)

states <- readOGR( paste0(dir, "/tl_2017_us_state/tl_2017_us_state.shp"),
                   layer = "tl_2017_us_state", GDAL1_integer64_policy = TRUE)

# Extract just the three states [Idaho: 16],  [OR: 41], [WA: 53]
counties <- counties[counties@data$STATEFP %in% c("16", "41", "53"), ]
##########################################################################################
#
# Simplify polygons/shapefile for making the website faster
#
counties <- rmapshaper::ms_simplify(counties)
#
# put different color on the three states, so they
# are distinguishable
#
factpal <- colorFactor(topo.colors(5), counties$category)

analog_front_page <- counties %>%
                     leaflet() %>%
                     # addTiles() %>% 
                     setView(lng = -118.4942, lat = 47.2149, zoom = 5) %>%
                     addPolygons( fillColor = ~factpal(STATEFP), fillOpacity = 0.5,
                                  # The following line is associated with borders
                                  color = "black", opacity = 1.0, weight = .6, smoothFactor = 0.5,
                                  highlightOptions = highlightOptions(color="white", weight=2, bringToFront = TRUE),
                                  label= ~ NAME)

server <- function(input, output){
  
}

shinyApp(ui = ui, server = server)