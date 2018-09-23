
library(shiny)
library(leaflet)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  points = eventReactive(input$recalc, {cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)}, ignoreNULL = FALSE)
  output$mymap = renderLeaflet({ leaflet() %>% 
                       # providers$Stamen.TonerLite
      addProviderTiles(providers$Esri.NatGeoWorldMap, options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = points())
    })
})

