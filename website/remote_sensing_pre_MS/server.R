
library(shiny)
library(leaflet)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  output$mymap = renderLeaflet({ 
  	  leaflet() %>%
       addProviderTiles(providers$Esri.WorldStreetMap, # Esri.WorldStreetMap or OpenTopoMap
                        options= providerTileOptions(opacity = 0.99))%>%
       setView(lat = 46.5, lng = -119, zoom = 9) %>%
       addPolygons(data = Min_sp,
                   stroke = TRUE, 
                   fillOpacity = 0.5, 
                   smoothFactor = 0.5)

    })


  output$mymap_1 <- renderLeaflet({
     counties %>%
     leaflet(options = leafletOptions(zoomControl = TRUE,
             minZoom = 4, maxZoom = 20, dragging = TRUE))  %>%
     addTiles() %>%
     setView(lat = 46, lng = -119, zoom = 6) %>%
      addPolygons(data = Min_sp,
                   stroke = TRUE, 
                   fillOpacity = 0.5, 
                   smoothFactor = 0.5) %>%
     addPolylines(data = states, color = "black", opacity = 1, weight = 1.5)
  })
           
})



