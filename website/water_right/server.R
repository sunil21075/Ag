# Water Rights

library(scales)
library(lattice)
# library(ggmap)
library(jsonlite)

library(data.table)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(maps)
library(raster)
library(rgdal)    # for readOGR and others
library(sp)       # for spatial objects
library(leaflet)  # for interactive maps (NOT leafletR here)
library(dplyr)    # for working with data frames
library(ggplot2)  # for plotting
library(reshape2)
library(RColorBrewer)

shinyServer(function(input, output, session) {
  
  #
  # Create the map
  #
  ###################################################
  
    output$water_right_map <- renderLeaflet({
      target_date <- as.Date(paste(as.character(input$year_input),
                                   as.character(input$month_input),
                                   as.character(input$day_input),
                                   sep="-")
                            )

      spatial_wtr_right <- data.table(spatial_wtr_right)
      spatial_wtr_right[, color := ifelse(date < target_date, "#FF3333", "#0080FF")]

      # pal <- colorBin(palette = "plasma", reverse = TRUE,
      #                 domain = spatial_wtr_right$color, 
      #                 bins = 2, pretty=TRUE)
      leaflet() %>%
      addTiles(urlTemplate = "http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
               attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>',
               layerId = "Satellite",
               options= providerTileOptions(opacity = 0.9)) %>%

      # addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      #          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
      setView(lat = 47, lng = -120, zoom = 7) %>%
      addCircleMarkers(data = spatial_wtr_right, 
                       lng = ~long, lat = ~lat,
                       label = ~ popup,
                       layerId = ~ location,
                       radius = 3,
                       color = ~ color,
                       stroke  = FALSE,
                       fillOpacity = .95 #,
                       # popup=paste0("<b>", title, ": </b>", 
                       #              spatial_wtr_right$WR_Doc_ID,
                       #              "<br/><b>Latitude: </b> ", 
                       #              spatial_wtr_right$lat, 
                       #              "<br/><b>Longitude: </b> ",
                       #              spatial_wtr_right]$long)
                       )
    })
  
  ###################################################################
  #
  #     Functions
  #
  ####################################################################

  constructMap <- function(mapLayerData, layerlist, palColumn, 
                           legendVals, title, gradient = "RdBu") {
    pal <- colorNumeric(
      palette = gradient, #Spectral_reverse, #"Spectral",
      domain = legendVals
    )
    myLabelFormat = function(...,dates=FALSE){ 
      if(dates){ 
        function(type = "numeric", cuts){ 
          #as.Date(cuts, origin="1970-01-01")
          format(as.Date(cuts, origin = "2018-01-01"), "%b %d")
        }  
      }else{
        labelFormat(...)
      }
    } 
    
    map <- leaflet() %>% 
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>% 
      setView(lat = 47.40, lng = -119.53, zoom = 7)
    
    for(i in 1:length(mapLayerData)) {
      loc = tstrsplit(mapLayerData[[i]]$location, "_")
      mapLayerData[[i]]$latitude = as.numeric(unlist(loc[1]))
      mapLayerData[[i]]$longitude = as.numeric(unlist(loc[2]))
      
      map <- addCircleMarkers(map,
                              data = mapLayerData[[i]],
                              lat = mapLayerData[[i]]$latitude, 
                              lng = mapLayerData[[i]]$longitude,
                              stroke = FALSE,
                              radius = 5,
                              fillOpacity = 0.9,
                              color = ~pal(mapLayerData[[i]][, get(palColumn)]),
                              popup=paste0("<b>", title, ": </b>", 
                                           round(mapLayerData[[i]][, get(palColumn)], 1),
                                           "<br/><b>Latitude: </b> ", 
                                           mapLayerData[[i]]$latitude, 
                                           "<br/><b>Longitude: </b> ", 
                                           mapLayerData[[i]]$longitude))
    }
    
    # if(title == "Median Day of Year") {
    #   map = addLegend(map, "bottomleft", 
    #                   pal = pal, 
    #                   values = legendVals,
    #                   title = title,
    #                   labFormat = myLabelFormat(prefix = "  ", dates=TRUE),
    #                   opacity = 0.7) 
    # }
    # else {
       # map = addLegend(map, "bottomleft", 
       #                 pal = pal, 
       #                 values = legendVals,
       #                 title = title,
       #                 labFormat = myLabelFormat(prefix = " "),
       #                 opacity = 0.7)
    # }
    map
  }

})
