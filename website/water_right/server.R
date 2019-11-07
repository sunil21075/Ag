# Bloom - Vince

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
  #############################################
  ############################################# Bloom W/ Global map
  #############################################

  # Show page on click event for chill frost
  spatial_bcf_data <- reactive({
    spatial_bcf
  })

  #
  # Create the map
  #
  output$bcf_map <- renderLeaflet({
    pal <- colorBin(palette = "plasma", reverse = TRUE,
                    domain = spatial_bcf_data()$lat, bins = 8, pretty=TRUE)
    leaflet() %>%
    addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
             attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
    setView(lat = 47, lng = -120, zoom = 7) %>%
    addCircleMarkers(data = spatial_bcf_data(), 
                     lng = ~ long, lat = ~ lat,
                     label = ~ location,
                     layerId = ~ location,
                     radius = 4,
                     color = ~ pal(lat),
                     stroke  = FALSE,
                     fillOpacity = .95)
  })

  ###################################################
  
  output$map_bloom_doy_50 <- renderLeaflet({
    layerlist = levels(bloom_rcp45_50$ClimateGroup) # diap$ClimateGroup

    if(input$cg_bloom_50 == "Historical") {
      climate_group = input$cg_bloom_50
      future_version = "rcp85"
     } else {
      temp = tstrsplit(input$cg_bloom_50, "_")
      climate_group = unlist(temp[1])
      future_version = unlist(temp[2])
    }

    if(future_version == "rcp45") {
     bloom_d = bloom_rcp45_50
      } else { 
     bloom_d = bloom_rcp85_50
    }
    print (bloom_d$ClimateGroup)
    print (climate_group)
    sub_bloom = subset(bloom_d, 
                       apple_type == input$apple_type & 
                       ClimateGroup == climate_group)
    
    sub_bloom$location = paste0(sub_bloom$latitude, "_", 
                                sub_bloom$longitude)
    sub_bloom <- na.omit(sub_bloom)
    medBloom = list( hist = subset(sub_bloom, ClimateGroup == layerlist[1]),
                     `2040` = subset(sub_bloom, ClimateGroup == layerlist[2]),
                     `2060` = subset(sub_bloom, ClimateGroup == layerlist[3]),
                     `2080` = subset(sub_bloom, ClimateGroup == layerlist[4]))

    BloomMap <- constructMap(medBloom, 
                             layerlist, 
                             palColumn = "medDoY", 
                             legendVals = seq(50,140), #seq(min(sub_bloom$medDoY), max(sub_bloom$medDoY)), # seq(50,165), 
                             "Median Day of Year")
    BloomMap
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
    
    if(title == "Median Day of Year") {
      map = addLegend(map, "bottomleft", 
                      pal = pal, 
                      values = legendVals,
                      title = title,
                      labFormat = myLabelFormat(prefix = "  ", dates=TRUE),
                      opacity = 0.7) 
    }
    else {
       map = addLegend(map, "bottomleft", 
                       pal = pal, 
                       values = legendVals,
                       title = title,
                       labFormat = myLabelFormat(prefix = " "),
                       opacity = 0.7)
    }
    map
  }

})
