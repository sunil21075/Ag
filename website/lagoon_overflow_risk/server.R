# Lagoon

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

  #
  # Bloom Dashboard plots
  #
  observeEvent(input$bcf_map_marker_click, 
           { p <- input$bcf_map_marker_click$id
             lat <- substr(as.character(p), 1, 8)
             long <- substr(as.character(p), 13, 21)
             file_dir_string <- paste0("/data/hnoorazar/", 
                                       "bloom/plots/", 
                                       "CM_locs/bloom_thresh_in_one",
                                       "/no_obs/apple/thresh_75/")
             toggleModal(session,
                         modalId = "bcf_graphs", 
                         toggle =  "open")

             output$bcf_plot <- renderImage({
                   image_name <- paste0(lat, "_-", long, "_", 
                                        gsub(" ", "_", input$em_scenario), 
                                        "_", 
                                        input$bcf_plot_fruit_type,
                                        ".png")

                   filename <- normalizePath(file.path(file_dir_string, 
                                                       image_name))
                   # Return a list containing the filename and alt text
                   list(src = filename, 
                        width = 620, height = 370)
                                            }, deleteFile = FALSE
                                            )
          })

  ########################### ANALOG W/ just side bar
  ########################### to choose County names from.
  #############################################
  ############################################# ANALOG WITH Global map
  #############################################
  #
  # Create the map
  #
  
  ###################################################
  ###################################################
  ###################################################
  output$location_group <- renderImage({filename <- normalizePath(file.path('./plots/', 'location-group.png'))
                                        # Return a list containing the filename and alt text
                                        list(src = filename, width = 600, height = 600)}, 
                                        deleteFile = FALSE)
  
  output$Adult_Gen_Aug_rcp85 <- renderImage({
                                  filename <- normalizePath(file.path('./plots/', 
                                                                      'Adult_Gen_Aug_rcp85.png'))
                                  # Return a list containing the filename and alt text
                                  list(src = filename, width = 600, height = 450)}, 
                                  deleteFile = FALSE)
  
  output$Larva_Gen_Aug_rcp85 <- renderImage({
                                  filename <- normalizePath(file.path('./plots/', 
                                                                      'Larva_Gen_Aug_rcp85.png'))
                                  # Return a list containing the filename and alt text
                                  list(src = filename, width = 600, height = 450)}, 
                                  deleteFile = FALSE)
  
  output$Adult_Gen_Aug_rcp45 <- renderImage({
                                        filename <- normalizePath(file.path('./plots/', 
                                                                 'Adult_Gen_Aug_rcp45.png'))
                                        # Return a list containing the filename and alt text
                                        list(src = filename, width = 600, height = 450)}, 
                                        deleteFile = FALSE)
  
  output$Larva_Gen_Aug_rcp45 <- renderImage({
                                        filename <- normalizePath(file.path('./plots/', 
                                                            'Larva_Gen_Aug_rcp45.png'))
                                        # Return a list containing the filename and alt text
                                      list(src = filename, width = 600, height = 450)}, 
                                      deleteFile = FALSE)

  output$adult_emergence_rcp85 <- renderImage({filename <- normalizePath(file.path('./plots/', 
                                                                                   'adult_emergence_rcp85.png'))
                                                # Return a list containing the filename and alt text
                                                list(src = filename, width = 600, height = 450)}, 
                                                deleteFile = FALSE)
  output$adult_emergence_rcp45 <- renderImage({filename <- normalizePath(file.path('./plots/', 
                                                                           'adult_emergence_rcp45.png'))
                                       # Return a list containing the filename and alt text
                                       list(src = filename, width = 600, height = 450)}, 
                                       deleteFile = FALSE)
  
  output$diapause_abs_rcp85 <- renderImage({
    filename <- normalizePath(file.path('./plots/', 'diapause_abs_rcp85.png'))
    
    # Return a list containing the filename and alt text
    list(src = filename, width = 800, height = 600)
    
  }, deleteFile = FALSE)

  output$abs_pop_doy <- renderImage({
    filename <- normalizePath(file.path('./plots/Diapause', 'abs_pop_doy.png'))
    
    # Return a list containing the filename and alt text
    list(src = filename, width = 800, height = 900)
    
  }, deleteFile = FALSE)

  output$diapause_abs_rcp45 <- renderImage({
    filename <- normalizePath(file.path('./plots/', 'diapause_abs_rcp45.png'))
    
    # Return a list containing the filename and alt text
    list(src = filename, width = 800, height = 600)
    
  }, deleteFile = FALSE)

  output$cumdd <- renderImage({
    filename <- normalizePath(file.path('./plots', 'cumdd_rcp85.png'))
    
    # Return a list containing the filename and alt text
    list(src = filename, width = 600, height = 450)
    
  }, deleteFile = FALSE)

  output$cum_larva_pop <- renderImage({
    filename <- normalizePath(file.path('./plots', 'eggHatch_rcp85.png'))
    
    # Return a list containing the filename and alt text
    list(src = filename, width = 600, height = 450)
    
  }, deleteFile = FALSE)

  output$cumdd_rcp45 <- renderImage({
    filename <- normalizePath(file.path('./plots', 'cumdd_rcp45.png'))
    # Return a list containing the filename and alt text
    list(src = filename, width = 600, height = 450)
    
  }, deleteFile = FALSE)

  output$cum_larva_pop_rcp45 <- renderImage({
         filename <- normalizePath(file.path('./plots', 'eggHatch_rcp45.png'))
         # Return a list containing the filename and alt text
         list(src = filename, width = 600, height = 450)
         }, 
        deleteFile = FALSE)
  #######################################################

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
                              radius = 7,
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
      map = addLegend(map, "bottomleft", pal = pal, values = legendVals,
                       title = title,
                       labFormat = myLabelFormat(prefix = "  ", dates=TRUE),
                       opacity = 0.7) 
    }
    else {
       map = addLegend(map, "bottomleft", pal = pal, values = legendVals,
                       title = title,
                       labFormat = myLabelFormat(prefix = " "),
                       opacity = 0.7)
    }

    map
  }


})
