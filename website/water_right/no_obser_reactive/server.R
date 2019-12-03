

shinyServer(function(input, output, session) {
  output$water_right_map <- renderLeaflet({
  target_date <- as.Date(input$cut_date)

  water_resource <- input$water_source_type
  if (water_resource == "surfaceWater") {
      curr_spatial <- spatial_wtr_right_surface

       } else if (water_resource == "groundwater"){
          curr_spatial <- spatial_wtr_right_ground
       } else {
          curr_spatial <- spatial_wtr_right_both
  }
  curr_spatial[, colorr := ifelse(right_date < target_date, "#FF3333", "#0080FF")]
  print(curr_spatial)

  leaflet() %>%
  addTiles(urlTemplate = "http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
           attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>',
           layerId = "Satellite",
           options= providerTileOptions(opacity = 0.9)) %>%
  setView(lat = 47, lng = -120, zoom = 7) %>%
  addCircleMarkers(data = curr_spatial, 
                   lng = ~long, lat = ~lat,
                   label = ~ popup,
                   # layerId = ~ location,
                   radius = 3,
                   color = ~ colorr,
                   stroke  = FALSE,
                   fillOpacity = .95 
                    )
 
  })

})

