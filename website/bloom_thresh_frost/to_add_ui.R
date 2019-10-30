#
# Dry
#
menuItem("Dry Days", tabName = "dry_map", icon = icon("tint"))
tabItem(tabName = "dry_map",
            box(id = "dry_days",
                width = NULL,
              
                #  Main Map  #
                tabPanel("Map",
                         div(class = "outer",
                             tags$style(type = "text/css", "#dry_map {height: calc(100vh - 125px) !important;}"),
                             leafletOutput("dry_map"),
                             absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                           draggable = TRUE, top = 90, left = "auto", right = 40, bottom = "auto",
                                           width = 250, height = "auto",
                                           h3(tags$b("Explorer")),
                                           radioButtons("dry_days_map_climate_proj", label = "Climate Projection", 
                                                        choices = list("RCP 4.5" = "rcp45", "RCP 8.5" = "rcp85")),
                                           radioButtons("dry_days_map_exceedance", label = "Exceedance Value", 
                                                        choices = list("20th Percentile" = "prob_80", 
                                                                       "10th Percentile" = "prob_90",
                                                                       "5th Percentile" = "prob_95")),
                                           radioButtons("dry_days_map_climate_group", label = "Climate Group", 
                                                        choices = list("2040s" = "2040s", "2060s" = "2060s", "2080s" = "2080s")))
                            )
                        )
                )
            )

bsModal(title = "Dry Days Summary", id = "dry_days_graphs", trigger = NULL, size = "large",
          fluidPage(fluidRow(column(2, radioButtons("dry_days_plot_climate_proj", label = h3("Climate Projection"), 
                                                    choices = list("RCP 4.5" = "rcp45", 
                                                                   "RCP 8.5" = "rcp85"),
                                                    selected = "rcp85")),
                             column(10, offset = 0, plotOutput("dry_days_plot", height = 550))
                            )
                   )
          )
##################################################
menuItem("Bloom and Chill Portions", 
             tabName = "precip_map", 
             icon = icon("tint"))

tabItem(tabName = "bloom_vs_CP_frost",
            box(id = "bcf", width = NULL,
                #  Main Map  #
                tabPanel("Map",
                         div(class = "outer",
                             tags$style(type = "text/css", "#bcf_map {height: calc(100vh - 125px) !important;}"),
                             leafletOutput("bcf_map"),
                             absolutePanel(id = "controls", 
                                           class = "panel panel-default", fixed = TRUE,
                                           draggable = TRUE, 
                                           top = 90, left = "auto", 
                                           right = 40, bottom = "auto",
                                           width = 250, height = "auto" ,
                                           h3(tags$b("Please click a dot!"))
                                           )
                            
                            )
                        )
                )
        ) # ,

bsModal(title = "Bloom vs. CP", 
          id = "precip_graphs", 
          trigger = NULL, 
          size = "large",
          fluidPage(fluidRow(column(2, 
                                    radioButtons("precip_plot_climate_proj", 
                                                 label = h4("Climate Projection"), 
                                                 choices = list("RCP 4.5" = "rcp45", 
                                                                "RCP 8.5" = "rcp85"),
                                                 selected = "rcp85"),

                                    radioButtons("precip_plot_time_scale", 
                                                 label = h4("Time Scale"), 
                                                 choices = list("Daily" = "day", 
                                                                "Weekly" = "week", 
                                                                "Monthly" = "month"),
                                                 selected = "day")),
                             column(10, offset = 0, plotOutput("precip_plot", height = 550))
                            )
                    )
         ) # ,
  

######################################################
######################################################
#
#                   server part
#
######################################################
######################################################
precip_map_data <- reactive({
    
      filter(spatial_precip, 
             climate_proj == input$precip_map_climate_proj,
             group == input$precip_map_climate_group, 
             exceedance == input$precip_map_exceedance,
             time_scale == input$precip_map_time_scale)

  })

precip_data_month <- reactive({

    # Test if location is selected
    validate(
      need(!is.null(input$precip_map_marker_click$id), 
            "Please select a location")
    )

    # load data
    readRDS(paste0("/data/pruett/precip/", 
                    input$precip_plot_time_scale, 
                    "_prob_month/", 
                    input$precip_map_marker_click$id, 
                    ".rds")) %>% 
      filter(climate_proj == input$precip_plot_climate_proj)
  })

precip_data_octmar <- reactive({

    # Test if location is selected
    validate(
      need(!is.null(input$precip_map_marker_click$id), "Please select a location")
    )
    
    # load data
    readRDS(paste0("/data/pruett/precip/", 
                   input$precip_plot_time_scale, 
                   "_prob_octmar/", 
                   input$precip_map_marker_click$id, 
                   ".rds")) %>% 
      filter(climate_proj == input$precip_plot_climate_proj)
    
  })

 ## Build Map ####
  output$precip_map <- renderLeaflet({
    pal <- colorBin(palette = "plasma", reverse = TRUE,
                    domain = precip_map_data()$prob_median, 
                    bins = 8, pretty=TRUE)
    
    leaflet() %>%
    addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
             attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
    addPolygons(data = skagit, fill = FALSE, stroke = 1, color = 'black') %>% 
    addPolygons(data = whatcom, fill = FALSE, stroke = 1, color = 'black') %>% 
    addPolygons(data = snohomish, fill = FALSE, stroke = 1, color = 'black') %>% 
    setView(lat = 48.35, lng = -121.5, zoom = 8) %>%
    addCircleMarkers(data = precip_map_data(), lng = ~ lng, lat = ~ lat,
                     label = ~ file_name,
                     layerId = ~ file_name,
                     radius = 6,
                     color = ~ pal(prob_median),
                     stroke  = FALSE,
                     fillOpacity = .95) # %>% 
    # addLegend("bottomleft", pal = pal, values = NULL, 
    #           title = "Difference from Exceedance Probability") 
      
  })

 ## Observe Map Input ##
  observeEvent(input$precip_map_marker_click, {
    
    toggleModal(session, modalId = "precip_graphs", toggle = "open")
    
  })

output$precip_plot <- renderPlot({
      p_month <- plot_monthly_prob(precip_data_month(), "Daily Probability")
      p_octmar <- plot_octmar_prob(precip_data_octmar())
      
      plot_grid(p_month, p_octmar, 
                nrow = 1, align = "vh", 
                rel_widths = c(4, 1), axis = 'b')
    
  }, res = 140)
#########################################################
#########################################################
#
# Global part
#
#########################################################
#########################################################
data_dir <- "/data/hnoorazar/bloom_thresh_frost/"

bloom_f_name <- "fullbloom_50percent_day.rds"
frost_f_name <- "first_frost.rds"
CP_f_name <- "sept_summary_comp.rds"

bloom_dt <- readRDS(paste0(data_dir, bloom_f_name)) %>% 
            group_by(location, lat, lng)

frost_dt <- readRDS(paste0(data_dir, frost_f_name)) %>% 
            group_by(location, lat, lng)

thresh_dt <- readRDS(paste0(data_dir, CP_f_name)) %>% 
            group_by(location, lat, lng)


