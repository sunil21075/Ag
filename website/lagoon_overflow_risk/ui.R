# Lagoon

library(leaflet)
library(shinyBS)
library(shiny)
library(plotly)
library(shinydashboard)

navbarPage(title = div("",
                       img(src='csanr_logo.png', style='width:100px;height:35px;'), 
                       img(src='WSU-DAS-log.png', style='width:100px;height:35px;'),
                       img(src='NW-Climate-Hub-Logo.jpg', style='width:100px;height:35px;')
                       ),
           id="nav", 
           windowTitle = "Lagoon Overflow Risk",
           #
           ############## Home Begin
           #
           tabPanel(tags$b("Home"),
                    navlistPanel(tabPanel(tags$b("About"), 
                                          tags$div(style="width:950px", 
                                                   includeHTML("home-page/about.html")
                                                   )
                                          ),
                                 
                                 tabPanel(tags$b("People"), 
                                          tags$div(style="width:950px", 
                                                   includeHTML("home-page/people.html")
                                                   )
                                          ),
                                 tabPanel(tags$b("Codling Moth Life Cycle and Management"), 
                                          tags$div(style = "width: 950px", 
                                                   includeHTML("home-page/life-cycle.html")
                                                   )
                                          ),

                                 tabPanel(tags$b("Climate Data"), 
                                          tags$div(style="width:950px", 
                                                   includeHTML("home-page/climate-change-projections.html")
                                                   )
                                          ),

                                 tabPanel(tags$b("What's the story?"), 
                                                 tags$div(style="width: 950px", 
                                                          includeHTML("home-page/changing-pest-pressures.html")
                                                          )
                                                 ),

                                 tabPanel(tags$b("Contact"), 
                                          tags$div(style="width:950px", 
                                               includeHTML("home-page/contact.html")
                                               )
                                          ),

                                 tabPanel(tags$b("Take a tour! (video)"), 
                                          tags$div(style="width:950px", 
                                                   includeHTML("home-page/take-a-tour.html")
                                                   )
                                          ),
                                 widths = c(2,10)
                                 )
                    ),
           #
           ############## Home End
           #
           ############## Regional Plots START
           #
           tabPanel(tags$b("Regional Plots"),
                    navlistPanel(
                                 ###### Subregion Groups start
                                 # tabPanel(tags$b("Subregion Map"),
                                 #          tags$div(style="width:950px", 
                                 #                   includeHTML("./HTML_plots/subregion_groups.html")
                                 #                   )
                                 #          ),

                                 tabPanel(tags$b("Subregion Map"),
                                          tags$h4(paste0("We have clustered the grids ", 
                                                         "into 5 subregions based on ", 
                                                         "elevation and average annual ", 
                                                         "precipitation.")),
                                          imageOutput("location_group"),
                                          tags$h4(paste0("The variables", 
                                                         " distribution is given below ", 
                                                         "and they correspond to ", 
                                                         "historical data:")),
                                          imageOutput("cluster_visualization")
                                          ),
                                 ####### Subregion Groups End
                                 HTML("<b>RCP 8.5</b>"),
                                 
                                 ####### Western Coastal 85 start
                                 tabPanel("Western Coastal",
                                          fluidRow(tabBox(tabPanel("Annual", 
                                                                   # tags$div(style="width:950px", includeHTML("HTML_plots/Western_Coastal_85.html")),
                                                                   tags$blockquote("Below we see annual precipitation ranges."),

                                                                   imageOutput("Western_Coastal_Annual_rain_rcp85"),
                                                                   br(),br(), br(),br(),br(), br(),
                                                                   br(),br(), br(),br(),br(), br(),
                                                                   tags$blockquote("Below we see annual runoff ranges."),
                                                                   imageOutput("Western_Coastal_Annual_runoff_rcp85"),
                                                                   br(),br(),
                                                                   tags$blockquote("and finally 25-year/24-hour design storm intensity"),
                                                                   imageOutput("Western_Coastal_dsi_rcp85")
                                                                   ),
                                                          tabPanel("Seasonal", imageOutput("Western_Coastal_Seasonal_rcp85"), height=700),
                                                          tabPanel("Monthly", imageOutput("Western_Coastal_Monthly_rcp85"), height=700),
                                                          width = 12
                                                          )
                                                  )
                                          ),
                                 ####### Western Coastal 85 END

                                 ####### Cascade foothills 85 start
                                 tabPanel("Cascade foothills",
                                          fluidRow(tabBox(tabPanel("Annual", imageOutput("Cascade_foothills_Annual_rcp85"), height = 700),
                                                          tabPanel("Seasonal", imageOutput("Cascade_foothills_Seasonal_rcp85"), height=700),
                                                          tabPanel("Monthly", imageOutput("Cascade_foothills_Monthly_rcp85"), height=700),
                                                          width = 12
                                                          )
                                                  )
                                          ),
                                 ####### Cascade foothills 85 END

                                 ####### Northwest Cascades 85 start
                                 tabPanel("Northwest Cascades",
                                          fluidRow(tabBox(tabPanel("Annual", imageOutput("Northwest_Cascades_Annual_rcp85"), height = 700),
                                                          tabPanel("Seasonal", imageOutput("Northwest_Cascades_Seasonal_rcp85"), height=700),
                                                          tabPanel("Monthly", imageOutput("Northwest_Cascades_Monthly_rcp85"), height=700),
                                                          width = 12
                                                          )
                                                  )
                                          ),
                                 ####### Northwest Cascades 85 END

                                 ####### Northcentral Cascades 85 start
                                 tabPanel("Northcentral Cascades",
                                          fluidRow(tabBox(tabPanel("Annual", imageOutput("Northcentral_Cascades_Annual_rcp85"), height = 700),
                                                          tabPanel("Seasonal", imageOutput("Northcentral_Cascades_Seasonal_rcp85"), height=700),
                                                          tabPanel("Monthly", imageOutput("Northcentral_Cascades_Monthly_rcp85"), height=700),
                                                          width = 12
                                                          )
                                                  )
                                          ),
                                 ####### Northcentral Cascades 85 END

                                 ####### Northeast Cascades 85 start
                                 tabPanel("Northcentral Cascades",
                                          fluidRow(tabBox(tabPanel("Annual", imageOutput("Northeast_Cascades_Annual_rcp85"), height = 700),
                                                          tabPanel("Seasonal", imageOutput("Northeast_Cascades_Seasonal_rcp85"), height=700),
                                                          tabPanel("Monthly", imageOutput("Northeast_Cascades_Monthly_rcp85"), height=700),
                                                          width = 12
                                                          )
                                                  )
                                          ),
                                 ####### Northeast Cascades 85 END
                                 #######
                                 #######
                                 #######     RCP 4.5
                                 #######
                                 #######
                                 HTML("<b>RCP 4.5</b>"),

                                 
                                 ####### Western Coastal 45 start
                                 tabPanel("Western Coastal",
                                          fluidRow(tabBox(tabPanel("Annual", imageOutput("Western_Coastal_Annual_rcp45"), height = 700),
                                                          tabPanel("Seasonal", imageOutput("Western_Coastal_Seasonal_rcp45"), height=700),
                                                          tabPanel("Monthly", imageOutput("Western_Coastal_Monthly_rcp45"), height=700),
                                                          width = 12
                                                          )
                                                  )
                                          ),
                                 ####### Western Coastal 45 END

                                 ####### Cascade foothills 45 start
                                 tabPanel("Cascade foothills",
                                          fluidRow(tabBox(tabPanel("Annual", imageOutput("Cascade_foothills_Annual_rcp45"), height = 700),
                                                          tabPanel("Seasonal", imageOutput("Cascade_foothills_Seasonal_rcp45"), height=700),
                                                          tabPanel("Monthly", imageOutput("Cascade_foothills_Monthly_rcp45"), height=700),
                                                          width = 12
                                                          )
                                                  )
                                          ),
                                 ####### Cascade foothills 45 END

                                 ####### Northwest Cascades 45 start
                                 tabPanel("Northwest Cascades",
                                          fluidRow(tabBox(tabPanel("Annual", imageOutput("Northwest_Cascades_Annual_rcp45"), height = 700),
                                                          tabPanel("Seasonal", imageOutput("Northwest_Cascades_Seasonal_rcp45"), height=700),
                                                          tabPanel("Monthly", imageOutput("Northwest_Cascades_Monthly_rcp45"), height=700),
                                                          width = 12
                                                          )
                                                  )
                                          ),
                                 ####### Northwest Cascades 45 END

                                 ####### Northcentral Cascades 45 start
                                 tabPanel("Northcentral Cascades",
                                          fluidRow(tabBox(tabPanel("Annual", imageOutput("Northcentral_Cascades_Annual_rcp45"), height = 700),
                                                          tabPanel("Seasonal", imageOutput("Northcentral_Cascades_Seasonal_rcp45"), height=700),
                                                          tabPanel("Monthly", imageOutput("Northcentral_Cascades_Monthly_rcp45"), height=700),
                                                          width = 12
                                                          )
                                                  )
                                          ),
                                 ####### Northcentral Cascades 45 END

                                 ####### Northeast Cascades 45 start
                                 tabPanel("Northcentral Cascades",
                                          fluidRow(tabBox(tabPanel("Annual", imageOutput("Northeast_Cascades_Annual_rcp45"), height = 700),
                                                          tabPanel("Seasonal", imageOutput("Northeast_Cascades_Seasonal_rcp45"), height=700),
                                                          tabPanel("Monthly", imageOutput("Northeast_Cascades_Monthly_rcp45"), height=700),
                                                          width = 12
                                                          )
                                                  )
                                          ),
                                 ####### Northeast Cascades 45 END


                                 widths = c(2,10)
                                 )
                   ),
           #
           ############## Regional Plots END
           #
           #
           ############## Bloom, CP, frost Map Front Page start
           #
           tabPanel(tags$b("Some Insight"),
                    fluidPage( id = "nav", inverse=FALSE, 
                               fluid=FALSE, title="Tool",
                               div( class="outer",
                                    tags$head(includeCSS("styles.css"),
                                              includeScript("gomap.js")
                                              ),
                                    leafletOutput("lagoon_map", 
                                                   width="100%", 
                                                   height="100%")
                                  )
                              ),
                    fluidPage(bsModal(
                                       id="lagoon_graphs",
                                       trigger = NULL,
                                       title = "",
                                       size = "large",                                       
                                       dashboardPage( dashboardHeader(title = "Plots"),
                                                      dashboardSidebar(
                                                                       radioButtons(inputId = "em_scenario",
                                                                                    label = tags$b("Scenario"),
                                                                                    choices = c("RCP 8.5" = "RCP 8.5",
                                                                                                "RCP 4.5" = "RCP 4.5"), 
                                                                                    selected = "RCP 8.5"),
                                                                       radioButtons(inputId="lagoon_plot_fruit_type", 
                                                                                    label = h4("Fruit type"), 
                                                                                    choices = list("Cripps Pink" = "Cripps_Pink", 
                                                                                                   "Gala" = "Gala", 
                                                                                                   "Red Delicious" = "Red_Deli"),
                                                                                    selected = "Cripps_Pink")
                                                                       ),
                                                      #####################
                                                      #
                                                      # End of side bar of dashboard of analog maps
                                                      #
                                                      #####################
                                                      dashboardBody(
                                                                     tags$head(tags$style(HTML('.content-wrapper, 
                                                                                               .right-side {
                                                                                               background-color: #252d38;
                                                                                                 }'
                                                                                              ))),
                                                                     # plotOutput("lagoon_plot")
                                                                     fluidPage(fluidRow(column(2, offset = 0, 
                                                                                               plotOutput("lagoon_plot", height = 550)))
                                                                              )

                                                                    )
                                                    )
                                      )
                              )
                    )
       )
