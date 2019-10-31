           tabPanel(tags$b("Bloom vs. Chill Portion"),
                    fluidPage( id = "nav", inverse=FALSE, 
                               fluid=FALSE, title="Tool",
                               div( class="outer",
                                    tags$head(includeCSS("styles.css"),
                                              includeScript("gomap.js")
                                              ),
                                    leafletOutput("bcf_map_front_page", 
                                                   width="100%", height="100%")
                                  )
                              ),
                    fluidPage(bsModal( "Graphs", trigger = NULL, title = "", size = "large",
                                       dashboardPage( dashboardHeader(title = "Plots"),
                                                      dashboardSidebar(
                                                                       selectInput(inputId = "detail_level",
                                                                                   label = tags$b("Detail Level"),
                                                                                   choices = detail_levels, 
                                                                                   selected = detail_levels[1]
                                                                                   ),
                                                                       conditionalPanel(condition = "input.detail_level == 'all_models'",
                                                                                        radioButtons(inputId = "arrow_emission",
                                                                                                     label = tags$b("Scenario"),
                                                                                                     choices = emissions, 
                                                                                                     selected = emissions[1]
                                                                                                     )
                                                                                        ),

                                                                       # Only show this panel if the level of detail is more_details
                                                                       conditionalPanel(condition = "input.detail_level == 'more_details'",
                                                                                        radioButtons(inputId = "emission", 
                                                                                                     label = tags$b("Scenario"),
                                                                                                     choices = emissions, 
                                                                                                     selected = emissions[1]),

                                                                                        radioButtons(inputId = "time_period", 
                                                                                                     label = tags$b("Time Period"),
                                                                                                     choices = time_periods,
                                                                                                     selected = time_periods[3]),

                                                                                        selectInput(inputId = "climate_model", 
                                                                                                    label = tags$b("Climate Model"),
                                                                                                    choices = climate_models, 
                                                                                                    selected = climate_models[1]
                                                                                                    )
                                                                                        ) 
                                                                       
                                                                       ),
                                                      #####################
                                                      #
                                                      # End of side bar of dashboard of analog maps
                                                      #
                                                      #####################
                                                      dashboardBody(tags$head(tags$style(HTML('.content-wrapper, 
                                                                                               .right-side {
                                                                                               background-color: #252d38;
                                                                                                 }
                                                                                              ')
                                                                                        )
                                                                              ),
                                                                    plotOutput("Plot"), # ,
                                                                    br(), br(),
                                                                    br(), br(),
                                                                    br(), br(),
                                                                    br(), br(),
                                                                    br(), br(),
                                                                    p(tags$b(span("All Models Analogs", style = "color:white")),
                                                                      (span(" plot includes most similar", style = "color:white")),
                                                                      (span(" county in all models.", style = "color:white"))
                                                                      ),
                                                                    
                                                                    p((span("A county with", style = "color:white")),
                                                                      tags$b(span("red", style = "color:red")),
                                                                      (span(" border is in the future.", style = "color:white"))
                                                                      ), 
                                                                    
                                                                    p((span("A county with", style = "color:white")),
                                                                      tags$b(span("yellow", style = "color:#fff200")), # "color:GoldenRod"
                                                                      (span(" border is the best analog", style = "color:white")),
                                                                      (span(" for a given county.", style = "color:white"))
                                                                      )
                                                                    )
                                                    )
                                      )
                              )
                    )
