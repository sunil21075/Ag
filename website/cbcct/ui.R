
shinyUI(
  navbarPage("Climate Visualization Tool",
             tabPanel("Home",
                      fluidPage(id="nav", inverse=FALSE, fluid=FALSE,
                                title="WSU Columbia River Basin Climate Tool",
                                div(class="outer",
                                    tags$head(
                                      # Include our custom CSS
                                      includeCSS("style.css"),
                                      includeScript("gomap.js"),
                                      tags$style(type="text/css", "
                                                 #loadmessage {
                                                 position: fixed;
                                                 top: 0px;
                                                 left: 0px;
                                                 width: 100%;
                                                 padding: 5px 0px 50px 0px;
                                                 text-align: center;
                                                 font-weight: blod;
                                                 font-size: 100%;
                                                 color: #000000;
                                                 background-color: #CCFF66;
                                                 z-index: 105;
                                                 }
                                                 ")
                                      ), 
                                    leafletOutput("map", width="100%", height="100%"),
                                    absolutePanel(id="menuPanel_main", draggable = TRUE, width=330, height= "auto", left="auto",
                                                  right=20, bottom="auto", top=60, fixed=TRUE, inverse=TRUE,
                                                  h2("Map Key"),
                                                  selectInput("tileSelect", "Map Overlay", c("Satellite","Topographic", "Basic")),
                                                  selectInput("cropSelect", "Locations with:", crop_names, selectize = FALSE, multiple = TRUE, selected=crop_names)
                                    )
                                      ),
                                  bsModal(id="graphs", trigger=NULL, title="", size="large",
                                          navbarPage("Climate Trends", inverse=FALSE, fluid=FALSE,
                                                     tabPanel("GDD",
                                                              tabsetPanel(
                                                                tabPanel("Short Term and Seasonal Climate Forecast",
                                                                             sidebarLayout(position="right",
                                                                                           sidebarPanel(id="menuPanel_shortterm", width = 3,
                                                                                                        h2("Plot Parameters"),
                                                                                                        dateInput("planting_date",
                                                                                                                  label="Planting Date",
                                                                                                                  value = paste0(this_year, "-04-10")),
                                                                                                        selectInput("crop_name",
                                                                                                                    label = "Crop",
                                                                                                                    choices = selected_crops$name,
                                                                                                                    selected = default_crop_name),
                                                                                                        actionButton("gddButton", "Go!")
                                                                                           ),
                                                                                           mainPanel(width=9, class="main",
                                                                                                     p("Text can go here."),
                                                                                                     h3("Growing Degree Day Forecast"),
                                                                                                     p("Text can also go here."),
                                                                                                     plotlyOutput("gdd_forecast_plotly", width = "800px"),
                                                                                                     p("Text can also go here.")
                                                                                           )
                                                                             )
                                                                ),
                                                                tabPanel("Long Term Climate Change",
                                                                         sidebarLayout(position="right",
                                                                                         sidebarPanel(id="menuPanel_longterm", width = 3,
                                                                                                     h2("Plot Parameters"),
                                                                                                     selectInput("crop_name_longterm",
                                                                                                                 label = "Crop",
                                                                                                                 choices = selected_crops$name,
                                                                                                                 selected = default_crop_name)
                                                                                        ),
                                                                                       mainPanel(width=9, class="main",
                                                                                                 p("Text can go here."),
                                                                                                 h3("Long-Term Growing Degree Day Trend"),
                                                                                                 p("Text can also go here."),
                                                                                                 plotlyOutput("gdd_outlook_plotly", width = "800px"), #height = "400px"),
                                                                                                 p("Text can also go here.")
                                                                                       )
                                                                         )
                                                                )
                                                              )
                                                     ),
                                                     tabPanel("Temperature",
                                                              tabsetPanel(
#                                                                 tabPanel("Short Term and Seasonal Climate Forecast",
#                                                                          p("Text can also go here."),
#                                                                          h3("Temperature Forecast and growing season length"),
#                                                                          p("Text can also go here."),
#                                                                          plotlyOutput("temp_forecast", width = "800px"),
#                                                                          p("Text can also go here.")),
                                                                tabPanel("Long Term Climate Change",
                                                                         sidebarLayout(position="right",
                                                                                       sidebarPanel(id="menuPanel_shortterm", width = 3,
                                                                                                    h2("Plot Parameters"),
                                                                                                    sliderInput("heat_risk_range", "Daily Max Temperature Threshold (F)", 
                                                                                                                min = 32, max = 80, 
                                                                                                                value = 0, 
                                                                                                                sep='')
                                                                                       ),
                                                                                       mainPanel(width=9, class="main",
                                                                                                 p("In this tab, you can visualize the changes expected to daily minimum temperatures (T-min), daily maximum temperatures (T-max), and daily average temperatures (T-gvg) into the future. You can also visualize changes to the length of the growing season. The growing season length can be defined in many ways. Here, it is the length between the early season last frost event, and end of season, first frost event. You can also visualize expected changes to heat and frost risks.

                                                                                                   In the Pacific Northwest region, we expect temperatures to increase going into the future and this signal is robust across all climate model projections. This waming results in a longer available growing season. Even though a longer growing season is available, the actual time-to-maturity for crops is expected to be shorted due to accelerated growing degree day (GDD) accumulation (see GDD tab). This can have a negative impact on crop yields, but can be adapted to, by shifting to crop varieties that have longer times to maturity, or where possible, by shifting to double cropping systems."),
                                                                                                 h3("Expected changes to T-min and growing season length"),
                                                                                                 p("Text can also go here."),
                                                                                                 plotlyOutput("temp_outlook3", width = "800px"),
                                                                                                 p("Text can also go here."),
                                                                                                 h3("Expected changes to T-avg"),
                                                                                                 p("Text can also go here."),
                                                                                                 plotlyOutput("temp_outlook2", width = "800px"),
                                                                                                 p("Text can also go here."),
                                                                                                 h3("Expected changes to T-max"),
                                                                                                 p("Text can also go here."),
                                                                                                 plotlyOutput("temp_outlook1", width = "800px"),
                                                                                                 p("Text can also go here."),
                                                                                                 h3("Expected changes to  heat risk"),
                                                                                                 p("This figure shows the number of days in each month, temperatures higher than a threshold are expected.  Use the option on the side panel to change the thresholds and replot the figure."),
                                                                                                 plotlyOutput("temp_heat_risk", width = "800px"),
                                                                                                 p("Text can also go here."),
                                                                                                 h3("Expected changes to  frost risk"),
                                                                                                 p("This figure shows the number of days in each month, a frost event is expected."),
                                                                                                 plotlyOutput("temp_frost_risk", width = "800px"),
                                                                                                 p("Text can also go here.")
                                                                                       )
                                                                         ))
                                                              )
                                                      
                                                     ),
                                                     tabPanel("Precipitation",
                                                              tabsetPanel(
                                                                tabPanel("Short Term and Seasonal Climate Forecast",
                                                                         p("Text can also go here."),
                                                                         h3("Snow Forecast"),
                                                                         p("Text can also go here."),
                                                                         plotlyOutput("precip_forecast_snow",  width = "800px"),
                                                                         p("Text can also go here."),
                                                                         h3("Rain Forecast"),
                                                                         p("Text can also go here."),
                                                                         plotlyOutput("precip_forecast_rain",  width = "800px"),
                                                                         p("Text can also go here."),
                                                                         h3("Total Precipitation Forecast"),
                                                                         p("Text can also go here."),
                                                                         plotlyOutput("precip_forecast_total",  width = "800px"),
                                                                         p("Text can also go here.")),
                                                                tabPanel("Long Term Climate Change",
                                                                         sidebarLayout(position="right",
                                                                                       sidebarPanel(id="menuPanel_longterm", width = 3,
                                                                                                    h2("Plot Parameters"),
                                                                                                    sliderInput("change_risk_range", "Daily Precipitation Threshold (mm/day)", 
                                                                                                                min = 0, max = 100, 
                                                                                                                value = 0, 
                                                                                                                step = 1, 
                                                                                                                sep='')
                                                                                       ),
                                                                                       mainPanel(width=9, class="main",
                                                                                                 htmlOutput("In this tab, you can visualize the changes expected to  total precipitation, rain, and snow levels, going into the future. Total precipitaion is split into rain and snow using the Pipes and Quick linear split method. This method had limitations, but is an approximation used in several hydrology models. You can also visualize the frequency of daily precipitation events exceeding a user specified threshold. 
 
                                                                                                 In the Pacific Northwest region, we geneally expect a warming related shift in precipitation from snow to rain (decreases in snow levels and increases in rain levels). This signal is robust across multiple climate model projections. Total prepicipation is generally expected to stay about the same except perhaps certain months in early spring or winter. There is also variability among climate models in this aspect."),
                                                                                                 h3("Expected changed to snow levels"),
                                                                                                 p("Text can also go here."),
                                                                                                 plotlyOutput("precip_outlook_snow",  width = "800px"),
                                                                                                 p("Text can also go here."),
                                                                                                 h3("Expected changed to rain levels"),
                                                                                                 p("Text can also go here."),
                                                                                                 plotlyOutput("precip_outlook_rain",  width = "800px"),
                                                                                                 p("Text can also go here."),
                                                                                                 h3("Expected changes to total precipitation levels"),
                                                                                                 p("Text can also go here."),
                                                                                                 plotlyOutput("precip_outlook_total",  width = "800px"),
                                                                                                 p("Text can also go here."),
                                                                                                 h3("Expected changes to  daily precipitation event frequencies"),
                                                                                                 p("This figure shows the number of days in each month that that daily precipitation exceed a certain threshold.  Use the option on the side panel to change the thresholds and replot the figure."),
                                                                                                 plotlyOutput("precip_outlook_frequency",  width = "800px"),
                                                                                                 p("Text can also go here.")
                                                                                       )
                                                                         )
                                                                         )
                                                              )
                                                              
                                                     ),
                                                     tabPanel("Historical Similarity",
                                                              tabsetPanel(
                                                                tabPanel("Short Term and Seasonal Climate Forecast",
                                                                         h3("GDD Similarity"),
                                                                         plotlyOutput("gdd_similarity_plotly", width = "800px"),
                                                                         h3("GDD Similarity, Detrended"),
                                                                         plotlyOutput("gdd_similarity_detrended_plotly", width = "800px"),
                                                                         h3("Precipitation Similarity"),
                                                                         plotlyOutput("precip_similarity_plotly", width = "800px"),
                                                                         h3("Precipitation Similarity, Detrended"),
                                                                        plotlyOutput("precip_similarity_detrended_plotly", width = "800px"))
                                                                #tabPanel("Long Term Climate Change")
                                                              )
                                                              
                                                     ),
                                                     tabPanel("Climate Analogues",
                                                              tabsetPanel(
                                                                #tabPanel("Short Term and Seasonal Climate Forecast"),
                                                                tabPanel("Long Term Climate Change",
                                                                         sidebarLayout(position="right",
                                                                                       sidebarPanel(id="menuPanel_longterm", width = 3,
                                                                                                    h2("Analogue Parameters")
#                                                                                                     numericInput("analogue_number", "Number of Climate Analogues", 
#                                                                                                     5, min = 0, max = 100, 
#                                                                                                     step = 1) 
                                                                                       ),
                                                                                       mainPanel(width=9, class='main',
                                                                                                 p("Text can also go here."),
                                                                                                 h3("Climate Analogue Maps"),
                                                                                                 p("Maps are the 1970-2006 climate similarity to the selected location's projected climate in 2040, 2060, and 2080. Green is more similar, tan is less similar."),
                                                                                                 h4("2040"),
                                                                                                 imageOutput("image2040", width = "100%"),
                                                                                                 h4("2060"),
                                                                                                 imageOutput("image2060", width = "100%"),
                                                                                                 h4("2080"),
                                                                                                 imageOutput("image2080", width = "100%"),
                                                                                                 p("Text can also go here.")
                                                                                      )
                                                                         )
                                                                 )
                                                              )
                                                              
                                                     ),
                                                     actionButton("reset", "Start Over")
                                          )
                                  )                 
                              )
             ),
             tabPanel("Team",
                      h2("Kirti Rajagopalan"),
                      h5("<< photo >> "),
                      h5("<< description >> "),
                      
                      h2("Nicholas Potter"),
                      h5("<< photo >> "),
                      h5("<< description >> "),
                      
                      h2("Von Walden"),
                      h5("<< photo >> "),
                      h5("<< description >> "),
                      
                      
                      
                      
                      h2("Austin Schorsch"),
                      h5("<< photo >> "),
                      h5("<< description >> "),
                      
                      h2("Des Marks"),
                      h5("<< photo >> "),
                      h5("<< description >> "),
                      
                      
                      h2("Michael Antoz"),
                      h5("<< photo >> "),
                      h5("<< description >> "),
                      
                      h2("Trevor Mozingo"),
                      h5("<< photo >> "),
                      h5("<< description >> "))
  )
)




















