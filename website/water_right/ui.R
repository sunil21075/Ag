# Water Rights

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
           windowTitle = "Water Rights",
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
                            
                                 # tabPanel(tags$b("Climate Data"), 
                                 #          tags$div(style="width:950px", 
                                 #                   includeHTML("home-page/climate-change-projections.html")
                                 #                   )
                                 #          ),

                                 # tabPanel(tags$b("What's the story?"), 
                                 #                 tags$div(style="width: 950px", 
                                 #                          includeHTML("home-page/changing-pest-pressures.html")
                                 #                          )
                                 #                 ),

                                 tabPanel(tags$b("Contact"), 
                                          tags$div(style="width:950px", 
                                               includeHTML("home-page/contact.html")
                                               )
                                          ),
                                 widths = c(2,10)
                                 )
                    ),
           #
           ############## Home End
           #
           #
           ############## Water Right map start
           #
           tabPanel(tags$b("Water Right"),
                    div(class="outer",
                        tags$head(includeCSS("styles.css")),
                        leafletOutput("water_right_map", width="100%", height="100%"),
                        # absolutePanel(id = "controls", 
                        #               class = "panel panel-default", 
                        #               fixed = TRUE,
                        #               draggable = TRUE, 
                        #               top = 60, right = 700,
                        #               left = "auto", bottom = "auto",
                        #               width = 250, height = "auto",
                        #               h3(tags$b("Select a Date \n")),
                        #               h3("Earlier in red, later in blue"),
                        #               numericInput("year_input", "Select a Year", min = 1800, max = 2015, value = 1800),
                        #               numericInput("month_input", "Select a Month", min = 1, max = 12, value = 1),
                        #               numericInput("day_input", "Select a Day", min = 1, max = 30, value = 1)
                        #               ),

                        absolutePanel(id = "sliderControl2", 
                                      class = "panel panel-default", 
                                      fixed = TRUE,
                                      draggable = TRUE, 
                                      top = 60, right = 20,
                                      left = "auto", bottom = "auto",
                                      width = 330, height = "auto",
                                      # h3(tags$b("Select a Date \n")),
                                      h4("Earlier in red, later in blue"),
                                      sliderInput("cut_date",
						                          "Dates:",
						                          min = as.Date("1800-01-01","%Y-%m-%d"),
						                          max = as.Date("2015-12-30","%Y-%m-%d"),
						                          value=as.Date("1800-01-01"),
						                          timeFormat="%Y-%m-%d")
                                      )
                        )
                    )
           ############## Water Right map END
           #
           #
           #

       )
