library(ggplot2)
library(shiny)

ui = navbarPage("Hydro Lab", inverse=TRUE, collapsable=TRUE,
                
                # Home Tab starts here
                tabPanel(title = "Home",
                         fluidRow(
                           column(6, offset = 0,  
                                 navlistPanel(
                                   
                                   tabPanel(title = "About", 
                                            br(), 
                                            bootstrapPage(htmlTemplate("about-template.html", name = "About"))
                                            ),
                                   
                                   tabPanel(title = "People", 
                                            br(),
                                            bootstrapPage(htmlTemplate("people-template.html", name = "People"))
                                            ),
                                   
                                   tabPanel(title = "Codling Moth Life Cycle and Management",
                                            br(),
                                            bootstrapPage(htmlTemplate("Codling_Moth_Template.html", name = "Moth"))
                                            ),
                                   
                                   tabPanel(title = "climate Data",
                                            br(),
                                            bootstrapPage(htmlTemplate("climate-data-template.html", name = "Climate"))
                                            ),
                                   
                                   tabPanel(title = "What's the Story?", 
                                            br(),
                                            bootstrapPage(htmlTemplate("story-template.html", name = "Story"))
                                            ),
                                   
                                   tabPanel(title = "Contact",
                                            br(),
                                            bootstrapPage(htmlTemplate("contact-template.html", name = "Story"))),
                                   
                                   tabPanel(title = "Take a Tour (video)")
                                              ) # navlistPanel ends here
                                 )
                                )
                         ), # tabPanel of Home Page ends here
                
                navbarMenu(title = "Bloom",
                           tabPanel(title = "Median Day of Year"),
                           tabPanel(title = "Difference from Historical")),
                
                
                navbarMenu(title = "CM Flight",
                           tabPanel(title = "Median Day of Year (1st Flight)"),
                           tabPanel(title = "Difference from Historical (1st Flight)"),
                           tabPanel(title = "Median Day of Year (By Generation)"),
                           tabPanel(title = "Difference from Historical (By Generation)")
                           ),
                
                navbarMenu(title = "CM Egg Hatch",
                           tabPanel(title = "Pest Risk"),
                           tabPanel(title = "Median Day of Year (By Generation)"),
                           tabPanel(title = "Difference from Historical (By Generation)")
                           ),
                
                tabPanel(title = "CM Diapause"),
                
                tabPanel(title = "Regional Plots"),
                
                tabPanel(title = "Test",
                         fluidPage(theme = "paper.css",
                           titlePanel("Tabs!"),
                           sidebarLayout(
                             sidebarPanel(
                               textInput(inputId = "box_1", label = "Enter Tab 1 Text:", value="Tab 1!"),
                               textInput(inputId = "box_2", label = "Enter Tab 2 Text:", value="Tab 2!"),
                               textInput(inputId = "box_3", label = "Enter Tab 3 Text:", value="Tab 3!")
                                          ),
                             
                             mainPanel(
                               tabsetPanel(type="tabs",
                                           tabPanel(title = "Tab 1", br(), textOutput("out_1")),
                                           tabPanel(title = "Tab 2", br(), textOutput("out_2")),
                                           tabPanel(title = "Tab 3", br(), textOutput("out_3"))
                                           )
                                       )
                                        )
                                     ) 
                         ),
                
                ## Leaflet test tab
                tabPanel(title = "Leaflet",
                         fluidPage(leafletOutput("mymap"), 
                                   p(), 
                                   actionButton("recalc", "New points")
                                   )
                         )
                ) # navbarPage Ends here




