library(shiny)

shinyUI(fluidPage(
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
))
