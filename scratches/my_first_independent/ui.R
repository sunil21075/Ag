library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Tabs!"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("box_1_input", "Enter tab 1 input", min = 1, max = 100, value = 100),
      numericInput("box_2_input", "Enter tab 2 input", min = 1, max = 100, value = 2),
      numericInput("box_3_input", "Enter tab 3 input", min = 1, max = 100, value = 3)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Tab 1", br(), plotOutput("tab_1_plot")), 
                  tabPanel("Tab 2", br(), textOutput("output_2")),
                  tabPanel("Tab 3", br(), textOutput("output_3"))
      )
    )
  )
))
