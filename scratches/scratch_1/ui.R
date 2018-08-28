library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("First Scratch Shinny App"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       h3("slider input"),
       h1("This is h1."),
       h2("This is h2."),
       h6("This is h6."),
       sliderInput("slider_1_input", "slide me", min=0, max=100, value=0)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       code("This is some code."),
       textOutput("output_slider_1")
       
    )
  )
))
