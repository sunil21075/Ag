#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Slider App"),
  sidebarLayout(
    sidebarPanel(
      h1("Move the Slider"),
      sliderInput("slider2", "Slide Me!", min=0, max=1000, value=10)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3("Slider Value:"),
      textOutput("text1")
    )
  )
))
