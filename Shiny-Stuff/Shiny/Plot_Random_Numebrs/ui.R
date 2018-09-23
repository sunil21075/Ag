#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Plot Random Numebrs"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       numericInput("numeric", "How Many Random Numbers Should be plotted?",
                    value = 1000, min =1, max=1000, step=1),
       sliderInput("sliderX", "Puck Minimum and maxumum X values", 
                   min=-100, max=100, value=c(-50, 50)),
       sliderInput("sliderY", "Puck Minimum and maxumum Y values",
                   min=-100, max=100, value=c(-50, 50)),
       checkboxInput("show_xlab", "Show/Hide X axis label", value=TRUE),
       checkboxInput("show_ylab", "Show/Hide X axis label", value=TRUE),
       checkboxInput("show_title", "Show/Hide X axis label", value=TRUE)
       
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3("Graph of random points"),
      plotOutput("distPlot")
    )
  )
))
