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
  titlePanel("Predict Horsepower from MPG"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("sliderMPG",
                   "What is the MPG of the car?",
                   min = 10,
                   max = 35,
                   value = 20),
       checkboxInput("Show_Model_1", "Show/Hide Model 1", value=TRUE),
       checkboxInput("Show_Model_2", "Show/Hide Model 2", value=TRUE),
       submitButton("Submit") # New. comment out so the code works continuously
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot"),
       h3("Predicted Horsepower from model 1"),
       textOutput("prediction_1"),
       h3("Predicted Horsepower from model 2"),
       textOutput("prediction_2")
       
    )
  )
))
