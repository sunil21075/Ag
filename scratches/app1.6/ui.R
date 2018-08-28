library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Predict horsepower from MPG"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("slider_mpg", "What is the MPG of the car?", min=10, max=35, value=20),
      checkboxInput("show_model_1", "Show/Hide model 1", value=TRUE),
      checkboxInput("show_model_2", "Show/Hide model 2", value=TRUE),
      submitButton("Submit!")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("plot_1"),
       
       h4("Predicted horsepower from model 1:"),
       textOutput("pred_1"),
       
       h4("Predicted horsepower from model 2:"),
       textOutput("pred_2")
              )
  )
))
