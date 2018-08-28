library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Plor Random Numbers"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("numeric_input_1", "Enter number of sample points:", min = 10, max = 1000, value=200),
      sliderInput("x_range", "X range:", min = -100, max = 100, value = c(-30, 30)),
      sliderInput("y_range", "Y range:", min = -100, max = 100, value = c(-30, 30)),
      checkboxInput("x_label", "Show X label", value = TRUE),
      checkboxInput("y_label", "Show Y label", value = TRUE)
    )
    ,
    
    # Show a plot of the generated distribution
    mainPanel(
      h3("Graph of random points"),
      plotOutput("rand_no_plot")
    )
  )
))
