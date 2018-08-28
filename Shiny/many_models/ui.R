library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Visualize many models"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h3("Slope"),
      textOutput("slope_out"),
      
      h3("Intercept"),
      textOutput("intercept_out")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("plot_1", brush = brushOpts(id = "brush_1"))
    )
  )
))
