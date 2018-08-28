library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$output_slider_1 = renderText(input$slider_1_input ^ 2)
})
