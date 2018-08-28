library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$out_1 = renderText(input$box_1)
  output$out_2 = renderText(input$box_2)
  output$out_3 = renderText(input$box_3)
})
