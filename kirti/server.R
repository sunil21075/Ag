library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   output$output_1 = renderText(input$box_1_input)
   output$output_2 = renderText(input$box_2_input)
   output$output_3 = renderText(input$box_3_input)
})
