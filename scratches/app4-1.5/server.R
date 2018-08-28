#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$rand_no_plot = renderPlot({
    data_x = runif(input$numeric_input_1, input$x_range[1], input$x_range[2])
    data_y = runif(input$numeric_input_1, input$y_range[1], input$y_range[2])
    x_label = ifelse(input$x_label, "X Axis", "")
    y_label = ifelse(input$y_label, "Y Axis", "" )
    plot(data_x, data_y, xlab = x_label, ylab = y_label,
         xlim = c(-100, 100), ylim = c(-100, 100))

  })
  
})
