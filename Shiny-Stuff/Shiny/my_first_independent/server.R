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
  
  output$tab_1_plot = renderPlot({
    data_x = runif(input$box_1_input, -5, 5)
    data_y = data_x ^ 2
    plot(data_x, data_y, xlab = "x_label", ylab = "y_label", main = "x^2 plot",
         xlim = c(-10, 10), ylim = c(0, 50))
    
  })
  
  output$output_2 = reactive({input$box_2_input ^ 2})
  output$output_3 = renderText({input$box_3_input ^ 3})
  
  
})
