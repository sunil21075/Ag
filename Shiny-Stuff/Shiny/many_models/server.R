library(shiny)

shinyServer(function(input, output) {
  model = reactive({
    brushed_data = brushedPoints(trees, input$brush_1, xvar = "Girth", yvar = "Volume")
    if (nrow(brushed_data) < 2) {return (NULL)}
    
    lm(Volume ~ Girth, data = brushed_data)
  })
    
    output$slope_out = renderText({
      if (is.null(model())) {"No model found"}
      else {model()[[1]][2]}
    })
    
    output$intercept_out = renderText({
      if (is.null(model())) {"No model found"}
      else {model()[[1]][1]}
    })
    
    output$plot_1 = renderPlot({
      plot(trees$Girth, trees$Volume, xlab = "Girth", ylab = "Volume", main = "Tree Measurements", cex = 1.5, pch=16, bty = 'n')
      
      if(!is.null(model())){
        abline(model(), col='blue', lwd = 2)
        
      }
    })
})
