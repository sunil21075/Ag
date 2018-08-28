library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # create splie
  mtcars$mpgsp = ifelse(mtcars$mpg -20 > 0, mtcars$mpg - 20, 0)
  
  # fit models:
  model_1 = lm(hp ~ mpg, data = mtcars)
  model_2 = lm(hp ~ mpgsp + mpg, data = mtcars)
  
  # predict horsepower for the input provided via slider bar
  model_1_pred = reactive({
    mpg_input = input$slider_mpg
    predict(model_1, newdata = data.frame(mpg = mpg_input))
  })
   
  # predict horsepower for the input provided via slider bar
  model_2_pred = reactive({
    mpg_input = input$slider_mpg
    predict(model_2, newdata = data.frame(mpg = mpg_input,
                                          mpgsp = ifelse(mpg_input - 20 > 0, mpg_input - 20, 0)))
  })
  
  output$plot_1 = renderPlot({ 
    plot(mtcars$mpg, mtcars$hp, xlab = "Miles Per Gallon", 
                                ylab = "Horsepower", 
                                bty = "n", 
                                pch = 16,
                                xlim = c(10, 35), ylim = c(50, 350))
    
    if(input$show_model_1){
      abline(model_1, col = "red", lwd = 3)
    }
    
    if(input$show_model_2){
      model_2_lines = predict(model_2, 
                            newdata = data.frame(mpg = 10:35, mpgsp = ifelse(10:35 - 20 > 0, 10:35 - 20, 0))
                            )
      lines(10:35, model_2_lines, col = "blue", lwd = 5)
    }
    
    legend(25, 250, c("Model 1 Prediction", "Model 2 Prediction"), pch = 16, 
           col = c("red", "blue"), bty = "n", cex = 1.2)
    
    mpg_input = input$slider_mpg
    points(mpg_input, model_1_pred(), col = "red", pch = 16, cex = 2)
    points(mpg_input, model_2_pred(), col = "blue", pch = 16, cex = 2)
  })
  
  output$pred_1 = renderText({model_1_pred()})
  output$pred_2 = renderText({model_2_pred()})
  
})
