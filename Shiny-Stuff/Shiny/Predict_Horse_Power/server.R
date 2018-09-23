
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  mtcars$mpgsp = ifelse(mtcars$mpg - 20 > 0, mtcars$mpg - 20, 0)
  model_1 = lm(hp~ mpg, data=mtcars)
  model_2 = lm(hp~ mpgsp + mpg, data=mtcars)
  
  model_1_pred = reactive({
    mpgInput = input$sliderMPG
    predict(model_1, newdata = data.frame(mpg = mpgInput))
  })
  
  model_2_pred = reactive({
    mpgInput = input$sliderMPG
    predict(model_2, newdata = data.frame(mpg=mpgInput,
                                          mpgsp=ifelse(mpgInput - 20 > 0,
                                                       mpgInput - 20, 0)))
  })
  output$distPlot = renderPlot({
    mpgInput = input$sliderMPG
    plot(mtcars$mpg, mtcars$hp, xlab="MPG",
         ylac = "Horsepower", bty = "n", pch=16,
         xlim = c(10,35), ylim=c(50,350))
    
    if(input$Show_Model_1){
          abline(model_1, col="red", lwd=2)
           }
    
    if (input$Show_Model_2){
          model_2_lines = predict(model_2, 
                              newdata = data.frame(mpg=10:35, 
                              mpgsp=ifelse(10:35 - 20 > 0, 10:35 - 20 , 0)
                              ))
          lines(10:35, model_2_lines, col="blue", lwd=2)
    }
    
    legend(25, 250, c("Model 1 prediction", "model 2 prediction"), pch=16, col=c("red", "blue"), 
            bty="n", cex = 1.2)
    points(mpgInput, model_1_pred(), col="red", pch=16, cex=2)
    points(mpgInput, model_2_pred(), col="red", pch=16, cex=2)
    
    output$prediction_1 = renderText({model_1_pred()})
    
    output$prediction_2 = renderText({model_2_pred()})
    
  })
  
})
