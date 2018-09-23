rm(list=ls())
library(shiny)
library(leaflet)
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()
# Define UI for application that draws a histogram
shinyUI(fluidPage(
        leafletOutput("mymap"), 
        p(), 
        actionButton("recalc", "New points"))
)
