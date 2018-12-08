rm(list=ls())
library(chron)
library(EnvStats)
# library(data.table)
library(reshape2)
library(dplyr)
library(foreach)
library(iterators)
library(ggplot2)
par(mar=c(1,1,1,1))
dev.off() 
param_dir = "/Users/hn/Documents/GitHub/Kirti/codling_moth/code/parameters/"
param_name = "CodlingMothparameters.txt"
params = read.table (paste0(param_dir, param_name), header=TRUE, sep=",")
colors_1 = c("black", "blue", "red", "gray")

for (i in 5:8){
  shap = params[i, 3]
  scal = params[i, 4]
  if (i %in% 1:4)  {
    colors = "blue"
  }
  else if (i %in% 5:8) {
    colors = "black"
  }
  else if  (i %in% 9:12){
    colors = "red"
  }
  else {colors = "green"}
  curve(dweibull(x, shape=shap, scale=scal), xlim = c(-100, 6000),
        ylim = c(0, .004),
        col=colors,
        lty=1,
        add=TRUE)
}

for (i in 5:8){
  shap = params[i, 3]
  scal = params[i, 4] * (1.2)
  if (i %in% 1:4)  {
    colors = "blue"
  }
  else if (i %in% 5:8) {
    colors = "black"
  }
  else if  (i %in% 9:12){
    colors = "red"
  }
  else {colors = "green"}
  curve(dweibull(x, shape=shap, scale=scal), xlim = c(-100, 6000),
        ylim = c(0, .004),
        col=colors,
        lty=2,
        add=TRUE)
}

##########################################
pw <- function(x, shape=sh, scale=sc){
    pweibull(x, shape=sh, scale=sc)
}

sh = 2
sc = 1000
ggplot(data.frame(x=0), aes(x = x)) +
stat_function(fun = pw, color="red", linetype=2) + 
xlim(-100, 4000) + 
theme_bw() + 
labs(x = "Degree Days", 
     y = "pweibull", 
     color = "Climate Group") + 
theme(
 panel.grid.major = element_blank(), 
 # panel.grid.minor = element_blank(),
 axis.title.x = element_text(face = "plain", 
                             size=10, 
                             margin = margin(t=3, r=0, b=0, l=0))
)
