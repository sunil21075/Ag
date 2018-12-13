####################################################################################

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


param_name_shift = "CodlingMothparameters_0.1.txt"
params_shift = read.table (paste0(param_dir, param_name_shift), header=TRUE, sep=",")


colors_1 = c("green", "blue", "red", "gray")

pw <- function(x, shape=sh, scale=sc){
    pweibull(x, shape=sh, scale=sc)
}
sh = params[5, 3]
sc = params[5, 4]
the_theme = theme(panel.grid.major = element_blank(), 
                  # panel.grid.minor = element_blank(),
                   axis.text.x = element_text(size = 8),
                   axis.title.x = element_text(face = "plain", 
                                               size=11, 
                                               margin = margin(t=10, r=0, b=0, l=0)),
                   axis.text.y = element_text(size = 8),
                   axis.title.y = element_text(face = "plain", 
                                               size=11, 
                                               margin = margin(t=0, r=10, b=0, l=0)),
                   legend.position="bottom"
                  )

ggplot(data.frame(x=0), aes(x = x)) +
stat_function(fun = pw, color="red", linetype=2) + 
xlim(-100, 4000) + 
theme_bw() + 
labs(x = "Degree Days", 
     y = "Weibull Distribution") + the_theme
########################################################################
########################################################################
x_limits = c(-100, 6000)
y_limits = c(0, 0.003)

dw_egg_1 <- function(x, shape=params[1, "shape"], scale=params[1, "scale"]){
    dweibull(x, shape=shape, scale=scale)
}

dw_egg_2 <- function(x, shape=params[2, "shape"], scale=params[2, "scale"]){
    dweibull(x, shape=shape, scale=scale)
}

dw_egg_3 <- function(x, shape=params[3, "shape"], scale=params[3, "scale"]){
    dweibull(x, shape=shape, scale=scale)
}

dw_egg_4 <- function(x, shape=params[4, "shape"], scale=params[4, "scale"]){
    dweibull(x, shape=shape, scale=scale)
}


dw_egg_1_shift <- function(x, shape=params_shift[1, "shape"], scale=params_shift[1, "scale"]){
    dweibull(x, shape=shape, scale=scale)
}

dw_egg_2_shift <- function(x, shape=params_shift[2, "shape"], scale=params_shift[2, "scale"]){
    dweibull(x, shape=shape, scale=scale)
}

dw_egg_3_shift <- function(x, shape=params_shift[3, "shape"], scale=params_shift[3, "scale"]){
    dweibull(x, shape=shape, scale=scale)
}

dw_egg_4_shift <- function(x, shape=params_shift[4, "shape"], scale=params_shift[4, "scale"]){
    dweibull(x, shape=shape, scale=scale)
}

colorss = c("grey70", "dodgerblue", "olivedrab4", "red", "grey70", "dodgerblue", "olivedrab4", "red")
labelss = c("Gen. 1", "Gen. 2", "Gen. 3", "Gen. 4", "Gen. 1", "Gen. 2", "Gen. 3", "Gen. 4")

ggplot(data.frame(x=0), aes(x = x)) +
#legend("topleft", labelss, fill=colorss) + 
stat_function(fun = dw_egg_1, color="olivedrab4", linetype=1) + 
stat_function(fun = dw_egg_2, color="dodgerblue", linetype=1) + 
stat_function(fun = dw_egg_3, color="red", linetype=1) + 
stat_function(fun = dw_egg_4, color="grey70", linetype=1) + 
stat_function(fun = dw_egg_1_shift, color="olivedrab4", linetype=2) + 
stat_function(fun = dw_egg_2_shift, color="dodgerblue", linetype=2) + 
stat_function(fun = dw_egg_3_shift, color="red", linetype=2) + 
stat_function(fun = dw_egg_4_shift, color="grey70", linetype=2) + 
theme_bw() + 
scale_x_continuous(name="Degree Days", limits=x_limits) +
scale_y_continuous(name="Weibull Distribution", limits=y_limits, labels = function(x) format(x*1000)) +
scale_color_manual(name = "Functions",
                   labels = labelss, 
                   values = colorss)+
scale_fill_manual(name = "Functions",
                   labels = labelss, 
                   values = colorss)+
the_theme
