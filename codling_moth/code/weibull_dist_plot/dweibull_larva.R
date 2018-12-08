################## Larva dweibull
library(chron)
library(EnvStats)
library(grid)
# library(data.table)
library(reshape2)
library(dplyr)
library(foreach)
library(iterators)
library(ggplot2)
par(mar=c(1, 1, 1, 1))
dev.off() 

param_dir = "/Users/hn/Documents/GitHub/Kirti/codling_moth/code/parameters/"
param_name = "CodlingMothparameters.txt"
params = read.table(paste0(param_dir, param_name), header=TRUE, sep=",")

param_name_shift = "CodlingMothparameters_0.1.txt"
params_shift = read.table(paste0(param_dir, param_name_shift), header=TRUE, sep=",")

########################################################################
########################################################################
########################## Functions
##########################
dw_larva_1 <- function(x, shape=params[5, "shape"], scale=params[5, "scale"]){
  dweibull(x, shape=shape, scale=scale)
}

dw_larva_2 <- function(x, shape=params[6, "shape"], scale=params[6, "scale"]){
  dweibull(x, shape=shape, scale=scale)
}

dw_larva_3 <- function(x, shape=params[7, "shape"], scale=params[7, "scale"]){
  dweibull(x, shape=shape, scale=scale)
}

dw_larva_4 <- function(x, shape=params[8, "shape"], scale=params[8, "scale"]){
  dweibull(x, shape=shape, scale=scale)
}
######################### S H I F T 10 %
dw_larva_1_shift <- function(x, shape=params_shift[5, "shape"], scale=params_shift[5, "scale"]){
  dweibull(x, shape=shape, scale=scale)
}

dw_larva_2_shift <- function(x, shape=params_shift[6, "shape"], scale=params_shift[6, "scale"]){
  dweibull(x, shape=shape, scale=scale)
}

dw_larva_3_shift <- function(x, shape=params_shift[7, "shape"], scale=params_shift[7, "scale"]){
  dweibull(x, shape=shape, scale=scale)
}

dw_larva_4_shift <- function(x, shape=params_shift[8, "shape"], scale=params_shift[8, "scale"]){
  dweibull(x, shape=shape, scale=scale)
}
########################## 
########################## Functions
########################################################################
########################################################################
########################## The Theme
##########################
the_theme = theme(panel.grid.major = element_blank(), 
                  # panel.grid.minor = element_blank(),
                  axis.text.x = element_text(size = 9),
                  axis.title.x = element_text(face = "plain", 
                                              size=12, 
                                              margin = margin(t=10, r=0, b=0, l=0)),
                  axis.text.y = element_text(size = 9, angle=90),
                  axis.title.y = element_text(face = "plain", 
                                              size=12, 
                                              margin = margin(t=0, r=10, b=0, l=0)),
                  legend.position="bottom"
) + theme_bw()

x_limits = c(-100, 6000)
y_limits = c(0, 0.004)

colorss = c("grey70", "dodgerblue", "olivedrab4", "red", "grey70", "dodgerblue", "olivedrab4", "red")
labelss = c("Gen. 1", "Gen. 2", "Gen. 3", "Gen. 4", "Gen. 1", "Gen. 2", "Gen. 3", "Gen. 4")

larva_density = ggplot(data.frame(x=x_limits), aes(x=x)) + the_theme + 
			  geom_path(stat="function", fun=dw_larva_1, aes(colour="grey70"), linetype=1)+
			  geom_path(stat="function", fun=dw_larva_2, aes(colour="dodgerblue"), linetype=1)+
			  geom_path(stat="function", fun=dw_larva_3, aes(colour="olivedrab4"), linetype=1)+
			  geom_path(stat="function", fun=dw_larva_4, aes(colour="red"), linetype=1)+
			  geom_path(stat="function", fun=dw_larva_1_shift, aes(colour="grey70"), linetype=2)+
			  geom_path(stat="function", fun=dw_larva_2_shift, aes(colour="dodgerblue"), linetype=2)+
			  geom_path(stat="function", fun=dw_larva_3_shift, aes(colour="olivedrab4"), linetype=2)+
			  geom_path(stat="function", fun=dw_larva_4_shift, aes(colour="red"), linetype=2)+
			  scale_x_continuous(name="Degree days", limits=x_limits) + 
			  scale_y_continuous(name="Weibull density", limits=y_limits, labels = function(x) format(x, scientific=TRUE)) +
			  scale_colour_identity("", guide="legend", 
			                        labels = labelss, 
			                        breaks = colorss)

master_path = "/Users/hn/Documents/GitHub/Kirti/codling_moth/code/weibull_dist_plot/"
plot_path = master_path
ggsave(filename=paste0("dweibull_larva", ".png"), 
	   plot=larva_density, 
	   path=plot_path, 
	   width=7 ,
	   height=5 , 
	   dpi=1000, 
	   device="png")

