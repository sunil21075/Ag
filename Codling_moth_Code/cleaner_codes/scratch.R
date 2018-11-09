input_dir = "/Users/hn/Desktop/Kirti/check_point/my_aeolus_rds/"
input1 = "generations_combined_CMPOP_rcp45.rds"
input2 = "generations_combined_CMPOP_rcp85.rds"

plot_path = "/Users/hn/Documents/GitHub/Kirti/Codling_moth_Code/cleaner_codes/plots/"
plot_output_name1 = "Adult_Gen_Aug23_rcp45"
plot_output_name2 = "Adult_Gen_Aug23_rcp85"



plot_adult_generations_Aug23 <- function(input_dir, 
	                               file_name, 
	                               box_width=.25, 
	                               plot_path, 
	                               plot_output_name, 
	                               color_ord = c("grey70", "dodgerblue", "olivedrab4", "red")
	                               ){
	#
	# This function does not run on Aeolus with R.3.2.2. 
	# I will produce it on my computer.
	#
	file_name <- paste0(input_dir, file_name)
	data <- data.table(readRDS(file_name))
	data$CountyGroup = as.character(data$CountyGroup)
	data[CountyGroup == 1]$CountyGroup = 'Cooler Areas'
	data[CountyGroup == 2]$CountyGroup = 'Warmer Areas'
	  
	data = data[, .(NumAdultGens = NumAdultGens),
	                    by = c("ClimateGroup", "CountyGroup", "year", "month", "day", "dayofyear")]

	data <- subset(data, select = c("ClimateGroup", "CountyGroup", "NumAdultGens"))
    
  ######
  ###### Compute medians of each group to annotate in the plot, if possible!!!
  ######
	df <- data.frame(data)
  df <- (df %>% group_by(CountyGroup, ClimateGroup))
  medians <- (df %>% summarise(med = median(NumAdultGens)))
    # medians_vec <- medians$med
	p = ggplot(data = data, aes(x = ClimateGroup, y = NumAdultGens, fill = ClimateGroup)) + 
	    geom_boxplot(#outlier.shape = NA, 
	    	         outlier.size=0,
	    	         notch=TRUE, width=.2) +
	    theme_bw() +
	    scale_x_discrete(expand=c(0, 2), limits = levels(data$ClimateGroup[1])) +
	    # scale_y_discrete(limits=c(1, 2, 3, 4),labels=levels(data_melted$ClimateGroup)) +
	    # scale_x_discrete(limits=color_ord,labels=c("hESC1","hESC2","hESC3","hESC4")) +
	    labs(x="Time Period", y="Number of Adult Generations by August 23", color = "Climate Group") +
	    facet_wrap(~CountyGroup) +
	    theme(legend.position="bottom", 
	          legend.margin=margin(t=0, r=0, b=0, l=0, unit='cm'),
	          # plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
	          legend.title = element_blank(),
	          panel.grid.major = element_line(size = 0.1),
	          # panel.grid.major = element_blank(),
	          # panel.grid.minor = element_blank(),
	          axis.text = element_text(face = "plain", size = 10),
	          axis.title.x = element_text(face = "plain", size = 12, margin = margin(t = 10, r = 0, b = 0, l = 0)),
	          axis.title.y = element_text(face = "plain", size = 12, margin = margin(t = 0, r = 1, b = 0, l = 0)),
	          # axis.title.y = element_blank(),
	          # axis.text.y  = element_blank(),
	          # axis.ticks.y = element_blank()
	      ) +
	    scale_fill_manual(values=color_ord,
	                      name="Time\nPeriod", 
	                      labels=c("Historical","2040","2060","2080")) + 
	    scale_color_manual(values=color_ord,
	                      name="Time\nPeriod", 
	                      limits = color_ord,
	                      labels=c("Historical","2040","2060","2080")) + 
	    geom_text(data = medians, 
	    	       aes(label = sprintf("%1.1f", medians$med), y=medians$med), 
	    	       size=2, 
	    	       position =  position_dodge(.09),
	    	       vjust = -1.5) +
	    coord_flip()
    ggsave(paste0(plot_output_name, ".png"), p, path=plot_path, device = "png", width = 5.877, height = 4.406, units = "in")

}
#####################################################################################
#######################                                   ###########################
#######################       Larva Generation Aug 23     ###########################
#######################                                   ###########################
#####################################################################################
rm(list=ls())
library(chron)
library(data.table)
library(ggplot2)
library(reshape2)
library(dplyr)
library(foreach)
library(iterators)

input_dir = "/Users/hn/Desktop/Kirti/check_point/my_aeolus_rds/"
file_name = "generations_combined_CMPOP_rcp45.rds"
plot_path = "/Users/hn/Desktop/Kirti/check_point/my_aeolus_rds/"
plot_name = "fuck_me"

plot_generations_Aug23 <- function(input_dir,
                                   file_name,
                                   stage,
                                   box_width=.25,
                                   plot_path,
                                   version = "rcp45",
                                   color_ord = c("grey70", "dodgerblue", "olivedrab4", "red")
){
  #
  # This function does not run on Aeolus with R.3.2.2. 
  # I will produce it on my computer.
  #
  file_name <- paste0(input_dir, file_name)
  data <- data.table(readRDS(file_name))
  data$CountyGroup = as.character(data$CountyGroup)
  data[CountyGroup == 1]$CountyGroup = 'Cooler Areas'
  data[CountyGroup == 2]$CountyGroup = 'Warmer Areas'
  
  if (stage=="Larva"){var = "NumLarvaGens"
  } else {var = "NumAdultGens"}
  
  data <- subset(data, select = c("ClimateGroup", "CountyGroup", var))
  
  ######
  ###### Compute medians of each group to annotate in the plot, if possible!!!
  ######
  df <- data.frame(data)
  df <- (df %>% group_by(CountyGroup, ClimateGroup))
  medians <- (df %>% summarise(med = median(!!sym(var))))
  rm(df)
  box_plot = ggplot(data = data, aes(x = ClimateGroup, y = !!sym(var), fill = ClimateGroup)) + 
    geom_boxplot(#outlier.shape = NA, 
      outlier.size=0,
      notch=TRUE, width=.2) +
    theme_bw() +
    # The bigger the nimber in expand below, the smaller the space between y-ticks
    scale_x_discrete(expand=c(0, 3), limits = levels(data$ClimateGroup[1])) +
    labs(x="Time Period", y=paste0("Number of ", stage, " Generations by August 23"), color = "Climate Group") +
    facet_wrap(~CountyGroup) +
    theme(legend.position="bottom", 
          legend.margin=margin(t=-.1, r=0, b=0, l=0, unit='cm'),
          # plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          legend.title = element_blank(),
          panel.grid.major = element_line(size = 0.1),
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          axis.text = element_text(face = "plain", size = 10),
          axis.title.x = element_text(face = "plain", size = 10, margin = margin(t = 1, r = 0, b = 0, l = 0)),
          axis.text.x = element_text(size = 7),
          axis.title.y = element_text(face = "plain", size = 10, margin = margin(t = 0, r = 1, b = 0, l = 0)),
          #axis.title.y = element_blank(),
          axis.text.y  = element_blank(),
          axis.ticks.y = element_blank()
    ) +
    scale_fill_manual(values=color_ord, name="Time\nPeriod") + 
    scale_color_manual(values=color_ord, name="Time\nPeriod", limits = color_ord) + 
    geom_text(data = medians, 
              aes(label = sprintf("%1.1f", medians$med), y=medians$med), 
              size=1.75, 
              position =  position_dodge(.09),
              vjust = -1) +
    coord_flip()
  
  plot_name = paste0(stage, version, "_Generations_by_Aug23")
  ggsave(paste0(plot_name, ".png"), box_plot, path=plot_path, device="png", width=4.5, height=3.1, units = "in")
}
color_ord = c("grey70", "dodgerblue", "olivedrab4", "red")
plot_Larva_generations_Aug23(input_dir, 
                             file_name, 
                             box_width=.25, 
                             plot_path, 
                             plot_name, 
                             color_ord)




#####################################################################################
#######################                                   ###########################
#######################          Diapause Plots           ###########################
#######################                                   ###########################
#####################################################################################
rm(list=ls())
library(chron)
library(data.table)
library(ggplot2)
library(reshape2)
library(dplyr)
library(foreach)
library(iterators)

plot_abs_diapause <- function(input_dir, file_name_extension, version, plot_path){
  ##
  ## input_dir 
  ## file_name_extension, 
  ## version either rcp45 or rcp85
  ## plot_path 
  ## 
  file_name = paste0(input_dir, file_name_extension)
  data <- readRDS(file_name)
  data$CountyGroup = as.character(data$CountyGroup)
  data[CountyGroup == 1]$CountyGroup = 'Cooler Areas'
  data[CountyGroup == 2]$CountyGroup = 'Warmer Areas'
  data <- data[variable =="AbsLarvaPop" | variable =="AbsNonDiap"]
  data <- subset(data, select=c("ClimateGroup", "CountyGroup", "CumulativeDDF", "variable", "value"))
  data$variable <- factor(data$variable)
  
  diap_plot <- ggplot(data, aes(x=CumulativeDDF, y=value, color=variable, fill=factor(variable))) + 
                theme_bw() +
                facet_grid(. ~ CountyGroup ~ ClimateGroup, scales = "free") +
                labs(x = "Cumulative Degree (in F)", y = "Absolute Population", color = "Absolute Population") +
                theme(axis.text = element_text(face= "plain", size = 8),
                      axis.title.x = element_text(face= "plain", size = 12, margin = margin(t=10, r = 0, b = 0, l = 0)),
                      axis.title.y = element_text(face= "plain", size = 12, margin = margin(t=0, r = 10, b = 0, l = 0)),
                      legend.position="bottom"
                      ) + 
                scale_fill_manual(labels = c("Total", "Escape Diapause"), values=c("grey", "orange"), name = "Absolute Population") +
                scale_color_manual(labels = c("Total", "Escape Diapause"), values=c("grey", "orange"), guide = FALSE) +
                stat_summary(geom="ribbon", fun.y=function(z) { quantile(z,0.5) }, 
                                            fun.ymin=function(z) { 0 }, 
                                            fun.ymax=function(z) { quantile(z,0.9) }, alpha=0.7)+
                scale_x_continuous(limits = c(0, 4000))

  plot_name = paste0("diapause_abs_", version, ".png")
  ggsave(plot_name, diap_plot, device="png", path=plot_path, width=6.2, height=5.14, unit="in")
}



rm(list=ls())
library(chron)
library(data.table)
library(ggplot2)
library(reshape2)
library(dplyr)
library(foreach)
library(iterators)
data = readRDS("/Users/hn/Desktop/Kirti/check_point/my_aeolus_rds/diapause_rel_data_rcp45.rds")

data$CountyGroup = as.character(data$CountyGroup)
data[CountyGroup == 1]$CountyGroup = 'Cooler Areas'
data[CountyGroup == 2]$CountyGroup = 'Warmer Areas'
data <- data[variable =="RelLarvaPop" | variable =="RelNonDiap"]
data <- subset(data, select=c("ClimateGroup", "CountyGroup", "CumulativeDDF", "variable", "value"))
data$variable <- factor(data$variable)


pp = ggplot(data, aes(x=CumulativeDDF, y=value, color=variable, fill=factor(variable))) + 
      theme_bw() +
      facet_grid(. ~ CountyGroup ~ ClimateGroup, scales = "free") +
      labs(x = "Cumulative Degree (in F)", y = "Relative Population", color = "Relative Population") +
      theme(axis.text = element_text(face= "plain", size = 8),
            axis.title.x = element_text(face= "plain", size = 12, margin = margin(t=10, r = 0, b = 0, l = 0)),
            axis.title.y = element_text(face= "plain", size = 12, margin = margin(t=0, r = 10, b = 0, l = 0)),
            legend.position="bottom"
            ) + 
      scale_fill_manual(labels = c("Total", "Escape Diapause"), values=c("grey", "orange"), name = "Relative Population") +
      scale_color_manual(labels = c("Total", "Escape Diapause"), values=c("grey", "orange"), guide = FALSE) +
      stat_summary(geom="ribbon", fun.y=function(z) { quantile(z,0.5) }, 
                                  fun.ymin=function(z) { 0 }, 
                                  fun.ymax=function(z) { quantile(z,0.9) }, alpha=0.7)+
      scale_x_continuous(limits = c(0, max(data$CumulativeDDF)+10)) 
