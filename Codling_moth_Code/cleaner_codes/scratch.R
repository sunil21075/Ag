input_dir = "/Users/hn/Desktop/Kirti/check_point/my_aeolus_rds/"
input1 = "generations_combined_CMPOP_rcp45.rds"
input2 = "generations_combined_CMPOP_rcp85.rds"

plot_path = "/Users/hn/Documents/GitHub/Kirti/Codling_moth_Code/cleaner_codes/plots/"
plot_output_name1 = "Adult_Gen_Aug23_rcp45"
plot_output_name2 = "Adult_Gen_Aug23_rcp85"

plot_generations_Aug23 (input_dir, 
                        input1, 
                        box_width=.25, 
                        plot_path, 
                        plot_output_name1, color_ord = c("grey70", "dodgerblue", "olivedrab4", "red"))

plot_generations_Aug23 (input_dir, 
                        input2, 
                        box_width=.25, 
                        plot_path, 
                        plot_output_name2, color_ord = c("grey70", "dodgerblue", "olivedrab4", "red"))


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