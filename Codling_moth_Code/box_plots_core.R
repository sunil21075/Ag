library(chron)
library(data.table)
library(ggplot2)
library(reshape2)
library(dplyr)
library(foreach)
library(iterators)

source_path = "/home/hnoorazar/cleaner_codes/core.R"
source(source_path)



plot_some_plot <- function(input_dir, file_name, version="rcp45", plot_path, output_name){
	output_name = paste0(output_name, "_", version, ".png")
	file_name <- paste0(input_dir, file_name, version, ".rds")
	data <- data.table(readRDS(file_name))
	data = data[, .(PercAdultGen1 = median(PercAdultGen1), 
                    PercAdultGen2 = median(PercAdultGen2), 
                    PercAdultGen3 = median(PercAdultGen3), 
                    PercAdultGen4 = median(PercAdultGen4), 
                    PercLarvaGen1 = median(PercLarvaGen1), 
                    PercLarvaGen2 = median(PercLarvaGen2), 
                    PercLarvaGen3 = median(PercLarvaGen3), 
                    PercLarvaGen4 = median(PercLarvaGen4), 
                    CumDDinC = median(CumDDinC), 
                    CumDDinF = median(CumDDinF)), 
                    by = c("ClimateGroup", "year", "month", "day", "dayofyear")]
    data <- subset(data, select = c("ClimateGroup", "month", 
                                    "PercAdultGen1", "PercAdultGen2", "PercAdultGen3", "PercAdultGen4", 
                                    "PercLarvaGen1", "PercLarvaGen2", "PercLarvaGen3", "PercLarvaGen4"))

    data_melted = melt(data, c("ClimateGroup", "month"), 
                             c("PercAdultGen1", "PercAdultGen2", "PercAdultGen3", "PercAdultGen4", 
                               "PercLarvaGen1", "PercLarvaGen2", "PercLarvaGen3", "PercLarvaGen4"), 
                             variable.name = "Generations")

    p = ggplot(data = data_melted, aes(x = Generations, y = value, fill = ClimateGroup)) +
        geom_boxplot() + coord_flip() +
        facet_wrap(~month)
    ggsave(output_name, p, path=plot_path)

    # saveRDS(p, paste0(data_dir, "/", "popplot.rds"))
    # saveRDS(data, paste0(data_dir, "/", "subData.rds"))
}




#data = data[, .(PercAdultGen1 = median(PercAdultGen1), 
#                PercAdultGen2 = median(PercAdultGen2), 
#                PercAdultGen3 = median(PercAdultGen3), 
#                PercAdultGen4 = median(PercAdultGen4), 
#                PercLarvaGen1 = median(PercLarvaGen1), 
#                PercLarvaGen2 = median(PercLarvaGen2), 
#                PercLarvaGen3 = median(PercLarvaGen3), 
#                PercLarvaGen4 = median(PercLarvaGen4), 
#                CumDDinC = median(CumDDinC), 
#                CumDDinF = median(CumDDinF)), 
#                by = c("ClimateGroup", "year", "month", "day", "dayofyear")]
#data <- subset(data, select = c("ClimateGroup", "month", 
#		                         "PercAdultGen1", "PercAdultGen2", "PercAdultGen3", "PercAdultGen4", 
#		                         "PercLarvaGen1", "PercLarvaGen2", "PercLarvaGen3", "PercLarvaGen4"))

#data_melted = melt(data, c("ClimateGroup", "month"), 
#                         c("PercAdultGen1", "PercAdultGen2", "PercAdultGen3", "PercAdultGen4", 
#                           "PercLarvaGen1", "PercLarvaGen2", "PercLarvaGen3", "PercLarvaGen4"), 
#                         variable.name = "Generations")

#p = ggplot(data = data_melted, aes(x = Generations, y = value, fill = ClimateGroup)) +
#  geom_boxplot() + coord_flip() +
#  facet_wrap(~month)

#saveRDS(p, paste0(data_dir, "/", "popplot.rds"))
# saveRDS(data, paste0(data_dir, "/", "subData.rds"))

plot_generations_Aug23 <- function(input_dir, file_name, box_width=.25, plot_path, output_name){
	output_name = paste0(output_name, "_", version, ".png")
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
    medians_vec <- medians$med

	p = ggplot(data = data, aes(x = ClimateGroup, y = NumAdultGens, fill = ClimateGroup)) + 
	    geom_boxplot(outlier.shape = NA, notch=TRUE, width=.2) +
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
	          axis.title.y = element_text(face = "plain", size = 12, margin = margin(t = 0, r = 10, b = 0, l = 0)),
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
    ggsave(output_name, p, path=plot_path)
}








###################  Aug. 23   ###################

##################         
################## No. of Larva Generations 
##################
stage = "Larva"
file_name = "generations_combined_CMPOP_rcp85.rds"
plot_No_generations(input_dir,
                    file_name,
                    stage,
                    end_line = "Aug23",
                    box_width=.25,
                    plot_path,
                    version="rcp85",
                    color_ord)

file_name = "generations_combined_CMPOP_rcp45.rds"
plot_No_generations(input_dir,
                    file_name,
                    stage,
                    end_line = "Aug23",
                    box_width=.25,
                    plot_path,
                    version="rcp45",
                    color_ord)
##################
################## No. of Adult Generations
##################
stage = "Adult"
file_name = "generations_combined_CMPOP_rcp85.rds"
plot_No_generations(input_dir,
                    file_name,
                    stage,
                    end_line = "Aug23",
                    box_width=.25,
                    plot_path,
                    version="rcp85",
                    color_ord)

file_name = "generations_combined_CMPOP_rcp45.rds"
plot_No_generations(input_dir,
                    file_name,
                    stage,
                    end_line = "Aug23",
                    box_width=.25,
                    plot_path,
                    version="rcp45",
                    color_ord)

###################  Nov. 5   ###################

##################
################## No. of Larva Generations
##################
stage = "Larva"
file_name = "generations1_combined_CMPOP_rcp85.rds"
plot_No_generations(input_dir,
                    file_name,
                    stage,
                    end_line = "Nov5",
                    box_width=.25,
                    plot_path,
                    version="rcp85",
                    color_ord)

file_name = "generations1_combined_CMPOP_rcp45.rds"
plot_No_generations(input_dir,
                    file_name,
                    stage,
                    end_line = "Nov5",
                    box_width=.25,
                    plot_path,
                    version="rcp45",
                    color_ord)

##################
################## No. of Adult Generations
##################
stage = "Adult"
file_name = "generations1_combined_CMPOP_rcp85.rds"
plot_No_generations(input_dir,
                    file_name,
                    stage,
                    end_line = "Nov5",
                    box_width=.25,
                    plot_path,
                    version="rcp85",
                    color_ord)

file_name = "generations1_combined_CMPOP_rcp45.rds"
plot_No_generations(input_dir,
                    file_name,
                    stage,
                    end_line = "Nov5",
                    box_width=.25,
                    plot_path,
                    version="rcp45",
                    color_ord)

