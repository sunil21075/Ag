rm(list=ls())
library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr) # for ggarrange

data_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/chilling/7_time_intervals_data/"
month_names = c("Jan", "Feb", "Mar", "Sept", "Oct", "Nov", "Dec")

plot_dens <- function(data, month_name){
	color_ord = c("grey70", "dodgerblue", "olivedrab4", "red")
	the_theme <- theme_bw() + 
               theme(plot.margin = unit(c(t=0.1, r=0.1, b=.1, l=0.1), "cm"),
                     panel.border = element_rect(fill=NA, size=.3),
                     plot.title = element_text(hjust = 0.5),
                     plot.subtitle = element_text(hjust = 0.5),
                     panel.grid.major = element_line(size = 0.05),
                     panel.grid.minor = element_blank(),
                     panel.spacing=unit(.25,"cm"),
                     legend.position="bottom", 
                     legend.title = element_blank(),
                     legend.key.size = unit(1, "line"),
                     legend.text=element_text(size=7),
                     legend.margin=margin(t= -0.1, r = 0, b = 0, l = 0, unit = 'cm'),
                     legend.spacing.x = unit(.05, 'cm'),
                     strip.text.x = element_text(size = 10),
                     axis.ticks = element_line(color = "black", size = .2),
                     #axis.text = element_text(face = "plain", size = 2.5, color="black"),
                     axis.title.x = element_text(face = "plain", size=10, 
                     	                            margin = margin(t=4, r=0, b=0, l=0)),
                     # axis.title.x=element_blank(),
                     axis.text.x = element_text(size = 6, face = "plain", color="black"),
                     axis.ticks.x = element_blank(),
                     axis.title.y = element_text(face = "plain", size = 10, 
                                                 margin = margin(t=0, r=2, b=0, l=0)),
                     axis.text.y = element_text(size = 6, face="plain", color="black")
                     # axis.title.y = element_blank()
                     )
	obs_plot = ggplot(data, aes(x=Temp, fill=factor(ClimateGroup))) + 
               geom_density(alpha=.5) + 
               facet_grid( ~ CountyGroup) +
               ylab("Density") +
               xlab("Hourly temp.") + 
               ggtitle(label = paste0("The density of hourly temp. in the month of ", month_name, ".")) +
               scale_fill_manual(values=color_ord,
                      name="Time\nPeriod", 
                      labels=c("Historical","2040's","2060's","2080's")) + 
               scale_color_manual(values=color_ord,
                       name="Time\nPeriod", 
                       limits = color_ord,
                       labels=c("Historical","2040's","2060's","2080's")) + 
               the_theme
    return(obs_plot)
}

for (month in month_names){
	data = data.table(readRDS(paste0(data_dir, month, ".rds")))

	# data$ClimateGroup[data$Year >= 1950 & data$Year <= 2005] <- "Historical"
	data$ClimateGroup[data$Year <= 2005] <- "Historical"
	data$ClimateGroup[data$Year > 2025 & data$Year <= 2055] <- "2040's"
	data$ClimateGroup[data$Year > 2045 & data$Year <= 2075] <- "2060's"
	data$ClimateGroup[data$Year > 2065] <- "2080's"

	# There are years between 2006 and 2015 which ... becomes NA
	data = na.omit(data)

	# order the climate groups
	data$ClimateGroup <- factor(data$ClimateGroup, 
		                        levels = c("Historical", "2040's", "2060's", "2080's"))

    assign(x = paste0(month, "_density_plot"),
		   value = { plot_dens(data=data,
		   	                   month_name=month)})
	data_45 = data %>% filter(scenario %in% c("historical", "rcp45"))
	data_85 = data %>% filter(scenario %in% c("historical", "rcp85"))

	assign(x = paste0(month, "_density_plot_", "rcp45"),
		   value = { plot_dens(data=data_45,
		   	                   month_name=month)})
	assign(x = paste0(month, "_density_plot_", "rcp85"),
		   value = { plot_dens(data=data_85,
		   	                   month_name=month)})
}

#######
####### RCP45
#######

big_plot_45 <- ggarrange(Sept_density_plot_rcp45, 
	                     Oct_density_plot_rcp45,
	                     Nov_density_plot_rcp45,
	                     Dec_density_plot_rcp45,
	                     Jan_density_plot_rcp45,
	                     Feb_density_plot_rcp45,
	                     Mar_density_plot_rcp45,
	                     label.x = "Hourly temp.",
	                     label.y = "Density",
	                     ncol = 1, 
	                     nrow = 7, 
	                     common.legend = T,
	                     legend = "bottom")
ggsave(filename = "density_45.png", 
	   path = "/Users/hn/Desktop/", 
	   plot = big_plot_45,
       width = 15, height = 40, units = "in",
       dpi=400, 
       device = "png")

#######
####### RCP85
#######

big_plot_85 <- ggarrange(Sept_density_plot_rcp85, 
	                     Oct_density_plot_rcp85,
	                     Nov_density_plot_rcp85,
	                     Dec_density_plot_rcp85,
	                     Jan_density_plot_rcp85,
	                     Feb_density_plot_rcp85,
	                     Mar_density_plot_rcp85,
	                     label.x = "Hourly temp.",
	                     label.y = "Density",
	                     ncol = 1, 
	                     nrow = 7, 
	                     common.legend = T,
	                     legend = "bottom")
ggsave(filename = "density_85.png", 
	   path = "/Users/hn/Desktop/", 
	   plot = big_plot_85,
       width = 15, height = 40, units = "in",
       dpi=400, 
       device = "png")

#######
####### RCP 45 and85 combined
#######
big_plot_combined <- ggarrange(Sept_density_plot, 
	                     Oct_density_plot,
	                     Nov_density_plot,
	                     Dec_density_plot,
	                     Jan_density_plot,
	                     Feb_density_plot,
	                     Mar_density_plot,
	                     label.x = "Hourly temp.",
	                     label.y = "Density",
	                     ncol = 1, 
	                     nrow = 7, 
	                     common.legend = T,
	                     legend = "bottom")
ggsave(filename = "45_and_85_combined.png", 
	   path = "/Users/hn/Desktop/", 
	   plot = big_plot_combined,
       width = 15, height = 40, units = "in",
       dpi=400, 
       device = "png")



