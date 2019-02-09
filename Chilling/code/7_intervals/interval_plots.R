rm(list=ls())
library(data.table)
library(dplyr)
library(ggplot2)

##############################
############################## Global variables
##############################
data_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/chilling/7_time_intervals/"

# iof = interval of interest
iof = c(c(-Inf, -2), c(-2, 4), 
        c(4, 6), c(6, 8), 
        c(8, 13), c(13, 16), 
        c(16, Inf))
iof_breaks = c(-Inf, -2, 4, 6, 8, 13, 16, Inf)

# These are the order of months in climate calendar!
month_no = c(1, 2, 3, 9, 10, 11, 12)
month_names = c("Jan", "Feb", "Mar",
	              "Sept", "Oct", "Nov", "Dec"
                )

weather_type = c("Warmer", "Cooler")

plot_intervals <- function(data, month_name){
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
                     legend.margin=margin(t= -.3, r = 0, b = 0, l = 0, unit = 'cm'),
                     legend.spacing.x = unit(.05, 'cm'),
                     strip.text.x = element_text(size = 10),
                     axis.ticks = element_line(color = "black", size = .2),
                     #axis.text = element_text(face = "plain", size = 2.5, color="black"),
                     axis.title.x = element_text(face = "plain", size=10, 
                     	                            margin = margin(t=4, r=0, b=0, l=0)),
                     # axis.title.x=element_blank(),
                     axis.text.x = element_text(size = 6, face = "plain", 
                     	                          color="black", angle=-30),
                     axis.ticks.x = element_blank(),
                     axis.title.y = element_text(face = "plain", size = 10, 
                                                 margin = margin(t=0, r=.1, b=0, l=0)),
                     axis.text.y = element_text(size = 6, face="plain", color="black")
                     # axis.title.y = element_blank()
                     )
	obs_plot = ggplot(data = data) +
             geom_point(aes(x = Year, y = no_hours, fill = factor(scenario)),
	           	          alpha = 0.25, shape = 21, size = 1) +
             geom_smooth(aes(x = Year, y = no_hours, color = factor(scenario)),
				                 method = "lm", se = F, size=.4) +
             facet_grid( ~ CountyGroup ~ temp_cat) +
				     scale_color_viridis_d(option = "plasma", begin = 0, end = .7,
				                           name = "Model scenario", 
                                   aesthetics = c("color", "fill")) +
             ylab("No. of hours in a given temp. interval") +
             xlab("Year") +
             ggtitle(label = "No. of hours in Intervals of Interest (IoI)",
                     subtitle = month_name) +
             scale_fill_manual(values = c("yellow", "red", "blue")) +
             the_theme
    return(obs_plot)
}

for(month in month_names){
	data = paste0(data_dir, month, ".rds")
  data = data.table(readRDS(data))
  data <- data %>% 
          mutate(temp_cat=cut(Temp, breaks=iof_breaks)) %>% 
          group_by(Chill_season, Year, model, Month, scenario, temp_cat, CountyGroup) %>% 
          summarise(no_hours = n())
  
  data$temp_cat = factor(data$temp_cat, order=T)
	assign(x = paste0(month, "_plot"),
		     value = { plot_intervals(data=data,
		   	                          month_name=month)})

	data = paste0(data_dir, "observed_" ,month, ".rds")
  data = data.table(readRDS(data))
  data <- data %>% 
          mutate(temp_cat=cut(Temp, breaks=iof_breaks)) %>% 
          group_by(Chill_season, Year, Month, scenario, temp_cat, CountyGroup) %>% 
          summarise(no_hours = n())
  data$temp_cat = factor(data$temp_cat, order=T)
	assign(x = paste0("observed_", month, "_plot"),
		     value = { plot_intervals(data=data,
		   	                          month_name=month)})
}

library(ggpubr)
big_plot <- ggarrange(Sept_plot, observed_Sept_plot,
                      Oct_plot, observed_Oct_plot,
                      Nov_plot, observed_Nov_plot,
                      Dec_plot, observed_Dec_plot,
                      Jan_plot, observed_Jan_plot,
                      Feb_plot, observed_Feb_plot,
                      Mar_plot, observed_Mar_plot,
                      label.x = "Year",
                      label.y = "No. of hours in a given temp. interval",
                      ncol = 1, 
                      nrow = 14, 
                      common.legend = T,
                      legend = "bottom")

ggsave(filename = "7_intervals.png", 
	   path = "/Users/hn/Desktop/", 
	   plot = big_plot,
       width = 15, height = 40, units = "in",
       dpi=400, 
       device = "png")

ggsave(plot = Sept_plot, 
	   filename = paste0("Sept_plot", ".png"),
	   path = "/Users/hn/Desktop/",
	   device = "png",
       height = 5, width = 8, units = "in", dpi=400)

ggsave(plot = Oct_plot, 
	   filename = paste0("Oct_plot", ".png"),
	   path = "/Users/hn/Desktop/",
	   device = "png",
       height = 5, width = 8, units = "in", dpi=400)

ggsave(plot = Nov_plot, 
	   filename = paste0("Nov_plot", ".png"),
	   path = "/Users/hn/Desktop/",
	   device = "png",
       height = 5, width = 8, units = "in", dpi=400)

ggsave(plot = Dec_plot,
       filename = paste0("Dec_plot", ".png"),
	     path = "/Users/hn/Desktop/",
	     device = "png",
       height = 5, width = 8, units = "in", dpi=400)

ggsave(plot = Jan_plot,
	   filename = paste0("Jan_plot", ".png"),
	   path = "/Users/hn/Desktop/",
	   device = "png",
       height = 5, width = 8, units = "in", dpi=400)

ggsave(plot = Feb_plot,
	   filename = paste0("Feb_plot", ".png"),
	   path = "/Users/hn/Desktop/",
	   device = "png",
       height = 5, width = 8, units = "in", dpi=400)

ggsave(plot = Mar_plot, 
	   filename = paste0("Mar_plot", ".png"), 
	   path = "/Users/hn/Desktop/",
	   device = "png",
       height = 5, width = 8, units = "in", dpi=400)

ggsave(plot = observed_Sept_plot,
	filename = paste0("observed_Sept_plot", ".png"),
	   path = "/Users/hn/Desktop/",
	   device = "png",
       height = 5, width = 8, units = "in", dpi=400)

ggsave(plot = observed_Oct_plot,
	filename = paste0("observed_Oct_plot", ".png"),
	   path = "/Users/hn/Desktop/",
	   device = "png",
       height = 5, width = 8, units = "in", dpi=400)

ggsave(plot = observed_Nov_plot,
	filename = paste0("observed_Nov_plot", ".png"),
	   path = "/Users/hn/Desktop/",
	   device = "png",
       height = 5, width = 8, units = "in", dpi=400)

ggsave(plot = observed_Dec_plot, 
	filename = paste0("observed_Dec_plot", ".png"),
	   path = "/Users/hn/Desktop/",
	   device = "png",
       height = 5, width = 8, units = "in", dpi=400)

ggsave(plot = observed_Jan_plot,
	filename = paste0("observed_Jan_plot", ".png"),
	   path = "/Users/hn/Desktop/",
	   device = "png",
       height = 5, width = 8, units = "in", dpi=400)

ggsave(plot = observed_Feb_plot,
	filename = paste0("observed_Feb_plot", ".png"),
	   path = "/Users/hn/Desktop/",
	   device = "png",
       height = 5, width = 8, units = "in", dpi=400)

ggsave(plot = observed_Mar_plot,
	     filename = paste0("observed_Mar_plot", ".png"),
	     path = "/Users/hn/Desktop/",
	     device = "png",
       height = 5, width = 8, units = "in", dpi=400)


