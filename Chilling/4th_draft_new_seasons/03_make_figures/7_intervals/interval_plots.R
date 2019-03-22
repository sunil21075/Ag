rm(list=ls())
library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr) # for ggarrange
##############################
############################## Global variables
##############################
data_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/chilling/7_temp_intervals_data/"

# iof = interval of interest
iof = c(c(-Inf, -2), 
        c(-2, 4), 
        c(4, 6), 
        c(6, 8), 
        c(8, 13), 
        c(13, 16), 
        c(16, Inf))

iof_breaks = c(-Inf, -2, 4, 6, 8, 13, 16, Inf)

month_no = c(1, 2, 3, 9, 10, 11, 12)
month_names = c("Jan", "Feb", "Mar",
                "Sept", "Oct", "Nov", "Dec"
                )
weather_type = c("Warmer Area", "Cooler Area")

plot_intervals <- function(data, month_name){
  the_theme <- theme_bw() + 
               theme(plot.margin = unit(c(t=0.1, r=0.1, b=.5, l=0.1), "cm"),
                     panel.border = element_rect(fill=NA, size=.3),
                     plot.title = element_text(hjust = 0.5),
                     plot.subtitle = element_text(hjust = 0.5),
                     panel.grid.major = element_line(size = 0.05),
                     panel.grid.minor = element_blank(),
                     panel.spacing=unit(.25,"cm"),
                     legend.position="bottom", 
                     legend.title = element_blank(),
                     legend.key.size = unit(1, "line"),
                     legend.text=element_text(size=9),
                     legend.margin=margin(t= -0.2, r = 0, b = 0, l = 0, unit = 'cm'),
                     legend.spacing.x = unit(.05, 'cm'),
                     strip.text.x = element_text(size = 12),
                     axis.ticks = element_line(color = "black", size = .2),
                     axis.title.x = element_text(face = "plain", size=12, 
                                                 margin = margin(t=6, r=0, b=0, l=0)),
                     axis.text.x = element_text(size = 8, face = "plain", 
                                                color="black", angle=-30),
                     axis.title.y = element_text(face = "plain", size = 12, 
                                                 margin = margin(t=0, r=6, b=0, l=0)),
                     axis.text.y = element_text(size = 8, face="plain", color="black")
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
             xlab("year") +
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
          mutate(temp_cat = cut(Temp, breaks = iof_breaks)) %>% 
          group_by(Chill_season, Year, model, Month, scenario, temp_cat, CountyGroup) %>% 
          summarise(no_hours = n())
  
  data$temp_cat = factor(data$temp_cat, order=T)
  assign(x = paste0(month, "_plot"),
         value = { plot_intervals(data=data,
                                  month_name=month)})

  data = paste0(data_dir, "observed_", month, ".rds")
  data = data.table(readRDS(data))
  data <- data %>% 
          mutate(temp_cat=cut(Temp, breaks=iof_breaks)) %>% 
          group_by(Chill_season, Year, Month, scenario, temp_cat, CountyGroup) %>% 
          summarise(no_hours = n())
  data$temp_cat = factor(data$temp_cat, order=T)
  
  assign(x = paste0("observed_", month, "_plot"),
         value = { plot_intervals(data=data,
                                  month_name=month)})

} ## end for-loop

####################################################################
#################
#################   modeled
#################
####################################################################

big_plot <- ggarrange(Sept_plot, observed_Sept_plot,
                      Oct_plot, observed_Oct_plot,
                      Nov_plot, observed_Nov_plot,
                      Dec_plot, observed_Dec_plot,
                      Jan_plot, observed_Jan_plot,
                      Feb_plot, observed_Feb_plot,
                      Mar_plot, observed_Mar_plot,
                      label.x = "year",
                      label.y = "no. of hours in a given temp. interval",
                      ncol = 1, 
                      nrow = 14, 
                      common.legend = T,
                      legend = "bottom")

ggsave(plot = big_plot,
       filename = "7_intervals.png",
       path = "/Users/hn/Desktop/",
       width = 15, height = 45, units = "in",
       dpi=400, 
       device = "png")

ggsave(plot = Sept_plot, 
       filename = paste0("modeled_Sept", ".png"),
       path = "/Users/hn/Desktop/",
       device = "png",
       height = 5, width = 8, units = "in", dpi=400)

ggsave(plot = Oct_plot, 
       filename = paste0("modeled_Oct", ".png"),
       path = "/Users/hn/Desktop/",
       device = "png",
       height = 5, width = 8, units = "in", dpi=400)

ggsave(plot = Nov_plot, 
       filename = paste0("modeled_Nov", ".png"),
       path = "/Users/hn/Desktop/",
       device = "png",
       height = 5, width = 8, units = "in", dpi=400)

ggsave(plot = Dec_plot,
       filename = paste0("modeled_Dec", ".png"),
       path = "/Users/hn/Desktop/",
       device = "png",
       height = 5, width = 8, units = "in", dpi=400)

ggsave(plot = Jan_plot,
       filename = paste0("modeled_Jan", ".png"),
       path = "/Users/hn/Desktop/",
       device = "png",
       height = 5, width = 8, units = "in", dpi=400)

ggsave(plot = Feb_plot,
       filename = paste0("modeled_Feb", ".png"),
       path = "/Users/hn/Desktop/",
       device = "png",
       height = 5, width = 8, units = "in", dpi=400)

ggsave(plot = Mar_plot, 
       filename = paste0("modeled_Mar", ".png"), 
       path = "/Users/hn/Desktop/",
       device = "png",
       height = 5, width = 8, units = "in", dpi=400)

####################################################################
#################
#################   observed
#################
####################################################################

ggsave(plot = observed_Sept_plot,
       filename = paste0("observed_Sept", ".png"),
       path = "/Users/hn/Desktop/",
       device = "png",
       height = 5, width = 8, units = "in", dpi=400)

ggsave(plot = observed_Oct_plot,
       filename = paste0("observed_Oct", ".png"),
       path = "/Users/hn/Desktop/",
       device = "png",
       height = 5, width = 8, units = "in", dpi=400)

ggsave(plot = observed_Nov_plot,
       filename = paste0("observed_Nov", ".png"),
       path = "/Users/hn/Desktop/",
       device = "png",
       height = 5, width = 8, units = "in", dpi=400)

ggsave(plot = observed_Dec_plot, 
       filename = paste0("observed_Dec", ".png"),
       path = "/Users/hn/Desktop/",
       device = "png",
       height = 5, width = 8, units = "in", dpi=400)

ggsave(plot = observed_Jan_plot,
       filename = paste0("observed_Jan", ".png"),
       path = "/Users/hn/Desktop/",
       device = "png",
       height = 5, width = 8, units = "in", dpi=400)

ggsave(plot = observed_Feb_plot,
       filename = paste0("observed_Feb", ".png"),
       path = "/Users/hn/Desktop/",
       device = "png",
       height = 5, width = 8, units = "in", dpi=400)

ggsave(plot = observed_Mar_plot,
       filename = paste0("observed_Mar", ".png"),
       path = "/Users/hn/Desktop/",
       device = "png",
       height = 5, width = 8, units = "in", dpi=400)

####################################################################
#####                                                          #####
#####      observed and modeled months, neck to neck           #####
#####                                                          #####
####################################################################

sept_neck <- ggarrange(Sept_plot, observed_Sept_plot,
                       label.x = "year",
                       label.y = "No. of hours in a given temp. interval",
                       ncol = 1, 
                       nrow = 2, 
                       common.legend = T,
                       legend = "bottom")
ggsave(plot = sept_neck,
       filename = paste0("mo_sept", ".png"),
       path = "/Users/hn/Desktop/",
       device = "png",
       height = 10, width = 8, units = "in", dpi=400)

oct_neck <- ggarrange(Oct_plot, observed_Oct_plot,
                       label.x = "year",
                       label.y = "No. of hours in a given temp. interval",
                       ncol = 1, 
                       nrow = 2, 
                       common.legend = T,
                       legend = "bottom")
ggsave(plot = oct_neck,
       filename = paste0("mo_oct", ".png"),
       path = "/Users/hn/Desktop/",
       device = "png",
       height = 10, width = 8, units = "in", dpi=400)

nov_neck <- ggarrange(Nov_plot, observed_Nov_plot,
                       label.x = "year",
                       label.y = "No. of hours in a given temp. interval",
                       ncol = 1, 
                       nrow = 2, 
                       common.legend = T,
                       legend = "bottom")
ggsave(plot = nov_neck,
       filename = paste0("mo_nov", ".png"),
       path = "/Users/hn/Desktop/",
       device = "png",
       height = 10, width = 8, units = "in", dpi=400)

dec_neck <- ggarrange(Dec_plot, observed_Dec_plot,
                       label.x = "year",
                       label.y = "No. of hours in a given temp. interval",
                       ncol = 1, 
                       nrow = 2, 
                       common.legend = T,
                       legend = "bottom")
ggsave(plot = dec_neck,
       filename = paste0("mo_dec", ".png"),
       path = "/Users/hn/Desktop/",
       device = "png",
       height = 10, width = 8, units = "in", dpi=400)

jan_neck <- ggarrange(Jan_plot, observed_Jan_plot,
                       label.x = "year",
                       label.y = "No. of hours in a given temp. interval",
                       ncol = 1, 
                       nrow = 2, 
                       common.legend = T,
                       legend = "bottom")
ggsave(plot = jan_neck,
       filename = paste0("mo_jan", ".png"),
       path = "/Users/hn/Desktop/",
       device = "png",
       height = 10, width = 8, units = "in", dpi=400)

feb_neck <- ggarrange(Feb_plot, observed_Feb_plot,
                       label.x = "year",
                       label.y = "No. of hours in a given temp. interval",
                       ncol = 1, 
                       nrow = 2, 
                       common.legend = T,
                       legend = "bottom")
ggsave(plot = feb_neck,
       filename = paste0("mo_feb", ".png"),
       path = "/Users/hn/Desktop/",
       device = "png",
       height = 10, width = 8, units = "in", dpi=400)

mar_neck <- ggarrange(Mar_plot, observed_Mar_plot,
                       label.x = "year",
                       label.y = "No. of hours in a given temp. interval",
                       ncol = 1, 
                       nrow = 2, 
                       common.legend = T,
                       legend = "bottom")
ggsave(plot = mar_neck,
       filename = paste0("mo_mar", ".png"),
       path = "/Users/hn/Desktop/",
       device = "png",
       height = 10, width = 8, units = "in", dpi=400)
                      
####################################################################
#####                                                          #####
#####                   Sept. Through Dec.                     #####
#####                                                          #####
####################################################################

input_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/chilling/7_temp_intervals_data/"
# iof = interval of interest
iof = c(c(-Inf, -2), c(-2, 4), 
        c(4, 6), c(6, 8), 
        c(8, 13), c(13, 16), 
        c(16, Inf))
iof_breaks = c(-Inf, -2, 4, 6, 8, 13, 16, Inf)

month_no = c(9, 10, 11, 12)
month_names = c("Sept", "Oct", "Nov", "Dec")
############
############ modeled
############
rm(data)
data <- data.table(readRDS(paste0(input_dir, "sept_thru_dec_modeled.rds")))

data <- data %>% 
          mutate(temp_cat = cut(Temp, breaks = iof_breaks)) %>% 
          group_by(Chill_season, Year, model, Month, scenario, temp_cat, CountyGroup) %>% 
          summarise(no_hours = n())

sep_Dec_plot <- plot_intervals(data, month_name="Sept. through Dec.")
ggsave(plot = sep_Dec_plot,
       filename = "sep_thru_Dec_modeled.png",
       path = "/Users/hn/Desktop/",
       device = "png",
       height = 5, width = 8, units = "in", dpi=400)
rm(data)
############
############ observed
############

data <- data.table(readRDS(paste0(input_dir, "sept_thru_dec_observed.rds")))
data <- data %>% 
          mutate(temp_cat=cut(Temp, breaks=iof_breaks)) %>% 
          group_by(Chill_season, Year, model, Month, scenario, temp_cat, CountyGroup) %>% 
          summarise(no_hours = n())

sep_Dec_plot <- plot_intervals(data, month_name="Sept. through Dec.")
ggsave(plot = sep_Dec_plot,
       filename = "sep_thru_Dec_observed.png",
       path = "/Users/hn/Desktop/",
       device = "png",
       height = 5, width = 8, units = "in", dpi=400)
rm(data)
####################################################################
#####                                                          #####
#####                   Sept. Through Jan.                     #####
#####                                                          #####
####################################################################

input_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/chilling/7_temp_intervals_data/"
# iof = interval of interest
iof = c(c(-Inf, -2), c(-2, 4), 
        c(4, 6), c(6, 8), 
        c(8, 13), c(13, 16), 
        c(16, Inf))
iof_breaks = c(-Inf, -2, 4, 6, 8, 13, 16, Inf)

month_no = c(9, 10, 11, 12, 1)
month_names = c("Sept", "Oct", "Nov", "Dec", "Jan")
############
############ modeled
############
data <- data.table(readRDS(paste0(input_dir, "sept_thru_jan_modeled.rds")))
data <- data %>% 
          mutate(temp_cat=cut(Temp, breaks = iof_breaks)) %>% 
          group_by(Chill_season, Year, model, Month, scenario, temp_cat, CountyGroup) %>% 
          summarise(no_hours = n())

sep_jan_plot <- plot_intervals(data, month_name = "Sept. through Jan.")
ggsave(plot = sep_jan_plot,
       filename = paste0("sep_thru_jan_modeled", ".png"),
       path = "/Users/hn/Desktop/",
       device = "png",
       height = 5, width = 8, units = "in", dpi=400)
rm(data)
############
############ observed
############
data <- data.table(readRDS(paste0(input_dir, "sept_thru_jan_observed.rds")))
data <- data %>% 
        mutate(temp_cat=cut(Temp, breaks=iof_breaks)) %>% 
        group_by(Chill_season, Year, model, Month, scenario, temp_cat, CountyGroup) %>% 
        summarise(no_hours = n())

sep_jan_plot <- plot_intervals(data, month_name="Sept. through Jan.")
ggsave(plot = sep_jan_plot,
       filename = paste0("sep_thru_jan_observed.png"),
       path = "/Users/hn/Desktop/",
       device = "png",
       height = 5, width = 8, units = "in", dpi=400)


