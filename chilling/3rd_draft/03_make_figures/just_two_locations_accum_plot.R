# Script for creating chill accumulation & threshold plots (not maps).
# Intended to work with create-model-plots.sh script.

# 1. Load packages --------------------------------------------------------
rm(list=ls())
library(ggpubr)
library(plyr)
library(tidyverse)
library(ggplot2)
options(digits=9)
options(digit=9)

# 2. Pull data from current directory -------------------------------------

main_in_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/chilling/overlapping/"
# main_in_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/chilling/non_overlapping/"

write_dir_utah = paste0(main_in_dir, "utah_model_stats/")
write_dir_dynamic = paste0(main_in_dir, "dynamic_model_stats/")

model = "dynamic"

if (model=="dynamic"){
  setwd(write_dir_dynamic)
  write_dir = write_dir_dynamic
} else {
  setwd(write_dir_utah)
  write_dir = write_dir_utah
}

##############################################################################
############# 
#############              ********** start from here **********
#############
##############################################################################

summary_comp <- data.table(readRDS(paste0(write_dir, "summary_comp.rds")))
summary_comp$location = paste0(summary_comp$lat, "_", summary_comp$long)
summary_comp <- summary_comp %>% filter(location %in% c("46.28125_-119.34375", "48.40625_-119.53125"))

# 3. Plotting -------------------------------------------------------------
summary_comp_loc_medians <- summary_comp %>%
                            filter(model != "observed") %>%
                            group_by(climate_type, year, model, scenario) %>%
                            summarise_at(.funs = funs(med = median), vars(thresh_20:sum_A1)) %>% 
                            data.table()

##################################
##                              ##
##      Accumulation plots      ##
##                              ##
##################################
write_dir = "/Users/hn/Desktop/"
accum_plot <- function(data, y_name, due){
  y = eval(parse(text =paste0( "data$", y_name)))
  lab = paste0("Median chill units accumulated by ", due)

  acc_plot <- ggplot(data = data) +
              geom_point(aes(x = year, y = y, fill = scenario),
                         alpha = 0.25, shape = 21) +
              geom_smooth(aes(x = year, y = y, color = scenario),
                          method = "lm", size=.8, se = F) +
              facet_wrap( ~ climate_type) +
              scale_color_viridis_d(option = "plasma", begin = 0, end = .7,
                                    name = "Model scenario", 
                                    aesthetics = c("color", "fill")) +              
              ylab("Median accum. chill units") +
              xlab("Year") +
              ggtitle(label = lab,
                      subtitle = "by location, scenario, and model") +
              theme_bw() + 
              theme(plot.margin = unit(c(t=.5, r=.5, b=.5, l=0.5), "cm"),
                    plot.title = element_text(hjust = 0.5),
                    plot.subtitle = element_text(hjust = 0.5),
                    legend.position = "bottom",
                    axis.text.x = element_text(size = 10, face = "plain", color="black"),
                    axis.text.y = element_text(size = 10, face = "plain", color="black"),
                    panel.spacing=unit(.5, "cm"))
  return(acc_plot)
}

accum_hist_plot <- function(data, y_name, due){
  y = eval(parse(text =paste0( "data$", y_name)))
  lab = paste0("Chill units accumulated by ", due, " historically")

  hist_plt <- ggplot(data = data) +
              geom_point(aes(x = year, y = y), alpha = 0.4,
                             shape = 21, fill = "#21908CFF") +
              geom_smooth(aes(x = year, y = y), method = "lm",
                              se = F, size=.5, color = "#21908CFF") +
              facet_wrap( ~ climate_type) +
              ylab("Accum. chill units") +
              xlab("Year") +
              scale_x_continuous(limits = c(1950, 2075)) +
              ggtitle(label = lab,
                      subtitle = "by location") +
              theme_bw() + 
              theme(plot.margin = unit(c(t=.5, r=.5, b=.5, l=0.5), "cm"),
                    plot.title = element_text(hjust = 0.5),
                    plot.subtitle = element_text(hjust = 0.5),
                    axis.text.x = element_text(size = 10, face = "plain", color="black"),
                    axis.text.y = element_text(size = 10, face = "plain", color="black"))
  return(hist_plt)
}
# Data frame for historical values to be used for these figures
summary_comp_hist <- summary_comp %>%
                     filter(model == "observed") %>%
                     group_by(climate_type, year) %>%
                     summarise_at(.funs = funs(med = median), vars(thresh_20:sum_A1))

############################
##
##        Jan plot
##
############################

sum_J1_plot <- accum_plot(data=summary_comp_loc_medians, y_name="sum_J1_med", due="Jan. 1")
sum_J1_hist_plot <- accum_hist_plot(data=summary_comp_hist, y_name="sum_J1_med", due="Jan. 1")

J1_figs <- ggarrange(sum_J1_plot,
                     sum_J1_hist_plot,
                     ncol = 1, nrow = 2,
                     heights = c(1.25, 1))
ggsave(plot = J1_figs, "chill_plot_accum_Jan1.png",
       dpi=400, path=write_dir,
       height = 9, width = 9, units = "in")


################################
##                            ##
##        Feb. plot.      
##                            ##
################################

sum_F1_plot <- accum_plot(data=summary_comp_loc_medians, y_name="sum_F1_med", due="Feb. 1")
sum_F1_hist_plot <- accum_hist_plot(data=summary_comp_hist, y_name="sum_F1_med", due="Feb. 1")
F1_figs <- ggarrange(sum_F1_plot,
                     sum_F1_hist_plot,
                     ncol = 1, nrow = 2,
                     heights = c(1.25, 1))
ggsave(plot = F1_figs, "chill_plot_accum_Feb1.png",
       dpi=400, path=write_dir,
       height = 9, width = 9, units = "in")

############################
##
##       March plot
##
############################

sum_M1_plot <- accum_plot(data=summary_comp_loc_medians, y_name="sum_M1_med", due="Mar. 1")
sum_M1_hist_plot <- accum_hist_plot(data=summary_comp_hist, y_name="sum_M1_med", due="Mar. 1")
M1_figs <- ggarrange(sum_M1_plot,
                     sum_M1_hist_plot,
                     ncol = 1, nrow = 2,
                     heights = c(1.25, 1))
ggsave(plot = M1_figs, "chill_plot_accum_Mar1.png",
       dpi=400, path=write_dir,
       height = 9, width = 9, units = "in")

############################
##
##       April plot
##
############################

sum_A1_plot <- accum_plot(data=summary_comp_loc_medians, y_name="sum_A1_med", due="Apr. 1")
sum_A1_hist_plot <- accum_hist_plot(data=summary_comp_hist, y_name="sum_A1_med", due="Apr. 1")
A1_figs <- ggarrange(sum_A1_plot,
                     sum_A1_hist_plot,
                     ncol = 1, nrow = 2,
                     heights = c(1.25, 1))

ggsave(plot = A1_figs, "chill_plot_accum_Apr1.png",
       dpi=400, path=write_dir,
       height = 9, width = 9, units = "in")
