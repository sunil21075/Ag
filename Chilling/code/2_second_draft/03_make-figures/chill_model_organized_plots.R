# Script for creating chill accumulation & threshold plots (not maps).
# Intended to work with create-model-plots.sh script.

# 1. Load packages --------------------------------------------------------
rm(list=ls())
library(ggpubr)
library(plyr)
library(tidyverse)

# 2. Pull data from current directory -------------------------------------

main_in_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/chilling/overlapping/"
main_in_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/chilling/non_overlapping/"

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

# the_dir <- dir()

# # remove filenames that aren't data
# the_dir <- the_dir[grep(pattern = "summary", x = the_dir)]

# # drop the ones with stats in their name
# the_dir_summary <- the_dir[-grep(pattern = "summary_stats", x = the_dir)]

# # Compile the data files for plotting
# summary_comp <- lapply(the_dir_summary, read.table, header = T)
# summary_comp <- do.call(bind_rows, summary_comp)

# # Remove incomplete model runs
# # **** I removed the incomplete data in the first place
# # so, we do not need this
# # summary_comp <- summary_comp[-grep(x = summary_comp$model, pattern = "incomplete"),]

# # Combine the data with cold/warm geographic designations
# param_dir = "/home/hnoorazar/chilling_codes/parameters/"
# param_dir = "/Users/hn/Documents/GitHub/Kirti/codling_moth/code/parameters/"
# cold_warm <- read.csv(paste0(param_dir, "LocationGroups.csv"))

# summary_comp <- inner_join(x = summary_comp, y = cold_warm,
#                            by = c("long" = "longitude",
#                                   "lat" = "latitude")) %>%
#                 mutate(climate_type = case_when( # create var for cool/warm designation
#                                                 locationGroup == 1 ~ "Cooler Area",
#                                                 locationGroup == 2 ~ "Warmer Area")) %>%
#                 select(-locationGroup, -.id)

# saveRDS(summary_comp, paste0(write_dir, "summary_comp.rds"))

##############################################################################
############# 
#############              ********** start from here **********
#############
##############################################################################

summary_comp <- data.table(readRDS(paste0(write_dir, "summary_comp.rds")))

# 3. Plotting -------------------------------------------------------------
summary_comp_loc_medians <- summary_comp %>%
                            filter(model != "observed") %>%
                            group_by(climate_type, year, model, scenario) %>%
                            summarise_at(.funs = funs(med = median), vars(thresh_20:sum_A1)) %>% 
                            data.table()

# Two plots, collapsing all warm/cool locations but keeping all models separate
thresh_new_plot <- function(data, percentile){
  y = eval(parse(text =paste0( "data$", "thresh_", percentile, "_med")))
  lab = paste0("Median days to reach ", percentile , " accumulated chill units")
  
  the_plot <- ggplot(data = data) +
              geom_point(aes(x = year, y = y, fill = scenario),
                             alpha = 0.25, shape = 21, size = .25) +
              geom_smooth(aes(x = year, y = y, color = scenario),
                          method = "lm", size=0.5, se = F) +
              facet_wrap( ~ climate_type) +
              scale_color_viridis_d(option = "plasma", begin = 0, end = .7,
                                    name = "Model scenario", 
                                    aesthetics = c("color", "fill")) +
              geom_hline(yintercept=c(122, 137), linetype="solid", color ="red", size=0.2) +
              scale_y_continuous(breaks=c(0, 50, 75, 100, 122, 137, 150, 200, 250, 300)) + 
              ylab("Median days") +
              xlab("Year") +
              ggtitle(label = lab) +
              theme_bw() + 
              theme(plot.title = element_text(hjust = 0.5),
                    plot.subtitle = element_text(hjust = 0.5),
                    axis.text.y = element_text(size = 12, face = "plain", color="black"),
                    axis.title.x = element_text(face = "plain", size=15, 
                                                margin = margin(t=8, r=0, b=0, l=0)),
                    axis.text.x = element_text(size = 12, face = "plain", color="black"),
                    axis.ticks.x = element_blank(),
                    axis.title.y = element_text(face = "plain", size = 15, 
                                                margin = margin(t=0, r=8, b=0, l=0)),
                    panel.spacing=unit(.5, "cm")
                    )
  return (the_plot)
}

thresh_20_all_plot <- thresh_new_plot(data = summary_comp_loc_medians, percentile="20")
thresh_25_all_plot <- thresh_new_plot(data = summary_comp_loc_medians, percentile="25")
thresh_30_all_plot <- thresh_new_plot(data = summary_comp_loc_medians, percentile="30")
thresh_35_all_plot <- thresh_new_plot(data = summary_comp_loc_medians, percentile="35")
thresh_40_all_plot <- thresh_new_plot(data = summary_comp_loc_medians, percentile="40")
thresh_45_all_plot <- thresh_new_plot(data = summary_comp_loc_medians, percentile="45")
thresh_50_all_plot <- thresh_new_plot(data = summary_comp_loc_medians, percentile="50")
thresh_55_all_plot <- thresh_new_plot(data = summary_comp_loc_medians, percentile="55")
thresh_60_all_plot <- thresh_new_plot(data = summary_comp_loc_medians, percentile="60")
thresh_65_all_plot <- thresh_new_plot(data = summary_comp_loc_medians, percentile="65")
thresh_70_all_plot <- thresh_new_plot(data = summary_comp_loc_medians, percentile="70")
thresh_75_all_plot <- thresh_new_plot(data = summary_comp_loc_medians, percentile="75")

thresh_hist_plot <- summary_comp %>%
                    filter(model == "observed") %>%
                    group_by(climate_type, year) %>%
                    summarise_at(.funs = funs(med = median), vars(thresh_20:sum_A1)) %>%
                    ggplot() +
                    geom_point(aes(x = year, y = thresh_75_med, fill = "75 units"), 
                                   alpha = 0.4,
                                   shape = 21, size = 1) +
                    geom_smooth(aes(x = year, y = thresh_75_med, col = "75 units"), 
                                method = "lm", size=0.5,
                                se = F) +
                    geom_point(aes(x = year, y = thresh_70_med, fill = "70 units"), 
                                   alpha = 0.4,
                                   shape = 21, size = 1) +
                    geom_smooth(aes(x = year, y = thresh_70_med, col = "70 units"), 
                                method = "lm", size=0.5,
                                se = F) +

                    geom_point(aes(x = year, y = thresh_65_med, fill = "65 units"), 
                               alpha = 0.4,
                               shape = 21, size = 1) +
                    geom_smooth(aes(x = year, y = thresh_65_med, col = "65 units"), 
                                method = "lm", size=0.5,
                                se = F) +
                    geom_point(aes(x = year, y = thresh_60_med, fill = "60 units"), 
                               alpha = 0.4,
                               shape = 21, size = 1) +
                    geom_smooth(aes(x = year, y = thresh_60_med, col = "60 units"), 
                                method = "lm", size=0.5,
                                se = F) +

                    geom_point(aes(x = year, y = thresh_55_med, fill = "55 units"), 
                               alpha = 0.4,
                               shape = 21, size = 1) +
                    geom_smooth(aes(x = year, y = thresh_55_med, col = "55 units"), 
                                method = "lm", size=0.5,
                                se = F) +
                    geom_point(aes(x = year, y = thresh_50_med, fill = "50 units"), 
                               alpha = 0.4,
                               shape = 21, size = 1) +
                    geom_smooth(aes(x = year, y = thresh_50_med, col = "50 units"), 
                                method = "lm", size=0.5,
                                se = F) +

                    geom_point(aes(x = year, y = thresh_45_med, fill = "45 units"), 
                               alpha = 0.4,
                               shape = 21, size = 1) +
                    geom_smooth(aes(x = year, y = thresh_45_med, col = "45 units"), 
                                method = "lm", size=0.5,
                                se = F) +
                    geom_point(aes(x = year, y = thresh_40_med, fill = "40 units"), 
                               alpha = 0.4,
                               shape = 21, size = 1) +
                    geom_smooth(aes(x = year, y = thresh_40_med, col = "40 units"), 
                                method = "lm",size=0.5,
                                se = F) +
                    geom_point(aes(x = year, y = thresh_35_med, fill = "35 units"), 
                               alpha = 0.4,
                               shape = 21, size = 1) +
                    geom_smooth(aes(x = year, y = thresh_35_med, col = "35 units"), 
                                method = "lm", size=0.5,
                                se = F) +
                    geom_point(aes(x = year, y = thresh_30_med, fill = "30 units"), 
                               alpha = 0.4,
                               shape = 21, size = 1) +
                    geom_smooth(aes(x = year, y = thresh_30_med, col = "30 units"), 
                                method = "lm", size=0.5,
                                se = F) +
                    geom_point(aes(x = year, y = thresh_25_med, fill = "25 units"), 
                               alpha = 0.4,
                               shape = 21, size = 1) +
                    geom_smooth(aes(x = year, y = thresh_25_med, col = "25 units"), 
                                method = "lm", size=0.5,
                                se = F) +
                    geom_point(aes(x = year, y = thresh_20_med, fill = "20 units"), 
                               alpha = 0.4,
                               shape = 21, size = 1) +
                    geom_smooth(aes(x = year, y = thresh_20_med, col = "20 units"), 
                                method = "lm", size=0.5,
                                se = F) +
                    geom_hline(yintercept=c(122, 137), linetype="solid", color ="red", size=0.2) +
                    scale_color_manual(name = "Threshold", values = seq(1:12)) +
                    scale_fill_manual(name = "Threshold", values = seq(1:12)) +
                    scale_y_continuous(breaks=c(0, 50, 100, 75, 122, 137, 150, 200, 250, 300)) + 
                    facet_wrap( ~ climate_type) +
                    ylab("days") +
                    xlab("year") +
                    scale_x_continuous(limits = c(1950, 2075)) +
                    ggtitle(label = "Days to reach 20 through 75 accumulated chill units historically") +
                    theme_bw() + 
                    theme(plot.title = element_text(hjust = 0.5),
                          plot.subtitle = element_text(hjust = 0.5),
                          legend.position = "bottom",
                          axis.text.y = element_text(size = 12, face = "plain", color="black"),
                          axis.title.x = element_text(face = "plain", size=15, 
                                                      margin = margin(t=8, r=0, b=0, l=0)),
                          axis.text.x = element_text(size = 12, face = "plain", color="black"),
                          axis.ticks.x = element_blank(),
                          axis.title.y = element_text(face = "plain", size = 15, 
                                                      margin = margin(t=0, r=8, b=0, l=0)),
                          panel.spacing=unit(.5, "cm")
                          )

thresh_hist_plot <- annotate_figure(p = thresh_hist_plot,
                                    top = text_grob(label = "Observed historical accumulation by location",
                                                    face = "bold", size = 16))

# Combine the plots and export
thresh_future <- ggarrange(thresh_20_all_plot, thresh_25_all_plot,
                           thresh_30_all_plot, thresh_35_all_plot,
                           thresh_40_all_plot, thresh_45_all_plot,
                           thresh_50_all_plot, thresh_55_all_plot,
                           thresh_60_all_plot, thresh_65_all_plot,
                           thresh_70_all_plot, thresh_75_all_plot,
                           ncol = 1, nrow = 12, common.legend = T,
                           legend = "bottom")

thresh_future<- annotate_figure(p = thresh_future,
                                top = text_grob(label = "Modeled accumulation by location, scenario, and model",
                                                face = "bold", size = 16))

thresh_figs <- ggarrange(thresh_future,
                         thresh_hist_plot,
                         ncol = 1, nrow = 2,
                         heights = c(12, 1.1))

ggsave(plot = thresh_figs, filename ="chill_plot_thresholds.png",
       dpi=400, path=write_dir,
       height = 70, width = 18, units = "in", limitsize = FALSE)

##################################
##                              ##
##      Accumulation plots      ##
##                              ##
##################################

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
##
##        Feb. plot
##
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
