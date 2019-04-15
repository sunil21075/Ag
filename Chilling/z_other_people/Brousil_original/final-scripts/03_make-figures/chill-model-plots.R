# Script for creating chill accumulation & threshold plots (not maps).
# Intended to work with create-model-plots.sh script.

# 1. Load packages --------------------------------------------------------

library(ggpubr)
library(plyr)
library(tidyverse)

# 2. Pull data from current directory -------------------------------------
the_dir <- dir()

# remove filenames that aren't data
the_dir <- the_dir[grep(pattern = "chill-data-summary",
                        x = the_dir)]

the_dir_summary <- the_dir[-grep(pattern = "chill-data-summary-stats",
                                 x = the_dir)]

# Compile the data files for plotting
summary_comp <- lapply(the_dir_summary, read.table, header = T)
summary_comp <- do.call(bind_rows, summary_comp)

# Remove incomplete model runs
summary_comp <- summary_comp[-grep(x = summary_comp$model, pattern = "incomplete"),]

# Combine the data with cold/warm geographic designations
cold_warm <- read.csv("/home/mbrousil/files/LocationGroups.csv")

summary_comp <- inner_join(x = summary_comp, y = cold_warm,
                           by = c("long" = "longitude",
                                  "lat" = "latitude")) %>%
                mutate(climate_type = case_when( # create var for cool/warm designation
                                                locationGroup == 1 ~ "Cooler",
                                                locationGroup == 2 ~ "Warmer")) %>%
                select(-locationGroup, -.id)

# 3. Plotting -------------------------------------------------------------

# Threshold plots

# Two plots, each plotting only median model points
# For now not exporting this plot but leaving the code here

# Take a median within each year, collapsing 295 locations to just 2 groups
# (cool/warm) and collapsing all models (removing observed hist)
# but retaining scenarios.

summary_comp_medians <- summary_comp %>%
  filter(model != "observed", scenario != "historical") %>%
  group_by(climate_type, year, scenario) %>%
  summarise_at(.funs = funs(med = median), vars(thresh_50:sum_A1))

thresh_50_plot <- ggplot(data = summary_comp_medians) +
  geom_point(aes(x = year, y = thresh_50_med, color = scenario)) +
  geom_smooth(aes(x = year, y = thresh_50_med, color = scenario),
              method = "lm", se = F) +
  facet_wrap( ~ climate_type) +
  scale_color_viridis_d(option = "plasma", begin = .4, end = .7,
                        name = "Scenario") +
  ylab("Median days") +
  xlab("Year") +
  ggtitle(label = "Median days to reach 50 accumulated chill units",
          subtitle = "by cool/warm location and climate scenario") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

thresh_75_plot <- ggplot(data = summary_comp_medians) +
  geom_point(aes(x = year, y = thresh_75_med, color = scenario)) +
  geom_smooth(aes(x = year, y = thresh_75_med, color = scenario),
              method = "lm", se = F) +
  facet_wrap( ~ climate_type) +
  scale_color_viridis_d(option = "plasma", begin = .4, end = .7) +
  ylab("Median days") +
  xlab("Year") +
  ggtitle(label = "Median days to reach 75 accumulated chill units",
          subtitle = "by cool/warm location and climate scenario") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

thresh_figs_medians <- ggarrange(thresh_50_plot,
                                 thresh_75_plot,
                                 ncol = 1, nrow = 2)

#ggsave(plot = thresh_figs_medians, "chill-plot_thresholds.png",
#       height = 15, width = 8, units = "in")

# Two plots, collapsing all warm/cool locations but keeping all models separate
summary_comp_loc_medians <- summary_comp %>%
  filter(model != "observed") %>%
  group_by(climate_type, year, model, scenario) %>%
  summarise_at(.funs = funs(med = median), vars(thresh_50:sum_A1))

thresh_50_all_plot <- ggplot(data = summary_comp_loc_medians) +
  geom_point(aes(x = year, y = thresh_50_med, fill = scenario),
             alpha = 0.25, shape = 21, size = 1) +
  geom_smooth(aes(x = year, y = thresh_50_med, color = scenario),
              method = "lm", se = F) +
  facet_wrap( ~ climate_type) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = .7,
                        name = "Model scenario", aesthetics = c("color", "fill")) +
  ylab("Median days") +
  xlab("Year") +
  ggtitle(label = "Median days to reach 50 accumulated chill units") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

thresh_75_all_plot <- ggplot(data = summary_comp_loc_medians) +
  geom_point(aes(x = year, y = thresh_75_med, fill = scenario),
             alpha = 0.25, shape = 21, size = 1) +
  geom_smooth(aes(x = year, y = thresh_75_med, color = scenario),
              method = "lm", se = F) +
  facet_wrap( ~ climate_type) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = .7,
                        name = "Model scenario", aesthetics = c("color", "fill")) +
  ylab("Median days") +
  xlab("Year") +
  ggtitle(label = "Median days to reach 75 accumulated chill units") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

thresh_hist_plot <- summary_comp %>%
  filter(model == "observed") %>%
  group_by(climate_type, year) %>%
  summarise_at(.funs = funs(med = median), vars(thresh_50:sum_A1)) %>%
  ggplot() +
  geom_point(aes(x = year, y = thresh_75_med, fill = "75 units"), alpha = 0.4,
             shape = 21, size = 1) +
  geom_smooth(aes(x = year, y = thresh_75_med, col = "75 units"), method = "lm",
              se = F) +
  geom_point(aes(x = year, y = thresh_50_med, fill = "50 units"), alpha = 0.4,
             shape = 21, size = 1) +
  geom_smooth(aes(x = year, y = thresh_50_med, col = "50 units"), method = "lm",
              se = F) +
  scale_color_manual(name = "Threshold", values = c("#BBDF27FF", "#21908CFF")) +
  scale_fill_manual(name = "Threshold", values = c("#BBDF27FF", "#21908CFF")) +
  facet_wrap( ~ climate_type) +
  ylab("Days") +
  xlab("Year") +
  scale_x_continuous(limits = c(1950, 2075)) +
  ggtitle(label = "Days to reach 50 and 75 accumulated chill units historically") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")

thresh_hist_plot <- annotate_figure(p = thresh_hist_plot,
                                    top = text_grob(label = "Observed historical accumulation by location",
                                                    face = "bold", size = 16))


# Combine the plots and export
thresh_future <- ggarrange(thresh_50_all_plot, thresh_75_all_plot,
                           ncol = 1, nrow = 2, common.legend = T,
                           legend = "bottom")

thresh_future<- annotate_figure(p = thresh_future,
                                top = text_grob(label = "Modeled accumulation by location, scenario, and model",
                                                face = "bold", size = 16))
thresh_figs <- ggarrange(thresh_future,
                         thresh_hist_plot,
                         ncol = 1, nrow = 2,
                         heights = c(2, 1.1))

ggsave(plot = thresh_figs, "chill-plot_thresholds.png",
       height = 15, width = 8, units = "in")




# Accumulation plots

# Data frame for historical values to be used for these figures
summary_comp_hist <- summary_comp %>%
                     filter(model == "observed") %>%
                     group_by(climate_type, year) %>%
                     summarise_at(.funs = funs(med = median), vars(thresh_50:sum_A1))

# Jan plot
sum_J1_plot <- ggplot(data = summary_comp_loc_medians) +
  geom_point(aes(x = year, y = sum_J1_med, fill = scenario),
             alpha = 0.25, shape = 21) +
  geom_smooth(aes(x = year, y = sum_J1_med, color = scenario),
              method = "lm", se = F) +
  facet_wrap( ~ climate_type) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = .7,
                        name = "Model scenario", aesthetics = c("color", "fill")) +
  ylab("Median accum. chill units") +
  xlab("Year") +
  ggtitle(label = "Median chill units accumulated by Jan 1",
          subtitle = "by location, scenario, and model") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")

sum_J1_hist_plot <- ggplot(data = summary_comp_hist) +
  geom_point(aes(x = year, y = sum_J1_med), alpha = 0.4,
             shape = 21, fill = "#21908CFF") +
  geom_smooth(aes(x = year, y = sum_J1_med), method = "lm",
              se = F, color = "#21908CFF") +
  facet_wrap( ~ climate_type) +
  ylab("Accum. chill units") +
  xlab("Year") +
  scale_x_continuous(limits = c(1950, 2075)) +
  ggtitle(label = "Chill units accumulated by Jan 1 historically",
          subtitle = "by location") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

J1_figs <- ggarrange(sum_J1_plot,
                     sum_J1_hist_plot,
          ncol = 1, nrow = 2,
          heights = c(1.25, 1))

ggsave(plot = J1_figs, "chill-plot_accum-Jan1.png",
       height = 9, width = 9, units = "in")


# Feb plot
sum_F1_plot <- ggplot(data = summary_comp_loc_medians) +
  geom_point(aes(x = year, y = sum_F1_med, fill = scenario),
             alpha = 0.25, shape = 21, size = 1) +
  geom_smooth(aes(x = year, y = sum_F1_med, color = scenario),
              method = "lm", se = F) +
  facet_wrap( ~ climate_type) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = .7,
                        name = "Model scenario", aesthetics = c("color", "fill")) +
  ylab("Median accum. chill units") +
  xlab("Year") +
  ggtitle(label = "Median chill units accumulated by Feb 1",
          subtitle = "by location, scenario, and model") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")

sum_F1_hist_plot <- ggplot(data = summary_comp_hist) +
  geom_point(aes(x = year, y = sum_F1_med), alpha = 0.4,
             shape = 21, fill = "#21908CFF") +
  geom_smooth(aes(x = year, y = sum_F1_med), method = "lm",
              se = F, color = "#21908CFF") +
  facet_wrap( ~ climate_type) +
  ylab("Accum. chill units") +
  xlab("Year") +
  scale_x_continuous(limits = c(1950, 2075)) +
  ggtitle(label = "Chill units accumulated by Feb 1 historically",
          subtitle = "by location") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

F1_figs <- ggarrange(sum_F1_plot,
                     sum_F1_hist_plot,
                     ncol = 1, nrow = 2,
                     heights = c(1.25, 1))

ggsave(plot = F1_figs, "chill-plot_accum-Feb1.png",
       height = 9, width = 9, units = "in")


# March plot
sum_M1_plot <- ggplot(data = summary_comp_loc_medians) +
  geom_point(aes(x = year, y = sum_M1_med, fill = scenario),
             alpha = 0.25, shape = 21, size = 1) +
  geom_smooth(aes(x = year, y = sum_M1_med, color = scenario),
              method = "lm", se = F) +
  facet_wrap( ~ climate_type) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = .7,
                        name = "Model scenario", aesthetics = c("color", "fill")) +
  ylab("Median accum. chill units") +
  xlab("Year") +
  ggtitle(label = "Median chill units accumulated by Mar 1",
          subtitle = "by location, scenario, and model") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")

sum_M1_hist_plot <- ggplot(data = summary_comp_hist) +
  geom_point(aes(x = year, y = sum_M1_med), alpha = 0.4,
             shape = 21, fill = "#21908CFF") +
  geom_smooth(aes(x = year, y = sum_M1_med), method = "lm",
              se = F, color = "#21908CFF") +
  facet_wrap( ~ climate_type) +
  ylab("Accum. chill units") +
  xlab("Year") +
  scale_x_continuous(limits = c(1950, 2075)) +
  ggtitle(label = "Chill units accumulated by Mar 1 historically",
          subtitle = "by location") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

M1_figs <- ggarrange(sum_M1_plot,
                     sum_M1_hist_plot,
                     ncol = 1, nrow = 2,
                     heights = c(1.25, 1))

ggsave(plot = M1_figs, "chill-plot_accum-Mar1.png",
       height = 9, width = 9, units = "in")


# April plot
sum_A1_plot <- ggplot(data = summary_comp_loc_medians) +
  geom_point(aes(x = year, y = sum_A1_med, fill = scenario),
             alpha = 0.25, shape = 21, size = 1) +
  geom_smooth(aes(x = year, y = sum_A1_med, color = scenario),
              method = "lm", se = F) +
  facet_wrap( ~ climate_type) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = .7,
                        name = "Model scenario", aesthetics = c("color", "fill")) +
  ylab("Median accum. chill units") +
  xlab("Year") +
  ggtitle(label = "Median chill units accumulated by Apr 1",
          subtitle = "by location, scenario, and model") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")

sum_A1_hist_plot <- ggplot(data = summary_comp_hist) +
  geom_point(aes(x = year, y = sum_A1_med), alpha = 0.4,
             shape = 21, fill = "#21908CFF") +
  geom_smooth(aes(x = year, y = sum_A1_med), method = "lm",
              se = F, color = "#21908CFF") +
  facet_wrap( ~ climate_type) +
  ylab("Accum. chill units") +
  xlab("Year") +
  scale_x_continuous(limits = c(1950, 2075)) +
  ggtitle(label = "Chill units accumulated by Apr 1 historically",
          subtitle = "by location") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

A1_figs <- ggarrange(sum_A1_plot,
                     sum_A1_hist_plot,
                     ncol = 1, nrow = 2,
                     heights = c(1.25, 1))

ggsave(plot = A1_figs, "chill-plot_accum-Apr1.png",
       height = 9, width = 9, units = "in")
