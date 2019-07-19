rm(list=ls())
library(lubridate)
library(ggpubr)
library(purrr)
library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)


source_path_1 = "/Users/hn/Documents/GitHub/Kirti/Lagoon/core_lagoon.R"
source_path_2 = "/Users/hn/Documents/GitHub/Kirti/Lagoon/core_plot_lagoon.R"
source(source_path_1)
source(source_path_2)

options(digit=9)
options(digits=9)

in_dir <- "/Users/hn/Desktop/Desktop/Kirti/check_point/lagoon/cum_precip/"
plot_dir <- "/Users/hn/Desktop/Desktop/Kirti/check_point/lagoon/plots/"

##############################

files <- c("ann_all_last_days", "month_all_last_days",
           "Sept_March_all_last_days", "wtr_yr_sept_all_last_days")

plotting_cols <- c("annual_cum_precip", "monthly_cum_precip",
                   "chunk_cum_precip", "annual_cum_precip")

file <- files[1]
plot_col <- plotting_cols[1]

for (ii in 1:4){
  print("first line of loop")
  file <- files[ii]
  plot_col <- plotting_cols[ii]

  dt_tb <- data.table(readRDS(paste0(in_dir, file, ".rds")))
  head(dt_tb, 2)

  # exclude modeled historical
  # dt <- dt %>% filter(time_period != "1950-2005")

  box_plt <- cum_clust_box_plots(dt=dt_tb, tgt_col=plot_col)
  box_plt_clst_x <- cum_box_cluster_x(dt=dt_tb, tgt_col=plot_col)
  ggsave(filename = paste0(file, ".png"), 
         plot = box_plt, 
         width = 8, height = 3, units = "in", 
         dpi=600, device = "png",
         path = paste0(plot_dir, "time_on_x/"))

  ggsave(filename = paste0(file, ".png"), 
         plot = box_plt_clst_x, 
         width = 8, height = 3, units = "in", 
         dpi=600, device = "png",
         path = paste0(plot_dir, "clust_on_x/"))

  print(ii)
  print("Last line of loop")
}


