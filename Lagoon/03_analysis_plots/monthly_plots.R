rm(list=ls())
library(lubridate)
library(ggpubr)
library(purrr)
library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)
options(digit=9)
options(digits=9)

source_path_1 = "/Users/hn/Documents/GitHub/Kirti/Lagoon/core_lagoon.R"
source_path_2 = "/Users/hn/Documents/GitHub/Kirti/Lagoon/core_plot_lagoon.R"
source(source_path_1)
source(source_path_2)

in_dir <- "/Users/hn/Desktop/Desktop/Kirti/check_point/lagoon/cum_precip/"
plot_dir <- paste0(in_dir, "plots/")

##############################

file <- "month_all_last_days"
plotting_col <- "monthly_cum_precip"
dt_tb <- data.table(readRDS(paste0(in_dir, file, ".rds")))
head(dt_tb, 2)

box_plt <- box_trend_monthly(dt=dt_tb, p_type="box")
trend_med <- box_trend_monthly(dt=dt_tb, p_type="trend", trend_type="median")
# trend_mean <- box_trend_monthly(dt=dt_tb, p_type="trend", trend_type="mean")

ggsave(filename = paste0(file, ".png"), 
       plot = box_plt, 
       width = 8, height = 3, units = "in", 
       dpi=600, device = "png",
       path = paste0(plot_dir, "time_on_x/"))

ggsave(filename = paste0(file, "_trend_med.png"), 
       plot = trend_med, 
       width = 40, height = 20, units = "in", 
       dpi=600, device = "png",
       path = paste0(plot_dir, "time_on_x/")) 


