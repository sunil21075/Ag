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

in_dir <- "/Users/hn/Desktop/Desktop/Kirti/check_point/lagoon/runoff/"
plot_dir <- paste0(in_dir, "plots/")

##############################
fileN <- "all_monthly_cum_runoff_LD"
dt_tb <- data.table(readRDS(paste0(in_dir, fileN, ".rds")))
head(dt_tb, 2)

plotting_col <- "monthly_cum_runbase"
y_lab <- "monthly cum. [runff + BF] (mm)"

box_plt <- box_trend_monthly_cum(dt=dt_tb, p_type="box", y_lab)

ggsave(filename = "monthly_box.png", 
       plot = box_plt, 
       width = 14, height = 6, units = "in", 
       dpi=600, device = "png",
       path = paste0(plot_dir, "clust_on_x/"))

