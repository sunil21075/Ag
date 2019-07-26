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
fileN <- "all_wtr_yr_cum_runoff_LD"
dt_tb <- data.table(readRDS(paste0(in_dir, fileN, ".rds")))
head(dt_tb, 2)

plot_col <- "annual_cum_runbase"
y_lab <- "annual (water year) cum. [runff + BF] (mm)"

wtr_yr_cum_prec <- ann_wtrYr_chunk_cum_box_cluster_x(dt_tb, 
                                                      y_lab=y_lab, 
                                                      tgt_col=plot_col)

ggsave(filename = paste0(fileN, ".png"), 
       plot = wtr_yr_cum_prec, 
       width = 8, height = 3, units = "in", 
       dpi=600, device = "png",
       path = plot_dir)





