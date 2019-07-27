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

fileN <- "Sept_March_all_last_days"
dt_tb <- data.table(readRDS(paste0(in_dir, fileN, ".rds")))
head(dt_tb, 2)

plot_col <- "chunk_cum_precip"
y_lab <- "Sept. - Mar. cum. precip. (mm)"

chunk_box <- ann_wtrYr_chunk_cum_box_cluster_x(dt=dt_tb,
                                                y_lab = y_lab,
                                                tgt_col=plot_col)

ggsave(filename = paste0(fileN, ".png"), 
       plot = chunk_box, 
       width = 10, height = 3, units = "in", 
       dpi=600, device = "png",
       path = paste0(plot_dir, "clust_on_x/"))

