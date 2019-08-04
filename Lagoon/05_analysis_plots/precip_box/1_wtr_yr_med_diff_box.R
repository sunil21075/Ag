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
plot_dir <- paste0(in_dir, "plots/wtr_yr/")
if (dir.exists(plot_dir) == F) {dir.create(path = plot_dir, recursive = T)}

##############################

fileN <- "wtr_yr_sept_all_last_days"
tgt_col <- "annual_cum_precip"
dt_tb <- data.table(readRDS(paste0(in_dir, fileN, ".rds")))
head(dt_tb, 2)

meds <- compute_median_diff_4_map(dt_tb, tgt_col=tgt_col)

dt_tb <- subset(dt_tb, select=c(location, cluster))
dt_tb <- unique(dt_tb)
meds <- merge(meds, dt_tb, all.x=T, by="location")

box_title <- "diff. of medians from median of obs."
box_subtitle <- "for each model median is taken over years, separately"

b <- ann_wtrYr_chunk_cum_box_cluster_x(dt=meds,
                                       y_lab="magnitude of differences",
                                       tgt_col="diff",
                                       ttl=box_title, 
                                       subttl=box_subtitle)

ggsave(filename = "mag_med_diff_med_wtr_yr.png", 
       plot = b, 
       width = 9.5, height = 5, units = "in", 
       dpi=300, device = "png",
       path = plot_dir)

b <- ann_wtrYr_chunk_cum_box_cluster_x(dt=meds,
                                       y_lab="differences (%)",
                                       tgt_col="perc_diff",
                                       ttl=box_title, 
                                       subttl=box_subtitle)

ggsave(filename = "perc_med_diff_med_wtr_yr.png", 
       plot = b, 
       width = 9.5, height = 5, units = "in", 
       dpi=300, device = "png",
       path = plot_dir)


