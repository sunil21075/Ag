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

base <- "/Users/hn/Desktop/Desktop/Kirti/check_point/lagoon/cum_precip/"
in_dir <- paste0(base, "/02_med_diff_med_no_bias/")
plot_dir <- paste0(base, "plots/chunky/")
if (dir.exists(plot_dir) == F) {dir.create(path = plot_dir, recursive = T)}
##############################
param <- "/Users/hn/Documents/GitHub/Kirti/Lagoon/parameters/loc_fip_clust.csv"
clusters <- read.csv(param, header=T, as.is=T)
clusters <- within(clusters, remove(ann_prec_mean, centroid, fips))
##############################

fileN <- "detail_med_diff_med_chunk_precip"
dt_tb <- data.table(readRDS(paste0(in_dir, fileN, ".rds")))
dt_tb <- merge(dt_tb, clusters, all.x=T, by="location")
head(dt_tb, 2)

box_title <- "diff. of medians (w/ no bias)"
box_subtitle <- "for each model median is taken over years, separately"
b <- ann_wtrYr_chunk_cum_box_cluster_x(dt=dt_tb,
                                       y_lab="magnitude of differences",
                                       tgt_col="diff",
                                       ttl = box_title, 
                                       subttl= box_subtitle)

ggsave(filename = "no_bias_mag_med_diff_med_chunk.png", 
       plot = b, 
       width = 9.5, height = 5, units = "in", 
       dpi=300, device = "png",
       path = plot_dir)

b <- ann_wtrYr_chunk_cum_box_cluster_x(dt=dt_tb,
                                       y_lab="differences (%)",
                                       tgt_col="perc_diff",
                                       ttl = box_title, 
                                       subttl= box_subtitle)

ggsave(filename = "no_bias_perc_med_diff_med_chunk.png", 
       plot = b, 
       width = 9.5, height = 5, units = "in", 
       dpi=300, device = "png",
       path = plot_dir)




