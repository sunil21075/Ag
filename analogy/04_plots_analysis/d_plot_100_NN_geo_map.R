rm(list=ls())
library(data.table)
library(dplyr)
library(ggmap)
library(ggpubr)

options(digits=9)
options(digit=9)


in_dir <- "/Users/hn/Desktop/Desktop/Kirti/check_point/analogs/avg_avg_rcp85_NO_precip/rcp85/"

NNs_name <-  "NN_loc_year_tb_avg_26_50.rds"
dist_name <- "NN_dist_tb_avg_26_50.rds"
sigma_name <-"NN_sigma_tb_avg_26_50.rds"

NNs_name <- paste0(in_dir, NNs_name)
dist_name <- paste0(in_dir, dist_name)
sigma_name <- paste0(in_dir, sigma_name)

NNs <- data.table(readRDS(NNs_name))
dists <- data.table(readRDS(dist_name))
sigmas <- data.table(readRDS(sigma_name))

###########################################################
# 46.28125_-119.34375 Richland
# 48.40625_-119.53125 Omak
# 47.40625_-120.34375 Wenatchee     IS not in all USA data
# 45.53125_-123.15625 Hilsboro
# 44.09375_-123.34375 Elmira
###########################################################

given_locations <- c("46.28125_-119.34375", "48.40625_-119.53125", "47.40625_-120.34375")

NNs_int <- NNs %>% filter(location %in% given_locations)
dist_int <- dists %>% filter(location %in% given_locations)
sigma_int <- sigmas %>% filter(location %in% given_locations)


plot_100_NN_geo_map





