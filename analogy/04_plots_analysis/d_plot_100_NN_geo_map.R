rm(list=ls())
library(data.table)
library(dplyr)
library(ggmap)
library(ggpubr)

options(digits=9)
options(digit=9)

######################################################################
#
#    Set up directories
#
######################################################################

in_dir <- "/Users/hn/Desktop/Desktop/Kirti/check_point/analogs/avg_avg_rcp85_NO_precip/rcp85/"
param_dir <- "/Users/hn/Documents/GitHub/Kirti/analogy/parameters/"

######################################################################
#
#    Set up file names
#
######################################################################
NNs_name <-  "NN_loc_year_tb_avg_26_50.rds"
dist_name <- "NN_dist_tb_avg_26_50.rds"
sigma_name <-"NN_sigma_tb_avg_26_50.rds"

NNs_name <- paste0(in_dir, NNs_name)
dist_name <- paste0(in_dir, dist_name)
sigma_name <- paste0(in_dir, sigma_name)

######################################################################
#
#    read files
#
######################################################################

NNs_orig <- data.table(readRDS(NNs_name))
dists_orig <- data.table(readRDS(dist_name))
sigmas_orig <- data.table(readRDS(sigma_name))
county_list <- data.table(read.table(paste0(param_dir, "us_fips_st_county_lat_long.csv"), header=T, sep=","))

a <- count_NNs_per_counties_all_locs(NNs=NNs_orig, dists=dists_orig, sigmas=sigmas_orig, 
                                     county_list=county_list, 
                                     sigma_bd=2, novel_thresh=4)
###########################################################
# 46.28125_-119.34375 Richland
# 48.40625_-119.53125 Omak
# 47.40625_-120.34375 Wenatchee     IS not in all USA data (from codling moth results)
# 45.53125_-123.15625 Hilsboro
# 44.09375_-123.34375 Elmira
###########################################################

given_locations <- c("46.28125_-119.34375") # , "48.40625_-119.53125", "47.40625_-120.34375"

NNs_int <- NNs_orig %>% filter(location %in% given_locations)
dist_int <- dists_orig %>% filter(location %in% given_locations)
sigma_int <- sigmas_orig %>% filter(location %in% given_locations)

NNs_1 <- NNs_int
dists_1 <- dist_int
sigmas_1 <- sigma_int

###########################################################











