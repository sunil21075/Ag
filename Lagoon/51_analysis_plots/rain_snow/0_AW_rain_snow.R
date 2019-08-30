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
############################################################################

data_base <- "/Users/hn/Desktop/Desktop/Kirti/check_point/lagoon/rain_snow_fractions/"

AV_fileNs <- c("annual_fracs", "wtr_yr_fracs")
timeP_ty_middN <- c("ann", "wtr_yr")
timeP_ty <- 1

for (timeP_ty in 1:2){ # annual or wtr_yr?
  ###############################################################
  # set up title stuff
  # 
  if (timeP_ty_middN[timeP_ty]== "ann"){
     title_time <- "calendar year"
     } else if (timeP_ty_middN[timeP_ty]== "wtr_yr"){
     title_time <- "water year"
  }
  AV_y_lab <- "cum. precip. (mm)"
  AV_title <- paste0("ann. cum. precip.", " (", title_time, ")")
  AV_tg_col <- "annual_cum_precip"
  
  ###############################################################

  AVs <- readRDS(paste0(data_base, AV_fileNs[timeP_ty], ".rds")) %>% data.table()
  AVs <- subset(AVs, select = c("location", "cluster", "year", "time_period", 
                                "model", "emission",
                                "annual_cum_precip", "rain_fraction", "snow_fraction"))
  
  AVs_45 <- AVs %>% filter(emission=="RCP 4.5") %>% data.table()
  AVs_85 <- AVs %>% filter(emission=="RCP 8.5") %>% data.table()
  rm(AVs)

  AV_box_85 <- ann_wtrYr_chunk_cum_box_cluster_x(dt = AVs_85, y_lab = AV_y_lab, 
                                                 tgt_col = AV_tg_col) + ggtitle(AV_title)
  AV_box_45 <- ann_wtrYr_chunk_cum_box_cluster_x(dt = AVs_45, y_lab = AV_y_lab, 
                                                 tgt_col = AV_tg_col) + ggtitle(AV_title)

  ###################################
  #####
  ##### difference plots
  #####
  ###################################
  box_title <- "unbiased differences"
  box_subtitle <- "for each model median is\ntaken over years, separately"
  unbias_perc_diff_85 <- ann_wtrYr_chunk_cum_box_cluster_x(dt = unbias_diff_85,
                                                           y_lab = "differences (%)",
                                                           tgt_col = "perc_diff",
                                                           ttl = box_title, 
                                                           subttl = box_subtitle) + 
                         ggtitle(box_title)

  unbias_perc_diff_45 <- ann_wtrYr_chunk_cum_box_cluster_x(dt = unbias_diff_45,
                                                           y_lab = "differences (%)",
                                                           tgt_col = "perc_diff",
                                                           ttl = box_title, 
                                                           subttl = box_subtitle) + 
                         ggtitle(box_title)
  ###################################
  #####
  ##### arrange plots
  #####
  ###################################

  RCP45 <- ggarrange(plotlist = list(AV_box_45, unbias_perc_diff_45),
                     ncol = 1, nrow = 2, common.legend = TRUE, legend="bottom")

  RCP85 <- ggarrange(plotlist = list(AV_box_85, unbias_perc_diff_85),
                     ncol = 1, nrow = 2, common.legend = TRUE, legend="bottom")
    
  plot_dir <- paste0(in_dir, "narrowed_", d_type, "/", timeP_ty_middN[timeP_ty], "/")
  if (dir.exists(plot_dir) == F) {dir.create(path = plot_dir, recursive = T)}
    
  ggsave(filename = paste0(timeP_ty_middN[timeP_ty], "_RCP45.png"),
         plot = RCP45, width = 6, height = 3, units = "in", 
         dpi=400, device = "png", path = plot_dir)

  ggsave(filename = paste0(timeP_ty_middN[timeP_ty], "_RCP85.png"),
         plot = RCP85,  width = 6, height = 3, units = "in", 
         dpi = 400, device = "png", path = plot_dir)
  print (plot_dir)
}


