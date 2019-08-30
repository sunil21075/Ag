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

data_base <- "/Users/hn/Desktop/Desktop/Kirti/check_point/lagoon/"
in_dir_ext <- c("precip", "runbase")
unbias_dir_ext <- "/02_med_diff_med_no_bias/"
precip_AV_fileNs <- c("seasonal_cum_precip")
runoff_AV_fileNs <- c("seasonal_cum_runbase")

cluster_types <- c("least precip", "lesser precip", "less precip", "most precip")
timeP_ty_middN <- c("seasonal")
av_tg_col_pref <- c("seasonal_cum_")
av_titles <- c("seasonal cum. ")

dt_type <-  in_dir_ext[1]
in_dir <- paste0(data_base, dt_type, "/")
timeP_ty <- 1
clust_g <- cluster_types[1]

for (dt_type in in_dir_ext){ # precip or runoff?
  in_dir <- paste0(data_base, dt_type, "/")
  for (timeP_ty in 1:1){ # annual or chunk or wtr_yr?
    if (dt_type=="precip"){
     files <- precip_AV_fileNs
     AV_y_lab <- "cum. precip. (mm)"
     AV_tg_col <- paste0(av_tg_col_pref[timeP_ty], dt_type)
     AV_title <- paste0(av_titles[timeP_ty], "precip.")

     } else if (dt_type=="runbase"){
      files <- runoff_AV_fileNs
      AV_y_lab <- "cum. runoff (mm)"
      AV_tg_col <- paste0(av_tg_col_pref[timeP_ty], "runbase")
      AV_title <- paste0(av_titles[timeP_ty], "runoff.")
    }

    AVs <- readRDS(paste0(in_dir, files[timeP_ty], ".rds")) %>% data.table()
    unbias_diff <- readRDS(paste0(in_dir, unbias_dir_ext, "detail_med_diff_med_", 
                                  timeP_ty_middN[timeP_ty], "_", dt_type, ".rds")) %>% 
                   data.table()

    AVs_45 <- AVs %>% filter(emission=="RCP 4.5") %>% data.table()
    AVs_85 <- AVs %>% filter(emission=="RCP 8.5") %>% data.table()

    unbias_diff_45 <- unbias_diff %>% filter(emission=="RCP 4.5") %>% data.table()
    unbias_diff_85 <- unbias_diff %>% filter(emission=="RCP 8.5") %>% data.table(); 
    rm(AVs, unbias_diff)

    for (clust_g in cluster_types){
      subttl <- paste0("(", clust_g, " regions)")
      curr_AVs_85 <- AVs_85 %>% filter(cluster == clust_g) %>% data.table()
      curr_AVs_45 <- AVs_45 %>% filter(cluster == clust_g) %>% data.table()

      curr_diff_45 <- unbias_diff_45 %>% filter(cluster == clust_g) %>% data.table()
      curr_diff_85 <- unbias_diff_85 %>% filter(cluster == clust_g) %>% data.table()
      #########
      ######### Actual value plots
      #########
      AV_box_85 <- seasonal_cum_box_season_x(dt = curr_AVs_85, tgt_col = AV_tg_col,
                                             y_lab = paste0("seasonal ", AV_y_lab))+ 
                   ggtitle(label= paste0(AV_title, " ", subttl)) 

      AV_box_45 <- seasonal_cum_box_season_x(dt = curr_AVs_45, tgt_col = AV_tg_col,
                                             y_lab = paste0("seasonal ", AV_y_lab)) + 
                   ggtitle(label= paste0(AV_title, " ", subttl))
      #########
      ######### difference plot
      #########
      box_title <- paste0("unbiased differences", subttl)
      box_subtitle <- "for each model median is\ntaken over years, separately"
      unbias_perc_diff_85 <- seasonal_cum_box_season_x(dt = curr_diff_85,
                                                      y_lab = "differences (%)",
                                                      tgt_col = "perc_diff") + 
                             ggtitle(box_title)

      unbias_perc_diff_45 <- seasonal_cum_box_season_x(dt = curr_diff_45,
                                                      y_lab = "differences (%)",
                                                      tgt_col = "perc_diff") + 
                             ggtitle(box_title)
      RCP45 <- ggarrange(plotlist = list(AV_box_45, unbias_perc_diff_45),
                         ncol = 1, nrow = 2, common.legend = TRUE, legend="bottom")

      RCP85 <- ggarrange(plotlist = list(AV_box_85, unbias_perc_diff_85),
                         ncol = 1, nrow = 2, common.legend = TRUE, legend="bottom")

      plot_dir <- paste0(in_dir, "narrowed_", dt_type, "/seasonal/season_x/")
      if (dir.exists(plot_dir) == F) {dir.create(path = plot_dir, recursive = T)}

      ggsave(filename = paste0(gsub("\ ", "_", clust_g), "_", 
                               timeP_ty_middN[timeP_ty], "_RCP45.png"),
             plot = RCP45, width = 6, height = 3.5, units = "in", 
             dpi=400, device = "png", path = plot_dir)

      ggsave(filename = paste0(gsub("\ ", "_", clust_g), "_", 
                               timeP_ty_middN[timeP_ty], "_RCP85.png"),
             plot = RCP85,  width = 6, height = 4, units = "in", 
             dpi = 400, device = "png", path = plot_dir)
      
      # ggsave(filename = paste0(gsub("\ ", "_", clust_g), "_", 
      #                          timeP_ty_middN[timeP_ty], "_RCP85.png"),
      #        plot = AV_box_85, 
      #        width = 6, height = 2.5, units = "in", 
      #        dpi = 400, device = "png",
      #        path = plot_dir)

      # ggsave(filename = paste0(gsub("\ ", "_", clust_g), "_", 
      #                          timeP_ty_middN[timeP_ty], "_RCP45.png"),
      #        plot = AV_box_45, 
      #        width = 6, height = 2.5, units = "in", 
      #        dpi = 400, device = "png",
      #        path = plot_dir)
      
    }
    print (plot_dir)
  }
}


