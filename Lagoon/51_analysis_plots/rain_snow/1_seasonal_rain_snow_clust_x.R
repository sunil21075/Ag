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
plot_dir <- paste0(data_base, "narrowed_rain_snow_fractions/seasonal/clust_x/")
if (dir.exists(plot_dir) == F) {dir.create(path = plot_dir, recursive = T)}
print (plot_dir)
############################################################################

AV_fileNs <- "seasonal_fracs"

AV_y_lab <- "cum. precip. (mm)"
AV_title <- paste0("seasonal cum. precip.")
AV_tg_col <- "seasonal_cum_precip"

AVs <- readRDS(paste0(data_base,"seasonal_fracs.rds")) %>% data.table()
AVs <- subset(AVs, select = c("location", "cluster", "year", "time_period", 
                              "model", "emission",
                              "seasonal_cum_precip", "rain_fraction", "snow_fraction",
                              "season"))

AVs_45 <- AVs %>% filter(emission=="RCP 4.5") %>% data.table()
AVs_85 <- AVs %>% filter(emission=="RCP 8.5") %>% data.table(); rm(AVs)

season_types <- c("fall", "winter", "spring", "summer")
season_g <- "fall"

for (season_g in season_types){
  subttl <- paste0(" (", season_g, " season)")
  curr_AVs_85 <- AVs_85 %>% filter(season == season_g) %>% data.table()
  curr_AVs_45 <- AVs_45 %>% filter(season == season_g) %>% data.table()

  #########
  ######### Actual value plots
  #########
  quans_85 <- find_quantiles(curr_AVs_85, tgt_col= AV_tg_col, time_type="seasonal")
  quans_45 <- find_quantiles(curr_AVs_85, tgt_col= AV_tg_col, time_type="seasonal")
  
  AV_box_85 <- seasonal_cum_box_clust_x(dt = curr_AVs_85, tgt_col = AV_tg_col,
                                        y_lab = AV_y_lab) +
               ggtitle(label= paste0(AV_title, subttl)) +
               coord_cartesian(ylim = c(quans_85[1], quans_85[2]))

  AV_box_45 <- seasonal_cum_box_clust_x(dt = curr_AVs_45, tgt_col = AV_tg_col,
                                        y_lab = AV_y_lab) +
               ggtitle(label= paste0(AV_title, subttl)) + 
               coord_cartesian(ylim = c(quans_45[1], quans_45[2]))
  #########
  ######### rain plot
  #########
  box_title <- paste0("rain fracion (", season_g, ")")
  quans_85 <- 100 * find_quantiles(curr_AVs_85, tgt_col= "rain_fraction", time_type="seasonal")
  quans_45 <- 100 * find_quantiles(curr_AVs_85, tgt_col= "rain_fraction", time_type="seasonal")

  rain_frac_85 <- seasonal_fraction_clust_x(data_tb = curr_AVs_85,
                                    y_lab = "rain fraction (%)", 
                                    tgt_col="rain_fraction") +
                  ggtitle(box_title) + 
                  coord_cartesian(ylim = c(quans_85[1], quans_85[2]))

  rain_85 <- ggarrange(plotlist = list(AV_box_85, rain_frac_85),
                       ncol = 1, nrow = 2, common.legend = TRUE, legend="bottom")

  ggsave(filename = paste0(season_g, "_rain_85.png"),
         plot = rain_85, width = 6, height = 5, units = "in", 
         dpi=400, device = "png", path = plot_dir)

  rain_frac_45 <- seasonal_fraction_clust_x(data_tb = curr_AVs_45,
                                    y_lab = "rain fraction (%)", 
                                    tgt_col="rain_fraction") +
                  ggtitle(box_title) +
                  coord_cartesian(ylim = c(quans_45[1], quans_45[2]))

  rain_45 <- ggarrange(plotlist = list(AV_box_45, rain_frac_45),
                       ncol = 1, nrow = 2, common.legend = TRUE, legend="bottom")

  ggsave(filename = paste0(season_g, "_rain_45.png"),
         plot = rain_45, width = 6, height = 5, units = "in", 
         dpi=400, device = "png", path = plot_dir)
  ##################################################################################
  ##################################################################################
  box_title <- paste0("snow fracion (", season_g, ")")
  quans_85 <- 100 * find_quantiles(curr_AVs_85, tgt_col= "snow_fraction", time_type="seasonal")
  quans_45 <- 100 * find_quantiles(curr_AVs_85, tgt_col= "snow_fraction", time_type="seasonal")

  snow_frac_85 <- seasonal_fraction_clust_x(data_tb = curr_AVs_85,
                                    y_lab = "snow fraction (%)", 
                                    tgt_col="snow_fraction") +
                  ggtitle(box_title) + 
                  coord_cartesian(ylim = c(quans_85[1], quans_85[2]))

  snow_85 <- ggarrange(plotlist = list(AV_box_85, snow_frac_85),
                       ncol = 1, nrow = 2, common.legend = TRUE, legend="bottom")

  ggsave(filename = paste0(season_g, "_snow_85.png"),
         plot = snow_85, width = 6, height = 5, units = "in", 
         dpi=400, device = "png", path = plot_dir)

  snow_frac_45 <- seasonal_fraction_clust_x(data_tb = curr_AVs_45,
                                    y_lab = "snow fraction (%)", 
                                    tgt_col="snow_fraction") +
                  ggtitle(box_title) +
                  coord_cartesian(ylim = c(quans_45[1], quans_45[2]))

  snow_45 <- ggarrange(plotlist = list(AV_box_45, snow_frac_45),
                       ncol = 1, nrow = 2, common.legend = TRUE, legend="bottom")
  ggsave(filename = paste0(season_g, "_snow_45.png"),
         plot = snow_45, width = 6, height = 5, units = "in", 
         dpi=400, device = "png", path = plot_dir)  
  print(paste0(season_g, "_snow_45.png"))  
}



