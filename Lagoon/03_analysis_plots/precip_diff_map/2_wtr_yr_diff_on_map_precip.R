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

fileN <- "wtr_yr_sept_all_last_days"
dt_tb <- data.table(readRDS(paste0(in_dir, fileN, ".rds")))
head(dt_tb, 2)

tgt_col <- "annual_cum_precip"

meds <- compute_median_diff_4_map(dt_tb, tgt_col=tgt_col)

min_diff <- min(meds$diff)
max_diff <- max(meds$diff)

min_diff_perc <- min(meds$perc_diff)
max_diff_perc <- max(meds$perc_diff)

emissions <- c("RCP 4.5", "RCP 8.5")
future_rn_pr <- c("2026-2050", "2051-2075", "2076-2099")

#######
#######     Difference of medians of annual precip
#######
for (em in emissions){
  for (rp in future_rn_pr){
    curr_dt <- meds %>%
               filter(emission == em & time_period==rp) %>%
               data.table()
    title <- paste0(em, " (", rp, ")")
    subtitle <- "Difference of medians of cum. precip. (Water Year)"
    assign(x = paste0(gsub(pattern = " ", 
                           replacement = "_", 
                           x = em),
                      "_",
                      gsub(pattern = "-", 
                           replacement = "_", 
                           x = rp)),
           value ={geo_map_of_diffs(dt = curr_dt, 
                                    col_col = "diff" , 
                                    minn = min_diff, maxx = max_diff,
                                    ttl = title, 
                                    subttl= subtitle)})

  }
}

diff_figs <- ggarrange(plotlist = list(RCP_4.5_2026_2050,
                                       RCP_8.5_2026_2050,
                                       RCP_4.5_2051_2075,
                                       RCP_8.5_2051_2075,
                                       RCP_4.5_2076_2099,
                                       RCP_8.5_2076_2099),
                       ncol = 2, nrow = 3,
                       common.legend = TRUE)

rm(RCP_4.5_2026_2050, RCP_8.5_2026_2050,
   RCP_4.5_2051_2075, RCP_8.5_2051_2075,
   RCP_4.5_2076_2099, RCP_8.5_2076_2099)

ggsave(filename = "precip_diff_medians_WTRYR.png", 
       plot = diff_figs, 
       width = 7, height = 8, units = "in", 
       dpi=600, device = "png",
       path = plot_dir)

#######
#######     Percentage perc_difference of medians of annual precip
#######
for (em in emissions){
  for (rp in future_rn_pr){
    curr_dt <- meds %>%
               filter(emission == em & time_period==rp) %>%
               data.table()
    title <- paste0(em, " (", rp, ")")
    subtitle <- "perc. difference of medians of cum. precip. (Water Year)"
    assign(x = paste0(gsub(pattern = " ", 
                           replacement = "_", 
                           x = em),
                      "_",
                      gsub(pattern = "-", 
                           replacement = "_", 
                           x = rp)),
           value ={geo_map_of_diffs(dt = curr_dt, 
                                    col_col = "perc_diff" , 
                                    min=min_diff_perc, max=max_diff_perc,
                                    ttl = title, 
                                    subttl= subtitle)})

  }
}

perc_diff_figs <- ggarrange(plotlist = list(RCP_4.5_2026_2050,
                                            RCP_8.5_2026_2050,
                                            RCP_4.5_2051_2075,
                                            RCP_8.5_2051_2075,
                                           RCP_4.5_2076_2099,
                                           RCP_8.5_2076_2099),
                           ncol = 2, nrow = 3,
                           common.legend = TRUE)

ggsave(filename = "precip_perc_diff_medians_WTRYR.png", 
       plot = perc_diff_figs, 
       width = 7, height = 8, units = "in", 
       dpi=600, device = "png",
       path = plot_dir)







