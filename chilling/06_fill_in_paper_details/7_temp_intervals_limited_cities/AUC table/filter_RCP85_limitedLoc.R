#
# This is obtaied by copying and modifying "plot_densities.R"
# To plot densities for specific locations
#

#####################################################
###                                               ###
###             Sept. thru Apr.                   ###
###                                               ###
#####################################################
rm(list=ls())

.libPaths("/data/hydro/R_libs35")
.libPaths()

library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr) # for ggarrange
options(digit=9)
options(digits=9)

base_in <- "/data/hydro/users/Hossein/chill/7_time_intervals/"
data_dir <- file.path(base_in, "RDS_files/")

modeled  <- readRDS(paste0(data_dir, "/modeled.rds")) %>% data.table()

modeled <- modeled %>% filter(scenario != "historical")
modeled <- modeled %>% filter(scenario != "rcp45")

modeled <- modeled %>% 
           filter(city %in% c("Omak", "Eugene",  "Walla Walla", "Yakima")) %>%
           data.table()

saveRDS(modeled, paste0(data_dir, "modeled_RCP85.rds"))


