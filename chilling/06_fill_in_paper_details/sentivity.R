
# 1. Load packages --------------------------------------------------------
rm(list=ls())
library(ggpubr) # library(plyr)
library(tidyverse)
library(data.table)
library(ggplot2)
options(digits=9)
options(digit=9)

##############################################################################
############# 
#############              ********** start from here **********
#############
##############################################################################
param_dir <- "/Users/hn/Documents/00_GitHub/Ag/chilling/parameters/"
limited_locs <- read.csv(paste0(param_dir, "limited_locations.csv"), 
                                header=T, sep=",", as.is=T)

limited_locs$location <- paste0(limited_locs$lat, "_", limited_locs$long)
limited_locs <- limited_locs %>% filter(city %in% c("Omak", "Eugene", "Walla Walla", "Yakima"))

##############################################################################

main_in_dir <- "/Users/hn/Documents/01_research_data/chilling/01_data/02/"
write_dir <- "/Users/hn/Documents/00_GitHub/Ag_papers/Chill_Paper/figures/Accum_CP_Sept_Apr/"
if (dir.exists(file.path(write_dir)) == F) { dir.create(path = write_dir, recursive = T)}

sept_summary_comp <- data.table(readRDS(paste0(main_in_dir, "sept_summary_comp.rds")))
mid_sept_summary_comp <- data.table(readRDS(paste0(main_in_dir, "mid_sept_summary_comp.rds")))

sept_summary_comp <- sept_summary_comp %>% 
                     filter(time_period %in% c("2026-2050", "2051-2075", "2076-2099")) %>% 
                     data.table()

mid_sept_summary_comp <- mid_sept_summary_comp %>% 
                         filter(time_period %in% c("2026-2050", "2051-2075", "2076-2099")) %>% 
                         data.table()

summary_comp <- rbind(sept_summary_comp, mid_sept_summary_comp)
rm(sept_summary_comp, mid_sept_summary_comp)

summary_comp$location <- paste0(summary_comp$lat, "_", summary_comp$long)

summary_comp <- summary_comp %>% filter(location %in% limited_locs$location)
summary_comp <- left_join(summary_comp, limited_locs)

summary_comp <- within(summary_comp, remove(location, lat, long, thresh_20, 
                                            thresh_25, thresh_30, thresh_35,
                                            thresh_40, thresh_45, thresh_50,
                                            thresh_55, thresh_60, thresh_65,
                                            thresh_70, thresh_75, sum_J1, sum_F1, sum_M1, sum))

######################################################################
#####
#####                Clean data
#####
#######################################################################
summary_comp <- summary_comp %>% filter(time_period %in% c("2026-2050", "2051-2075", "2076-2099")) %>% data.table()
summary_comp$emission[summary_comp$emission=="rcp45"] <- "RCP 4.5"
summary_comp$emission[summary_comp$emission=="rcp85"] <- "RCP 8.5"
summary_comp <- summary_comp %>% filter(emission == "RCP 8.5") %>% data.table()

unique(summary_comp$emission)
unique(summary_comp$time_period)


summary_comp_stats <- summary_comp %>%
                      group_by(city, emission, time_period, start) %>%
                      summarise_at(.funs = funs(mininum = min), vars(sum_A1)) %>% 
                      data.table()

# Data frame for historical values to be used for these figures
# summary_comp_hist <- summary_comp %>%
#                      filter(model == "observed") %>%
#                      group_by(city, year) %>%
#                      summarise_at(.funs = funs(med = median), vars(thresh_20:sum_A1))
