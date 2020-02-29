# """
# This driver will comupte the percentage diff between future CPs and observed CPs.
# and generate a map of colors for them.
# 
# """
rm(list=ls())

library(ggmap)
library(ggpubr)
library(lubridate)
library(purrr)
library(scales)
library(tidyverse)
library(maps)
library(data.table)
library(dplyr)

options(digits=9)
options(digit=9)


core_path = "/Users/hn/Documents/00_GitHub/Ag/chilling/chill_core.R"
plot_core_path = "/Users/hn/Documents/00_GitHub/Ag/chilling/chill_plot_core.R"
source(core_path)
source(plot_core_path)


data_dir = "/Users/hn/Documents/01_research_data/Ag_check_point/chilling/01_data/02/"
param_dir <- "/Users/hn/Documents/00_GitHub/Ag/chilling/parameters/"

LocationGroups_NoMontana <- read.csv(paste0(param_dir, "LocationGroups_NoMontana.csv"), 
                                     header=T, sep=",", as.is=T)

remove_montana <- function(data_dt, LocationGroups_NoMontana){
  if (!("location" %in% colnames(data_dt))){
    data_dt$location <- paste0(data_dt$lat, "_", data_dt$long)
  }
  data_dt <- data_dt %>% filter(location %in% LocationGroups_NoMontana$location)
  return(data_dt)
}


sept_summary_comp <- readRDS(paste0(data_dir, "sept_summary_comp.rds")) %>%
                     data.table()

head(sept_summary_comp, 2)
dim(sept_summary_comp)

keep_cols <- c("chill_season", "location", 
               "sum_A1", "year", "model", "emission", "time_period", 
               "start")

sept_summary_comp <- subset(sept_summary_comp, select=keep_cols)

sept_summary_comp <- sept_summary_comp %>% 
                     filter(!(time_period %in% c("2006-2025", "1950-2005"))) %>%
                     data.table()

sept_summary_comp <- remove_montana(sept_summary_comp, LocationGroups_NoMontana)


###### Change time period for sake of plotting:
# sept_summary_comp$time_period[sept_summary_comp$time_period== "2025_2050"] = "2025-2050"
# sept_summary_comp$time_period[sept_summary_comp$time_period== "2051_2075"] = "2051-2075"
# sept_summary_comp$time_period[sept_summary_comp$time_period== "2076_2100"] = "2076-2099"

sept_summary_comp$time_period[sept_summary_comp$model== "observed"] <- "Observed"








