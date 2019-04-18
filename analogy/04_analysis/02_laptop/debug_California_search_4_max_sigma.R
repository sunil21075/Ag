######################################################################
rm(list=ls())
library(lubridate)
library(ggpubr)
library(purrr)
library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)
library(maps)

options(digit=9)
options(digits=9)

##########################################################################################
#
#          convert the goddamn location to character to factor levels are killed!
#
##########################################################################################

data_dir <- "/Users/hn/Desktop/Desktop/Kirti/check_point/analogs/w_gen_w_prec/500/"
out_dir <- "/Users/hn/Desktop/"

file_pref <- "all_close_analogs_"
time_periods <- c("2026_2050", "2051_2075", "2076_2095")
emissions <- c("_rcp45.rds", "_rcp85.rds")

for (time in time_periods){
  for (emission in emissions){
    dt <- data.table(readRDS(paste0(data_dir, file_pref, time, emission)))
    
    dt$query_loc <- as.character(dt$query_loc)
    dt$analog <-  as.character(dt$analog)
    dt$st_county <- as.character(dt$st_county) # _unique_ does not have this
    saveRDS(dt, paste0(out_dir, file_pref, time, emission))
  }
}
##########################################################################################
######################################################################
rm(list=ls())
library(lubridate)
library(ggpubr)
library(purrr)
library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)
library(maps)

options(digit=9)
options(digits=9)
data_dir <- "/Users/hn/Desktop/Desktop/Kirti/check_point/analogs/w_gen_w_prec/500/"
out_dir <- "/Users/hn/Desktop/"

file_pref <- "all_close_analogs_"
time_periods <- c("2026_2050", "2051_2075", "2076_2095")
emissions <- c("_rcp45.rds", "_rcp85.rds")

# time <- time_periods[1]
# emission <- emissions[1]

for (time in time_periods){
  for (emission in emissions){
    dt <- data.table(readRDS(paste0(data_dir, file_pref, time, emission)))
    dt <- subset(dt, select = c(query_loc, query_year, ClimateScenario, sigma))

    sigma_data <- dt %>%
                  group_by(query_loc, query_year, ClimateScenario) %>%
                  summarize(max_sigma = max(sigma))

    unique_future_locs <- unique(dt$query_loc)
    unique_future_years <- unique(dt$query_year)
    unique_models <- unique(dt$ClimateScenario)

    N <- length(unique_future_locs) * length(unique_future_years) * length(unique_models)
    sigma_data <- setNames(data.table(matrix(nrow = N, ncol = 4)), 
                           c("query_loc", "query_year", "ClimateScenario", "max_sigma"))
    sigma_data$query_loc <- as.character(sigma_data$query_loc)
    sigma_data$query_year <- as.integer(sigma_data$query_year)
    sigma_data$ClimateScenario <- as.character(sigma_data$ClimateScenario)
    sigma_data$max_sigma <- as.numeric(sigma_data$max_sigma)
    counter <- 1
   

    for (location in unique_future_locs){
      for (year in unique_future_years){
        for (model in unique_models){
          curr_data <- dt %>% filter(query_loc == location & query_year==year & ClimateScenario==model) %>%
                       data.table()
          sigma_data[counter, ] <- curr_data %>% filter(sigma == max(curr_data$sigma))
          counter = counter + 1
        }
      }
    }
    file_name <- paste0("sigma_dbg_", time, "_", emission, ".rds")
    saveRDS(sigma_data, paste0(out_dir, file_name))
  }
}







