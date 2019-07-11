###################################################################
.libPaths("/data/hydro/R_libs35")
.libPaths()
library(data.table)
library(lubridate)
library(dplyr)

options(digit=9)
options(digits=9)

# Time the processing of this batch of files
start_time <- Sys.time()

######################################################################
##                                                                  ##
##                      Define all paths                            ##
##                                                                  ##
######################################################################
lagoon_source_path = "/home/hnoorazar/lagoon_codes/core_lagoon.R"
source(lagoon_source_path)

data_dir <- "/data/hydro/users/Hossein/lagoon/00_raw_data/"

lagoon_out = "/data/hydro/users/Hossein/lagoon/"
main_out <- file.path(lagoon_out, "/01_storm_cumPrecip/cum_precip/")
if (dir.exists(main_out) == F) {dir.create(path = main_out, recursive = T)}

######################################################################
##                                                                  ##
##                                                                  ##
##                                                                  ##
######################################################################
raw_files <- c("raw_modeled_hist.rds", 
               "raw_observed.rds", 
               "raw_RCP45.rds", 
               "raw_RCP85.rds")

for(file in raw_files){
  curr_dt <- data.table(readRDS(paste0(data_dir, file)))
  curr_dt <- create_wtr_calendar(curr_dt, wtr_yr_start=10)
  curr_dt <- compute_wtr_yr_cum_precip(curr_dt)
  saveRDS(curr_dt, paste0(main_out, "/wtr_yr/", gsub("raw", "wtr_yr_sept_cum_precip", file)))

  curr_dt <- curr_dt %>%
             group_by(location, wtr_yr, model, emission, time_period) %>%
             slice(n()) %>%
             data.table()
  saveRDS(curr_dt, paste0(main_out, "/wtr_yr/", 
                          gsub("raw", "wtr_yr_sept_cum_precip_last_day", file)))


}

end_time <- Sys.time()
print( end_time - start_time)



