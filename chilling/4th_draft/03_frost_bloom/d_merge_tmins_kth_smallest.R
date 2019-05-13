.libPaths("/data/hydro/R_libs35")
.libPaths()
library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)

start_time <- Sys.time()
###
### This takes 2 hours or so.
###

source_path = "/home/hnoorazar/chilling_codes/current_draft/chill_core.R"
source(source_path)
options(digit=9)
options(digits=9)

historical <- data.table()
rcp45 <- data.table()
rcp85 <- data.table()

main_in <- "/data/hydro/users/Hossein/chill/frost_bloom_initian_database/modeled/"
out_dir <- "/data/hydro/users/Hossein/chill/frost_bloom_initian_database/frost_RDS/"

models <- c("bcc-csm1-1", "bcc-csm1-1-m", "BNU-ESM", "CanESM2", "CCSM4",
            "CNRM-CM5", "CSIRO-Mk3-6-0", "GFDL-ESM2G", "GFDL-ESM2M", "HadGEM2-CC365",
            "HadGEM2-ES365", "inmcm4", "IPSL-CM5A-LR", "IPSL-CM5A-MR", "IPSL-CM5B-LR", 
            "MIROC5", "MIROC-ESM-CHEM", "MRI-CGCM3", "NorESM1-M")

emission_types <- c("historical", "rcp45", "rcp85")

for (model in models){
  curr_hist <- data.table(readRDS(paste0(main_in, model, "/historical_data.rds")))
  curr_45 <- data.table(readRDS(paste0(main_in, model, "/rcp45_data.rds")))
  curr_85 <- data.table(readRDS(paste0(main_in, model, "/rcp85_data.rds")))
  
  curr_hist <- within(curr_hist, remove(warm_cold))
  curr_45 <- within(curr_45, remove(warm_cold))
  curr_85 <- within(curr_85, remove(warm_cold))

  saveRDS(curr_hist, paste0(main_in, model, "/historical_data.rds"))
  saveRDS(curr_45, paste0(main_in, model, "/rcp45_data.rds"))
  saveRDS(curr_85, paste0(main_in, model, "/rcp85_data.rds"))


  curr_hist$model <- model
  curr_45$model <- model
  curr_85$model <- model

  curr_45$emission <- "RCP 4.5"
  curr_85$emission <- "RCP 8.5"
  
  historical <- rbind(historical, curr_hist)
  rcp45 <- rbind(rcp45, curr_45)
  rcp85 <- rbind(rcp85, curr_85)
}

historical_45 <- historical
historical_85 <- historical

historical_45$emission <- "RCP 4.5"
historical_85$emission <- "RCP 8.5"

main_in_obs <- "/data/hydro/users/Hossein/chill/frost_bloom_initian_database/"
observed <- data.table(readRDS(paste0(main_in_obs, "observed_dt.rds")))

observed_45 <- observed
observed_85 <- observed

observed_45$emission <- "RCP 4.5"
observed_85$emission <- "RCP 8.5"

rcp45 <- rbind(rcp45, historical_45, observed_45)
rcp85 <- rbind(rcp85, historical_85, observed_85)

saveRDS(rcp45, paste0(main_in_obs, "/rcp45.rds"))
saveRDS(rcp85, paste0(main_in_obs, "/rcp85.rds"))

###################################################################
#
#     Compute first and fifth frost day of year
#
###################################################################
# in_n_out <- "/data/hydro/users/Hossein/chill/frost_bloom_initian_database/frost_RDS/"
# rcp45 <- data.table(readRDS(paste0(in_n_out, "rcp45.rds")))
# rcp85 <- data.table(readRDS(paste0(in_n_out, "rcp85.rds")))
print ("line 83")
saveRDS(rcp45, paste0(out_dir, "rcp45.rds"))
saveRDS(rcp85, paste0(out_dir, "rcp85.rds"))

print ("line 86")
first_frost_45 <- kth_smallest_in_group(rcp45, target_column = "dayofyear", k=1)
first_frost_85 <- kth_smallest_in_group(rcp85, target_column = "dayofyear", k=1)
first_frost <- rbind(first_frost_45, first_frost_85)
saveRDS(first_frost, paste0(out_dir, "first_frost.rds"))
rm(first_frost_45, first_frost_85, first_frost)

print ("line 92")
fifth_frost_45 <- kth_smallest_in_group(rcp45, target_column = "dayofyear", k=5)
fifth_frost_85 <- kth_smallest_in_group(rcp85, target_column = "dayofyear", k=5)
fifth_frost <- rbind(fifth_frost_45, fifth_frost_85)
saveRDS(fifth_frost, paste0(out_dir, "fifth_frost.rds"))

rm(fifth_frost_45, fifth_frost_85, fifth_frost)
####################################################################
#
#      Compute medians
#
####################################################################
in_n_out <- "/data/hydro/users/Hossein/chill/frost_bloom_initian_database/frost_RDS/"

#######################################
####################################### FIRST frost median (1 st)
#######################################

first_frost_45 <- data.table(readRDS(paste0(in_n_out, "first_frost_45.rds")))
first_frost_85 <- data.table(readRDS(paste0(in_n_out, "first_frost_85.rds")))

print ("line 109")
first_frost_medians_45 <- first_frost_45 %>%
                          group_by(time_period, model, location, emission) %>%
                          summarise(median = median(dayofyear)) %>%
                          data.table()

first_frost_medians_85 <- first_frost_85 %>%
                          group_by(time_period, model, location, emission) %>%
                          summarise(median = median(dayofyear)) %>%
                          data.table()

first_frost_medians <- rbind(first_frost_medians_45, first_frost_medians_85)
saveRDS(first_frost_medians, paste0(out_dir, "first_frost_medians.rds"))
rm(first_frost_medians, first_frost_medians_45, first_frost_medians_85)

#######################################
####################################### FIFTH frost median (5 th)
#######################################

fifth_frost_45 <- data.table(readRDS(paste0(in_n_out, "fifth_frost_45.rds")))
fifth_frost_85 <- data.table(readRDS(paste0(in_n_out, "fifth_frost_85.rds")))

fifth_frost_medians_45 <- fifth_frost_45 %>%
                          group_by(time_period, model, location, emission) %>%
                          summarise(median = median(dayofyear)) %>%
                          data.table()

fifth_frost_medians_85 <- fifth_frost_85 %>%
                          group_by(time_period, model, location, emission) %>%
                          summarise(median = median(dayofyear)) %>%
                          data.table()

fifth_frost_medians <- rbind(fifth_frost_medians_45, fifth_frost_medians_85)
saveRDS(fifth_frost_medians, paste0(out_dir, "fifth_frost_medians.rds"))


end_time <- Sys.time()
print( end_time - start_time)




