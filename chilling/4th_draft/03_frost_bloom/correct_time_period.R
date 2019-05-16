.libPaths("/data/hydro/R_libs35")
.libPaths()
library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)


data_dir <- "/data/hydro/users/Hossein/chill/frost_bloom_initian_database/frost_RDS/"

fifth_frost_medians <- data.table(readRDS(paste0(data_dir, "fifth_frost_medians.rds")))
fifth_frost_medians$time_period[fifth_frost_medians$time_period=="2076-2095"] = "2076-2099"
saveRDS(fifth_frost_medians, paste0(data_dir, "fifth_frost_medians.rds"))


fifth_frost <- data.table(readRDS(paste0(data_dir, "fifth_frost.rds")))
fifth_frost$time_period[fifth_frost$time_period=="2076-2095"] = "2076-2099"
saveRDS(fifth_frost, paste0(data_dir, "fifth_frost.rds"))


first_frost_medians <- data.table(readRDS(paste0(data_dir, "first_frost_medians.rds")))
first_frost_medians$time_period[first_frost_medians$time_period=="2076-2095"] = "2076-2099"
saveRDS(first_frost_medians, paste0(data_dir, "first_frost_medians.rds"))

first_frost <- data.table(readRDS(paste0(data_dir, "first_frost.rds")))
first_frost$time_period[first_frost$time_period=="2076-2095"] = "2076-2099"
saveRDS(first_frost, paste0(data_dir, "first_frost.rds"))



rcp45 <- data.table(readRDS(paste0(data_dir, "rcp45.rds")))
rcp45$time_period[rcp45$time_period=="2076-2095"] = "2076-2099"
saveRDS(rcp45, paste0(data_dir, "rcp45.rds"))



rcp85 <- data.table(readRDS(paste0(data_dir, "rcp85.rds")))
rcp85$time_period[rcp85$time_period=="2076-2095"] = "2076-2099"
saveRDS(rcp85, paste0(data_dir, "rcp85.rds"))