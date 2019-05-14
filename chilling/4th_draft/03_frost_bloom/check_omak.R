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


rcp85 <- data.table(readRDS("/data/hydro/users/Hossein/chill/frost_bloom_initian_database/frost_RDS/rcp85.rds"))
rcp85 <- rcp85 %>% filter(location == "48.40625_-119.53125" & time_period == "2076-2099")
print(length(unique(rcp85$model)))

rcp45 <- data.table(readRDS("/data/hydro/users/Hossein/chill/frost_bloom_initian_database/frost_RDS/rcp45.rds"))
rcp45 <- rcp45 %>% filter(location == "48.40625_-119.53125" & time_period == "2076-2099")
print(length(unique(rcp85$model)))
