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
main_out <- file.path(lagoon_out, "/01/storm/")
if (dir.exists(main_out) == F) {dir.create(path = main_out, recursive = T)}

######################################################################
##                                                                  ##
##                                                                  ##
##                                                                  ##
######################################################################
raw_files <- c("raw_observed.rds")
obs = TRUE

for(file in raw_files){
  curr_dt <- data.table(readRDS(paste0(data_dir, file)))
  curr_dt <- design_storm_4_allLoc_allMod_from_raw(curr_dt, observed = obs)
  # curr_dt <- unique(curr_dt)
  saveRDS(curr_dt, paste0(main_out, "/", gsub("raw", "storm", file)))
}

end_time <- Sys.time()
print( end_time - start_time)



