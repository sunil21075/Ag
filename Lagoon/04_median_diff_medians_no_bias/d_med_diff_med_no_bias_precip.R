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

main_in <- "/data/hydro/users/Hossein/lagoon/01_cum_precip/"

in_dir <- paste0(main_in, "01_cumms/")
out_dir <- file.path(main_in, "02_median_diff_medians_no_bias/")
if (dir.exists(out_dir) == F) {dir.create(path = out_dir, recursive = T)}
######################################################################
##                                                                  ##
##                                                                  ##
######################################################################
raw_files <- c("ann_all_last_days.rds", 
               "Sept_March_all_last_days.rds", 
               "month_all_last_days.rds", 
               "wtr_yr_sept_all_last_days.rds")

target_columns <- c("annual_cum_precip", 
                    "chunk_cum_precip", 
                    "monthly_cum_precip", 
                    "annual_cum_precip")

output_names <- c("med_diff_med_ann_precip.rds", "med_diff_med_chunk_precip.rds", 
                  "med_diff_med_month_precip.rds", "med_diff_med_wtr_yr_precip.rds")

for(ii in 1:4){
  curr_dt <- data.table(readRDS(paste0(in_dir, raw_files[ii])))
  meds_detail <- median_diff_4_map_obs_or_modeled(dt = curr_dt, 
                                                  tgt_col=target_columns[ii], 
                                                  diff_from="1950-2005")
  meds <- median_of_diff_of_medians(meds_detail)
  saveRDS(meds_detail, paste0(out_dir, "detail_", output_names[ii]))
  saveRDS(meds, paste0(out_dir, output_names[ii]))
}


end_time <- Sys.time()
print( end_time - start_time)



