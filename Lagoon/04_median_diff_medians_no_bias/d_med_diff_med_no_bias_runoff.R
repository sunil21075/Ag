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

main_in <- "/data/hydro/users/Hossein/lagoon/02_run_off/"

in_dir <- paste0(main_in, "01_cum_runoffs/")
out_dir <- file.path(main_in, "02_median_diff_medians_no_bias/")
if (dir.exists(out_dir) == F) {dir.create(path = out_dir, recursive = T)}
######################################################################
##                                                                  ##
##                                                                  ##
######################################################################
raw_files <- c("all_ann_cum_runoff_LD.rds", 
               "all_chunk_cum_runoff_LD.rds", 
               "all_monthly_cum_runoff_LD.rds", 
               "all_wtr_yr_cum_runoff_LD.rds")

target_columns <- c("annual_cum_runbase", "chunk_cum_runbase", 
                    "monthly_cum_runbase", "annual_cum_runbase")

output_names <- c("med_diff_med_ann_runoff.rds", "med_diff_med_chunk_runoff.rds", 
                  "med_diff_med_month_runoff.rds", "med_diff_med_wtr_yr_runoff.rds")

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



