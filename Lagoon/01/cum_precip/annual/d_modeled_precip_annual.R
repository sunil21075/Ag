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
main_out <- file.path(lagoon_out, "/01/cum_precip/")
if (dir.exists(main_out) == F) {dir.create(path = main_out, recursive = T)}

######################################################################
##                                                                  ##
##                                                                  ##
##                                                                  ##
######################################################################

for(file in dir_con){
  met_data <- read_binary(file_path = file, hist = hist, no_vars=4)
  met_data <- data.table(met_data)

  location <- substr(file, start = 6, stop = 24)

  # Clean it up
  met_data <- met_data %>%
              select(c(precip, year, month, day)) %>%
              data.table()

  met_data$location <- location
  met_data <- put_time_period(met_data, observed=FALSE)

  all_data <- rbind(all_data, met_data)
}

new_col_order <- c("location", "year", "month", "day", "precip", "time_period")
setcolorder(all_data, new_col_order)
print ("before_saving")
print(current_out)
saveRDS(all_data, paste0(current_out, "/raw.rds"))

# How long did it take?
end_time <- Sys.time()
print( end_time - start_time)

