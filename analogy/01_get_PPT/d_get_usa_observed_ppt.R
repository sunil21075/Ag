
.libPaths("/data/hydro/R_libs35")
.libPaths()
library(tidyverse)
library(lubridate)
library(dplyr)
library(data.table)

source_path = "/home/hnoorazar/analog_codes/chill_core.R"
source(source_path)

options(digit=9)
options(digits=9)

#################################################################
main_out = "/data/hydro/users/Hossein/analog/usa/percipitation/"
param_dir = file.path("/home/hnoorazar/cleaner_codes/parameters/")

locations_list = read.table(paste0(param_dir, "all_us_locations_list.txt"), 
                            header=F, sep=",")
locations_list <- as.vector(locations_list$V1)
local_files = paste0("data_", locations_list)

# 2b. Note if working with a directory of historical data
hist <- TRUE

# 2c. If needed, make fastscratch folder matching the name of the 
#     current directory
if (dir.exists(main_out) == F) {
  dir.create(path = main_out, recursive = T)
}

# get files in current folder
dir_con <- dir()

# remove filenames that aren't data
dir_con <- dir_con[grep(pattern = "data_",
                        x = dir_con)]

# choose only files that we're interested in
dir_con <- dir_con[which(dir_con %in% local_files)]

print(dir_con)

# 3. Process the data -----------------------------------------------------

start_time <- Sys.time()

for(file in dir_con){

  met_data <- read_binary(file_path = file, hist = hist, no_vars=8)

  lat <- unlist(strsplit(file, "_"))[2]
  long <- unlist(strsplit(file, "_"))[3]
  met_data$location = paste0(lat, long)
  print (sort(colnames(met_data)))
  # data frame required
  met_data <- data.table(met_data)
  print (sort(colnames(met_data)))
  met_data <- met_data %>%
              select(-c(precip, tmin, SPH, SRAD, Rmax, Rmin, tmax, day, month)) %>%
              data.table()

  # 3e. Save output
  saveRDS(met_data, paste0(main_out, current_dir, "/", file, ".rds"))
  rm(met_data, lat, long)
}

end_time <- Sys.time()
print( end_time - start_time)

