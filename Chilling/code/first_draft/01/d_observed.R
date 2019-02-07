
.libPaths("/data/hydro/R_libs35")
.libPaths()

library(chillR)
library(MESS)
library(xts)
library(tidyverse)

source_path = "/data/hydro/users/Hossein/chill/data_by_core/chill_core.R"
source(source_path)

# devtools::session_info()
options(digits=9)

# 2. Pre-processing prep --------------------------------------------------
# 2a. Only use files in geographic locations we're interested in
param_dir = "/home/hnoorazar/chilling_codes/parameters/"

local_files <- read.delim(file = paste0(param_dir, "file_list.txt"), header = F)
local_files <- as.vector(local_files$V1)

# 2b. Note if working with a directory of historical data
hist <- TRUE

# Define main output path
main_out <- file.path("/data/hydro/users/Hossein/chill/data_by_core/observed/")

if (dir.exists(main_out) == F) {
  dir.create(path = main_out, recursive = T)
}

# 2d. Prep list of files for processing

# get files in current folder
dir_con <- dir()

# remove filenames that aren't data
dir_con <- dir_con[grep(pattern = "data_",
                        x = dir_con)]

# choose only files that we're interested in
dir_con <- dir_con[which(dir_con %in% local_files)]

print(dir_con)
# 3. Process the data -----------------------------------------------------
# Time the processing of this batch of files
start_time <- Sys.time()

for(file in dir_con){
  # 3a. read in binary meteorological data file from specified path
  met_data <- read_binary(file_path = file,
                          hist = hist, no_vars=8)

  # I make the assumption that lat always has same number of decimal points
  lat <- as.numeric(substr(x = file, start = 6, stop = 13))

  # data frame required
  met_data <- as.data.frame(met_data)
  
  # 3b. Clean it up

  # rename needed columns
  met_data <- met_data %>%
              rename(Year = year,
                     Month = month,
                     Day = day,
                     Tmax = tmax,
                     Tmin = tmin) %>%
              select(-c(precip, windspeed, SPH, SRAD, Rmax, Rmin)) %>%
              data.frame()
  # 3c. Get hourly interpolation

  # generate hourly data
  met_hourly <- stack_hourly_temps(weather = met_data,
                                   latitude = lat)

  # save only the necessary list item
  met_hourly <- met_hourly[[1]]

  # 3d. Run the chill accumulation model and sum up by day
  # we want this on a seasonal basis specific to chill
  met_hourly <- met_hourly %>%
    mutate(Chill_season = case_when(
      # If Jan:Aug then part of chill season of prev year - current year
      Month %in% c(1:8) ~ paste0("chill_", (Year - 1), "-", Year),
      # If Sept:Dec then part of chill season of current year - next year
      Month %in% c(9:12) ~ paste0("chill_", Year, "-", (Year + 1))
    ))
  
  # sum within a day using NON-cumulative chill portions
  met_daily <- met_hourly %>%
               group_by(Chill_season) %>% # should maintain correct day, time order
               mutate(chill = Dynamic_Model(HourTemp = Temp, summ = F)) %>%
               group_by(Chill_season, Year, Month, Day) %>%
               summarise(Daily_portions = sum(chill))
  
  met_daily <- met_daily %>%
               group_by(Chill_season) %>%
               mutate(Cume_portions = cumsum(Daily_portions))
  
  # 3e. Save output
  write.table(x = met_daily,
              file = file.path(main_out,
                               paste0("chill_output_",
                                      file,
                                      ".txt")),
              row.names = F)
  # Remove objects not needed in future iterations
  rm(met_data, met_hourly, met_daily)
}

# How long did it take?
end_time <- Sys.time()

print( end_time - start_time)

