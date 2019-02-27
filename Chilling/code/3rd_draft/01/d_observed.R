###################################################################
#          **********                            **********
#          **********        WARNING !!!!        **********
#          **********                            **********
##
## DO NOT load any libraries here.
## And do not load any libraries on the drivers!
## Unless you are aware of conflicts between packages.
## I spent hours to figrue out what the hell is going on!
###################################################################

.libPaths("/data/hydro/R_libs35")
.libPaths()
library(chillR)
library(tidyverse)
library(lubridate)

source_path = "/home/hnoorazar/chilling_codes/2_second_draft/chill_core.R"
source(source_path)

options(digit=9)
options(digits=9)

######################################################################
##                                                                  ##
##              Terminal/shell/bash arguments                       ##
##                                                                  ##
######################################################################

args = commandArgs(trailingOnly=TRUE)
model_type = args[1]

######################################################################
# Define main output path
chill_out = "/data/hydro/users/Hossein/chill/data_by_core/"

if (model_type == "dynamic"){
  main_out <- file.path(chill_out, "cleanup/dynamic/observed/")
} else if (model_type == "utah"){
  main_out <- file.path(chill_out, "cleanup/utah/observed/")
}

# 2a. Only use files in geographic locations we're interested in
param_dir = file.path("/home/hnoorazar/chilling_codes/parameters/")
local_files <- read.delim(file = paste0(param_dir, "file_list.txt"), header = F)
local_files <- as.vector(local_files$V1)

# 2b. Note if working with a directory of historical data
hist <- TRUE

# 2c. If needed, make fastscratch folder matching the name of the 
#     current directory
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
  met_data <- read_binary(file_path = file, hist = hist, no_vars=8)

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
  if (model_type == "dynamic"){
    met_daily <- met_hourly %>%
               group_by(Chill_season) %>% # should maintain correct day, time order
               mutate(chill = Dynamic_Model(HourTemp = Temp, summ = F)) %>%
               group_by(Chill_season, Year, Month, Day) %>%
               summarise(Daily_portions = sum(chill))
  } else if (model_type== "utah"){
    met_daily <- met_hourly %>%
                 group_by(Chill_season) %>% # should maintain correct day, time order
                 mutate(chill = Utah_Model(HourTemp = Temp, summ = F)) %>%
                 group_by(Chill_season, Year, Month, Day) %>%
                 summarise(Daily_portions = sum(chill))
  }
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

  rm(met_data, met_hourly, met_daily)
}

# How long did it take?
end_time <- Sys.time()

print( end_time - start_time)

