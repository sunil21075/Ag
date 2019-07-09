###################################################################
#**********                            **********
#**********        WARNING !!!!        **********
#**********                            **********
##
## DO NOT load any libraries here.
## And do not load any libraries on the drivers!
## Unless you are aware of conflicts between packages.
## I spent hours to figrue out what the hell is going on!
###################################################################
.libPaths("/data/hydro/R_libs35")
.libPaths()
library(data.table)
library(tidyverse)
library(lubridate)

options(digit=9)
options(digits=9)

# Time the processing of this batch of files
start_time <- Sys.time()

######################################################################
##                                                                  ##
##                      Define all paths                            ##
##                                                                  ##
######################################################################
source_path = "/home/hnoorazar/lagoon_codes/core_lagoon"
source(source_path)

param_dir = file.path("/home/hnoorazar/lagoon_codes/parameters/")

lagoon_out = "/data/hydro/users/Hossein/lagoon/"
main_out <- file.path(lagoon_out, "/01/")
if (dir.exists(main_out) == F) {dir.create(path = main_out, recursive = T)}

######################################################################
##                                                                  ##
##              Terminal/shell/bash arguments                       ##
##                                                                  ##
######################################################################

args = commandArgs(trailingOnly=TRUE)
model_type = args[1]

chill_seasons = args[2]
chill_seasons = c(chill_seasons)

# 2. Pre-processing prep --------------------------------------------------

local_files <- read.table(file = paste0(param_dir, "file_list.txt"), header = F)
local_files <- as.vector(local_files$V1)

# 2b. Note if working with a directory of historical data
hist <- ifelse(grepl(pattern = "historical", x = getwd()) == T, TRUE, FALSE)

# Get current folder
current_dir <- gsub(x = getwd(),
                    pattern = "/data/hydro/jennylabcommon2/metdata/maca_v2_vic_binary/",
                    replacement = "")

# 2d. Prep list of files for processing
# get files in current folder
dir_con <- dir()

dir_con <- dir_con[grep(pattern = "data_", x = dir_con)]

# filter locations of interest
dir_con <- dir_con[which(dir_con %in% local_files)] 

# 3. Process the data -----------------------------------------------------
for(file in dir_con){
  # 3a. read in binary meteorological data file from specified path
  met_data <- read_binary(file_path = file, hist = hist, no_vars=4)
  lat <- as.numeric(substr(x = file, start = 6, stop = 13))
  
  met_data <- as.data.frame(met_data) # data frame required

  # 3b. Clean it up
  # rename needed columns
  met_data <- met_data %>%
              select(-c(precip, windspeed)) %>%
              data.frame()

  setnames(met_data, old=c("year","month", "day", "tmax", "tmin"), 
                     new=c("Year", "Month", "Day", "Tmax", "Tmin"))
 
  # 3c. Get hourly interpolation
  # generate hourly data
  met_hourly <- stack_hourly_temps(weather = met_data,
                                   latitude = lat)
  rm(met_data)
  # save only the necessary list item
  met_hourly <- met_hourly[[1]]

  setnames(met_hourly, new=c("year","month", "day", "tmax", "tmin"), 
                       old=c("Year", "Month", "Day", "Tmax", "Tmin"))
  # 3d. Run the chill accumulation model and sum up by day
  # we want this on a seasonal basis specific to chill

  # chill_seasons = c("mid_sept", "oct", "mid_oct", "nov", "mid_nov") # "sept",
  for (chill_s in chill_seasons){
    current_out <- file.path(main_out, chill_s, "modeled", current_dir)
    if (dir.exists(current_out) == F) {
      dir.create(path = current_out, recursive = T)
    }
    met_hourly <- put_chill_season(met_hourly_dt=met_hourly, chill_start=chill_s)
    
    # sum within a day using NON-cumulative chill portions
    if (model_type == "dynamic"){
      met_daily <- met_hourly %>%
                   group_by(chill_season) %>% # should maintain correct day, time order
                   mutate(chill = Dynamic_Model(HourTemp = Temp, summ = F)) %>%
                   group_by(chill_season, year, month, day) %>%
                   summarise(daily_portions = sum(chill))
     } else if (model_type == "utah"){
      met_daily <- met_hourly %>%
                   group_by(chill_season) %>% # should maintain correct day, time order
                   mutate(chill = Utah_Model(HourTemp = Temp, summ = F)) %>%
                   group_by(chill_season, year, month, day) %>%
                   summarise(daily_portions = sum(chill))
    }
    met_daily <- met_daily %>%
                group_by(chill_season) %>%
                mutate(cume_portions = cumsum(daily_portions))
    
    # 3e. Save output
    output_name <- paste0("chill_output_", file, ".txt")
    write.table(x = met_daily,
                file = file.path(current_out, output_name),
                row.names = F)
    rm(met_daily)
  }
  rm(met_hourly)
}

# How long did it take?
end_time <- Sys.time()
print( end_time - start_time)

