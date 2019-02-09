# Script for creating inputs for chill accumulation and threshold figures for
# historical observed data. Summarizes the observed historical chill unit
# data into format used for figures and maps.
# Intended to work with observed-historical_data.sh script.

# When working locally have been using this folder: setwd("~/chilling-model/test-data/historical/observed-hist-outputs/historical/UI_historical/VIC_Binary_CONUS_to_2016")
# with local main_out <- "~/chilling-model/test-data/chill-figs/"

# 1. Load packages --------------------------------------------------------
.libPaths("/data/hydro/R_libs35")
.libPaths()
library(plyr)
library(lubridate)
library(purrr)
library(tidyverse)

source_path = "/data/hydro/users/Hossein/chill/data_by_core/11_threshold/chill_core.R"
source(source_path)
# 2. Script setup ---------------------------------------------------------

# Check current folder
print("does this look right?")
getwd()

# Set an output location for this script's outputs
main_out <- file.path("/data/hydro/users/Hossein/chill/data_by_core/11_threshold/02/")

# Create a figures-specific output pathway if it doesn't exist
if (dir.exists(file.path(main_out)) == F) {
  dir.create(path = main_out, recursive = T)
}

# 3. Some set up ----------------------------------------------------------

# List of filenames
the_dir <- dir()

# Remove filenames that aren't data, if they exist
the_dir <- the_dir[grep(pattern = "chill_output_data",
                        x = the_dir)]

# Pre-allocate lists to be used
data_list_historical <- vector(mode = "list", length = 295)

# 5. Iterate through files and process ------------------------------------
  
  # 5a. Iterate through historical files ------------------------------------
  
  # For each data file
  for(i in 1:length(the_dir)){
    
    # Read in file
    file <- read.table(file = the_dir[i],
                       header = T,
                       colClasses = c("factor", "numeric", "numeric", "numeric",
                                      "numeric", "numeric"))
    
    names(data_list_historical)[i] <- the_dir[i]
    
    # Append it to a list following some processing
    data_list_historical[[i]] <-  threshold_func(file, data_type="historical")
    rm(file)
  }
  
  # 5b. Process gathered historical data ------------------------------------
  
  # Get medians for each location during historical period
  summary_data_historical <- get_medians(data_list_historical)
  
  head(summary_data_historical)
  
  # Briefly want to export the raw data from the lists for use in other figs
  data_historical <- ldply(data_list_historical, function(x) data.frame(x))
  
  data_historical$year <- as.numeric(substr(x = data_historical$Chill_season,
                                            start = 12, stop = 15))
  data_historical$model <- "observed"
  data_historical$scenario <- "historical"
  data_historical$lat = as.numeric(substr(x = data_historical$.id,
                                          start = 19, stop = 26))
  data_historical$long = as.numeric(substr(x = data_historical$.id,
                                           start = 28, stop = 37))
  data_historical <- unique(data_historical)
  
  head(data_historical)
  
  # Remove what's no longer needed
  rm(data_list_historical)
  
  # .id row contains originating filename of this data
  write.table(x = data_historical,
              file = file.path(main_out,
                               "chill-data-summary-obs_hist.txt"),
              row.names = F)
  
  rm(data_historical)
  
  # Grab lat/long
  summary_data_historical <- grab_coord(summary_data_historical)
  
  summary_data_historical$model <- "observed"
  summary_data_historical$scenario <- "historical"
  
  write.table(x = summary_data_historical,
              file = file.path(main_out,
                               "chill-data-summary-stats-obs_hist.txt"),
              row.names = F)
  
