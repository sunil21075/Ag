# Script for creating inputs for chill accumulation and threshold figures for
# historical observed data. Summarizes the observed historical chill unit
# data into format used for figures and maps.
# Intended to work with observed-historical_data.sh script.

# When working locally have been using this folder: 
# setwd("~/chilling-model/test-data/historical/observed-hist-outputs/historical/UI_historical/VIC_Binary_CONUS_to_2016")
# with local main_out <- "~/chilling-model/test-data/chill-figs/"

# 1. Load packages --------------------------------------------------------

library(plyr)
library(lubridate)
library(purrr)
library(tidyverse)

 
# 2. Script setup ---------------------------------------------------------

# Check current folder
print("does this look right?")
getwd()

# Set an output location for this script's outputs
main_out <- file.path("/fastscratch",
                      "mbrousil",
                      "chill-figs/")

# Create a figures-specific output pathway if it doesn't exist
if (dir.exists(file.path(main_out)) == F) {dir.create(path = main_out, recursive = T)}

# 3. Some set up ----------------------------------------------------------

# List of filenames
the_dir <- dir()

# Remove filenames that aren't data, if they exist
the_dir <- the_dir[grep(pattern = "chill_output_data",
                        x = the_dir)]
# Pre-allocate lists to be used
data_list_historical <- vector(mode = "list", length = 295)

# 4. Set up functions -----------------------------------------------------

# A function to check against a threshold
chill_thresh <- function(x, threshold) {x >= threshold}

# A function that pulls the median values we want
medians <- function(thresh_50, thresh_75, sum_J1, sum_F1, sum_M1, sum_A1) {
  c(median_50 = median(thresh_50),
    median_75 = median(thresh_75),
    median_J1 = median(sum_J1),
    median_F1 = median(sum_F1),
    median_M1 = median(sum_M1),
    median_A1 = median(sum_A1))
}

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
    data_list_historical[[i]] <-  file %>%
      # Only want complete seasons of data
      filter(Chill_season != "chill_1978-1979" &
               Chill_season != "chill_2015-2016") %>% 
      # Within a season
      group_by(Chill_season) %>%
      # Mutate output is the row index of the first time where it meets threshold
      # within the group. (Index is the same as counting the start date as day = 1)
      mutate(thresh_50 = detect_index(.x = Cume_portions,
                                      .f = chill_thresh,
                                      threshold = 50), # threshold = 50 chill portions
             thresh_75 = detect_index(.x = Cume_portions,
                                      .f = chill_thresh,
                                      threshold = 75),
             # Below we set the row where, ex. month = 1 (jan) and day = 1 to be
             # equal to the value of Cume_portions. This creates one non-NA row
             # for the whole column. Then in summarise we remove all NAs, which
             # collapses the column to a single value -- perfect for a one-line
             # summary per season.
             sum_J1 =  case_when( # January 1 chill sum
               Month == 1 & Day == 1 ~ Cume_portions),
             sum_F1 = case_when( # February 1 chill sum, etc
               Month == 2 & Day == 1 ~ Cume_portions),
             sum_M1 = case_when(
               Month == 3 & Day == 1 ~ Cume_portions),
             sum_A1 = case_when(
               Month == 4 & Day == 1 ~ Cume_portions)) %>% 
      summarise(sum = sum(Daily_portions),
                thresh_50 = unique(thresh_50), # retain the thresholds
                thresh_75 = unique(thresh_75),
                sum_J1 = na.omit(sum_J1),
                sum_F1 = na.omit(sum_F1),
                sum_M1 = na.omit(sum_M1),
                sum_A1 = na.omit(sum_A1)) %>%
      data.frame() # to allow for ldply() later
    
    rm(file)
    
  }
  
  # 5b. Process gathered historical data ------------------------------------

  # Get medians for each location during historical period
  summary_data_historical <- ldply(.data = data_list_historical,
                                   .fun = function(x) medians(thresh_50 = x[, "thresh_50"],
                                                              thresh_75 = x[, "thresh_75"],
                                                              sum_J1 = x[, "sum_J1"],
                                                              sum_F1 = x[, "sum_F1"],
                                                              sum_M1 = x[, "sum_M1"],
                                                              sum_A1 = x[, "sum_A1"]))
  
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
  summary_data_historical <- summary_data_historical %>%
    transmute(lat = as.numeric(substr(x = .id, start = 19, stop = 26)),
              long = as.numeric(substr(x = .id, start = 28, stop = 37)),
              median_50 = median_50,
              median_75 = median_75,
              median_J1 = median_J1,
              median_F1 = median_F1,
              median_M1 = median_M1,
              median_A1 = median_A1)
  
  summary_data_historical$model <- "observed"
  summary_data_historical$scenario <- "historical"
  
  write.table(x = summary_data_historical,
              file = file.path(main_out,
                               "chill-data-summary-stats-obs_hist.txt"),
              row.names = F)
  
