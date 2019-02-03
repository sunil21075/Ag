# Script for creating inputs for chill accumulation and threshold figures.
# Intended to work with array-modeled_data.sh script.
# Uses outputs from chilling model as inputs.


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
data_list_2040 <- vector(mode = "list", length = 295)
data_list_2060 <- vector(mode = "list", length = 295)
data_list_2080 <- vector(mode = "list", length = 295)

# Check whether historical data or not
hist <- basename(getwd()) == "historical"


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

# If historical data, then run a simpler data cleaning routine
if(hist){
  
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
      filter(Chill_season != "chill_1949-1950" &
               Chill_season != "chill_2005-2006") %>% 
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
  data_historical$model <- basename(dirname(getwd()))
  data_historical$scenario <- basename(getwd())
  data_historical$lat = as.numeric(substr(x = data_historical$.id,
                                          start = 19, stop = 26))
  data_historical$long = as.numeric(substr(x = data_historical$.id,
                                           start = 28, stop = 37))
  data_historical <- unique(data_historical)
  
  head(data_historical)
  
  # No longer needed
  rm(data_list_historical)
  
  # .id row contains originating filename of this data
  write.table(x = data_historical,
              file = file.path(main_out,
                               paste0("chill-data-summary-",
                                      basename(dirname(getwd())), # model name
                                      "_",
                                      basename(getwd()), # scenario
                                      ".txt")),
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
  
  summary_data_historical$model <- basename(dirname(getwd()))
  summary_data_historical$scenario <- basename(getwd())

  write.table(x = summary_data_historical,
              file = file.path(main_out,
                               paste0("chill-data-summary-stats-",
                                      basename(dirname(getwd())), # model name
                                      "_",
                                      basename(getwd()), # scenario
                                      ".txt")),
              row.names = F)
  
  
  
  # If future data, then proceed with decadal calculations:
  
  # 5c. Iterate through future files ----------------------------------------
  
}else{
  
  for(i in 1:length(the_dir)){
    
    file <- read.table(file = the_dir[i],
                       header = T,
                       colClasses = c("factor", "numeric", "numeric", "numeric",
                                      "numeric", "numeric"))
    
    # The breakpoints for decades came from 
    # https://github.com/HNoorazar/Kirti/blob/master/codling_moth/code/core.R
    # on 2018-12-05
    
    # 2040s
    data_list_2040[[i]] <-  file %>%
      filter(Year > 2025 & Year <= 2055,
             Chill_season != "chill_2025-2026" &
               Chill_season != "chill_2055-2056") %>% 
      group_by(Chill_season) %>%
      # Mutate output is the row index of the first time where it meets threshold
      # within the group. (Index is the same as counting the start date as day = 1)
      mutate(thresh_50 = detect_index(.x = Cume_portions,
                                      .f = chill_thresh,
                                      threshold = 50),
             thresh_75 = detect_index(.x = Cume_portions,
                                      .f = chill_thresh,
                                      threshold = 75),
             # Below we set the row where, ex. month = 1 (jan) and day = 1 to be
             # equal to the value of Cume_portions. This creates one non-NA row
             # for the whole column. Then in summarise we remove all NAs, which
             # collapses the column to a single value -- perfect for a one-line
             # summary per season.
             sum_J1 =  case_when(
               Month == 1 & Day == 1 ~ Cume_portions),
             sum_F1 = case_when(
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
    
    names(data_list_2040)[i] <- the_dir[i]
    
    # 2060s
    data_list_2060[[i]] <-  file %>%
      filter(Year > 2045 & Year <= 2075,
             Chill_season != "chill_2045-2046" &
               Chill_season != "chill_2075-2076") %>% 
      group_by(Chill_season) %>%
      mutate(thresh_50 = detect_index(.x = Cume_portions,
                                      .f = chill_thresh,
                                      threshold = 50),
             thresh_75 = detect_index(.x = Cume_portions,
                                      .f = chill_thresh,
                                      threshold = 75),
             sum_J1 =  case_when(
               Month == 1 & Day == 1 ~ Cume_portions),
             sum_F1 = case_when(
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
    
    names(data_list_2060)[i] <- the_dir[i]
    
    
    # 2080s
    data_list_2080[[i]] <-  file %>%
      filter(Year > 2065 & Year <= 2095,
             Chill_season != "chill_2065-2066" &
               Chill_season != "chill_2095-2096") %>% 
      group_by(Chill_season) %>%
      mutate(thresh_50 = detect_index(.x = Cume_portions,
                                      .f = chill_thresh,
                                      threshold = 50),
             thresh_75 = detect_index(.x = Cume_portions,
                                      .f = chill_thresh,
                                      threshold = 75),
             sum_J1 =  case_when(
               Month == 1 & Day == 1 ~ Cume_portions),
             sum_F1 = case_when(
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
    
    names(data_list_2080)[i] <- the_dir[i]
    
    
    rm(file)
    
  }
  
  
  # 5d. Process gathered future data ----------------------------------------
  
  
  # Apply this function to a list and spit out a dataframe
  summary_data_2040 <- ldply(.data = data_list_2040,
                             .fun = function(x) medians(thresh_50 = x[, "thresh_50"],
                                                        thresh_75 = x[, "thresh_75"],
                                                        sum_J1 = x[, "sum_J1"],
                                                        sum_F1 = x[, "sum_F1"],
                                                        sum_M1 = x[, "sum_M1"],
                                                        sum_A1 = x[, "sum_A1"]))
  summary_data_2060 <- ldply(.data = data_list_2060,
                             .fun = function(x) medians(thresh_50 = x[, "thresh_50"],
                                                        thresh_75 = x[, "thresh_75"],
                                                        sum_J1 = x[, "sum_J1"],
                                                        sum_F1 = x[, "sum_F1"],
                                                        sum_M1 = x[, "sum_M1"],
                                                        sum_A1 = x[, "sum_A1"]))
  summary_data_2080 <- ldply(.data = data_list_2080,
                             .fun = function(x) medians(thresh_50 = x[, "thresh_50"],
                                                        thresh_75 = x[, "thresh_75"],
                                                        sum_J1 = x[, "sum_J1"],
                                                        sum_F1 = x[, "sum_F1"],
                                                        sum_M1 = x[, "sum_M1"],
                                                        sum_A1 = x[, "sum_A1"]))
  
  
  head(summary_data_2040)
  head(summary_data_2060)
  head(summary_data_2080)
  
  # Briefly want to export the raw data from the lists for use in other figs
  data2040 <- ldply(data_list_2040, function(x) data.frame(x))
  data2060 <- ldply(data_list_2060, function(x) data.frame(x))
  data2080 <- ldply(data_list_2080, function(x) data.frame(x))
  
  all_years <- bind_rows(data2040, data2060, data2060)
  
  head(all_years)
  
  all_years$year <- as.numeric(substr(x = all_years$Chill_season,
                                      start = 12, stop = 15))
  all_years$model <- basename(dirname(getwd()))
  all_years$scenario <- basename(getwd())
  all_years$lat = as.numeric(substr(x = all_years$.id, start = 19, stop = 26))
  all_years$long = as.numeric(substr(x = all_years$.id, start = 28, stop = 37))
  all_years <- unique(all_years)
  
  head(all_years)
  
  # No longer needed
  rm(list = c("data_list_2040", "data_list_2060", "data_list_2080"))
  
  
  # .id row contains originating filename of this data
  write.table(x = all_years,
              file = file.path(main_out,
                               paste0("chill-data-summary-",
                                      basename(dirname(getwd())), # model name
                                      "_",
                                      basename(getwd()), # scenario
                                      ".txt")),
              row.names = F)
  
  rm(all_years)
  
  
  # Grab lat/long
  summary_data_2040 <- summary_data_2040 %>%
    transmute(lat = as.numeric(substr(x = .id, start = 19, stop = 26)),
              long = as.numeric(substr(x = .id, start = 28, stop = 37)),
              median_50 = median_50,
              median_75 = median_75,
              median_J1 = median_J1,
              median_F1 = median_F1,
              median_M1 = median_M1,
              median_A1 = median_A1)
  
  summary_data_2060 <- summary_data_2060 %>%
    transmute(lat = as.numeric(substr(x = .id, start = 19, stop = 26)),
              long = as.numeric(substr(x = .id, start = 28, stop = 37)),
              median_50 = median_50,
              median_75 = median_75,
              median_J1 = median_J1,
              median_F1 = median_F1,
              median_M1 = median_M1,
              median_A1 = median_A1)
  
  summary_data_2080 <- summary_data_2080 %>%
    transmute(lat = as.numeric(substr(x = .id, start = 19, stop = 26)),
              long = as.numeric(substr(x = .id, start = 28, stop = 37)),
              median_50 = median_50,
              median_75 = median_75,
              median_J1 = median_J1,
              median_F1 = median_F1,
              median_M1 = median_M1,
              median_A1 = median_A1)
  
  head(summary_data_2040)
  head(summary_data_2060)
  head(summary_data_2080)
  
  # Combine dfs for plotting ease
  summary_data_2040 <- summary_data_2040 %>%
    mutate(year = 2040)
  
  summary_data_2060 <- summary_data_2060 %>%
    mutate(year = 2060)
  
  summary_data_2080 <- summary_data_2080 %>%
    mutate(year = 2080)
  
  summary_data_comb <- bind_rows(summary_data_2040,
                                 summary_data_2060,
                                 summary_data_2080)
  
  summary_data_comb$model <- basename(dirname(getwd()))
  summary_data_comb$scenario <- basename(getwd())
 
  write.table(x = summary_data_comb,
              file = file.path(main_out,
                               paste0("chill-data-summary-stats-",
                                      basename(dirname(getwd())), # model name
                                      "_",
                                      basename(getwd()), # scenario
                                      ".txt")),
              row.names = F)   
}

