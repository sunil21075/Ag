# Script for creating inputs for chill accumulation and threshold figures.
# Intended to work with array-modeled_data.sh script.
# Uses outputs from chilling model as inputs.

# 1. Load packages --------------------------------------------------------

.libPaths("/data/hydro/R_libs35")
.libPaths()
library(plyr)
library(lubridate)
library(purrr)
library(tidyverse)

source_path = "/home/hnoorazar/chilling_codes/2_second_draft/chill_core.R"
source(source_path)

# 2. Script setup ---------------------------------------------------------

# Check current folder
print("does this look right?")
getwd()
start_time <- Sys.time()
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
data_list_2040 <- vector(mode = "list", length = 295)
data_list_2060 <- vector(mode = "list", length = 295)
data_list_2080 <- vector(mode = "list", length = 295)

# Check whether historical data or not
hist <- basename(getwd()) == "historical"

# 5. Iterate through files and process ------------------------------------

# If historical data, then run a simpler data cleaning routine
if(hist){
  
  # 5a. Iterate through historical files ----------------------------------
  
  # For each data file
  for(i in 1:length(the_dir)){
    
    # Read in file
    file <- read.table(file = the_dir[i],
                       header = T,
                       colClasses = c("factor", "numeric", "numeric", "numeric",
                                      "numeric", "numeric"))
    
    names(data_list_historical)[i] <- the_dir[i]
    
    # Append it to a list following some processing
    data_list_historical[[i]] <-  threshold_func(file, data_type="modeled")
    rm(file)
  }

  # 5b. Process gathered historical data ------------------------------------
  # Get medians for each location during historical period
  summary_data_historical <- ldply(.data = data_list_historical,
                                   .fun = function(x) medians(thresh_20 = x[, "thresh_20"],
                                                              thresh_25 = x[, "thresh_25"],
                                                              thresh_30 = x[, "thresh_30"],
                                                              thresh_35 = x[, "thresh_35"],
                                                              thresh_40 = x[, "thresh_40"],
                                                              thresh_45 = x[, "thresh_45"],
                                                              thresh_50 = x[, "thresh_50"],
                                                              thresh_55 = x[, "thresh_55"],
                                                              thresh_60 = x[, "thresh_60"],
                                                              thresh_65 = x[, "thresh_65"],
                                                              thresh_70 = x[, "thresh_70"],
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
                               paste0("summary_",
                                      gsub("-", "_", basename(dirname(getwd()))), # model name
                                      "_",
                                      basename(getwd()), # scenario
                                      ".txt")),
              row.names = F)
  
  rm(data_historical)
  
  # Grab lat/long
  summary_data_historical <- grab_coord(summary_data_historical)
  
  summary_data_historical$model <- basename(dirname(getwd()))
  summary_data_historical$scenario <- basename(getwd())

  write.table(x = summary_data_historical,
              file = file.path(main_out,
                               paste0("summary_stats_",
                                      gsub("-", "_",basename(dirname(getwd()))), # model name
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
    data_list_2040[[i]] <- process_data(file, time_period="2040")
    
    names(data_list_2040)[i] <- the_dir[i]
    
    # 2060s
    data_list_2060[[i]] <- process_data(file, time_period="2060")
    names(data_list_2060)[i] <- the_dir[i]
    
    # 2080s
    data_list_2080[[i]] <- process_data(file, time_period="2080")
    names(data_list_2080)[i] <- the_dir[i]

    rm(file) 
  }
  
  # 5d. Process gathered future data ----------------------------------------
  
  # Apply this function to a list and spit out a dataframe
  summary_data_2040 <- get_medians(data_list_2040) 
  summary_data_2060 <- get_medians(data_list_2060)
  summary_data_2080 <- get_medians(data_list_2080)
  
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
                               paste0("summary_",
                                      gsub( "-", "_", basename(dirname(getwd()))), # model name
                                      "_",
                                      basename(getwd()), # scenario
                                      ".txt")),
              row.names = F)
  
  rm(all_years)
   
  # Grab lat/long
  summary_data_2040 <- grab_coord(summary_data_2040)
  summary_data_2060 <- grab_coord(summary_data_2060)
  summary_data_2080 <- grab_coord(summary_data_2080)
  
  head(summary_data_2040)
  head(summary_data_2060)
  head(summary_data_2080)
  
  # Combine dfs for plotting ease
  summary_data_2040 <- summary_data_2040 %>% mutate(year = 2040)
  summary_data_2060 <- summary_data_2060 %>% mutate(year = 2060)
  summary_data_2080 <- summary_data_2080 %>% mutate(year = 2080)
  
  summary_data_comb <- bind_rows(summary_data_2040,
                                 summary_data_2060,
                                 summary_data_2080)
  
  summary_data_comb$model <- basename(dirname(getwd()))
  summary_data_comb$scenario <- basename(getwd())
 
  write.table(x = summary_data_comb,
              file = file.path(main_out,
                               paste0("summary_stats_",
                                      gsub("-", "_", basename(dirname(getwd()))), # model name
                                      "_",
                                      basename(getwd()), # scenario
                                      ".txt")),
              row.names = F)   
}
# How long did it take?
end_time <- Sys.time()

print( end_time - start_time)


