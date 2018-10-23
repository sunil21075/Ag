#!/share/apps/R-3.2.2_gcc/bin/Rscript
library(data.table)
library(reshape2)
library(dplyr)
library(foreach)
library(iterators)
library(chron)

source_path = "/home/hnoorazar/cleaner_codes/core.R"
source(source_path)

raw_data_dir = "/data/hydro/users/Hossein/codling_moth/raw_data/local_data/"
write_dir = "/data/hydro/users/Hossein/codling_moth/processed_data/local/historical_CMPOP/"
param_dir  = "/home/hnoorazar/cleaner_codes/parameters/"

file_prefix = "data_"
file_list = "local_list"
conn = file(paste0(param_dir, file_list), open = "r")
locations = readLines(conn)

ClimateGroup = list("Historical", "2040's", "2060's", "2080's")
cellByCounty = data.table(read.csv(paste0(param_dir, "CropParamCRB.csv")))

categories = c("historical")
for( category in categories) {
  for( location in locations) {
    filename = paste0(category, "/", file_prefix, location)
    if(category == "historical") {
      start_year = 1979
      end_year = 2015
    }
    else {
      start_year = 2006
      end_year = 2099
    }
    
    temp <- prepareData(filename, raw_data_dir, start_year, end_year)
    temp_data <- data.table()
    if(category == "historical") {
      temp$ClimateGroup[temp$year >= 1979 & temp$year <= 2006] <- "Historical"
      temp_data <- rbind(temp_data, temp[temp$year >= 1979 & temp$year <= 2006, ])
    }
    else {
      temp$ClimateGroup[temp$year > 2025 & temp$year <= 2055] <- "2040's"
      temp_data <- rbind(temp_data, temp[temp$year > 2025 & temp$year <= 2055, ])
      temp$ClimateGroup[temp$year > 2045 & temp$year <= 2075] <- "2060's"
      temp_data <- rbind(temp_data, temp[temp$year > 2045 & temp$year <= 2075, ])
      temp$ClimateGroup[temp$year > 2065 & temp$year <= 2095] <- "2080's"
      temp_data <- rbind(temp_data, temp[temp$year > 2065 & temp$year <= 2095, ])
    }
    loc = tstrsplit(location, "_")
    temp_data$latitude <- as.numeric(unlist(loc[1]))
    temp_data$longitude <- as.numeric(unlist(loc[2]))
    temp_data$County <- as.character(unique(cellByCounty[lat == temp_data$latitude[1] & long == temp_data$longitude[1], countyname]))
    # dir.create(file.path(write_dir), recursive = TRUE)
    write.table(temp_data, file = paste0(write_dir, "/CMPOP_", location), 
    	                     sep = ",", 
    	                     row.names = FALSE, 
    	                     col.names = TRUE)
  }
}
close(conn)

