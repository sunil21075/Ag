# This generates the folder data_processed
#
#
#


#!/share/apps/R-3.2.2_gcc/bin/Rscript
library(chron)
library(data.table)
library(reshape2)
library(dplyr)
library(foreach)
library(iterators)

source_path = "/home/hnoorazar/cleaner_codes/core.R"
source(source_path)

write_path = "/home/hnoorazar/data/"

#data_dir = "/home/kiran/giridhar/codmoth_pop/"
data_dir = "/data/hydro/users/giridhar/giridhar/codmoth_pop/alldata_us_locations/"
categories = c("historical", "BNU-ESM", "CanESM2", "GFDL-ESM2G", "bcc-csm1-1-m", "CNRM-CM5", "GFDL-ESM2M")
#categories = c("CanESM2", "GFDL-ESM2G", "bcc-csm1-1-m", "CNRM-CM5", "GFDL-ESM2M", "historical", "BNU-ESM")
#categories = c("CanESM2", "GFDL-ESM2G", "bcc-csm1-1-m", "CNRM-CM5", "GFDL-ESM2M", "BNU-ESM")
file_prefix = "data_"
#file_list = "list"
file_list = "all_us_locations_list"
ClimateGroup = list("Historical", "2040's", "2060's", "2080's")
cellByCounty = data.table(read.csv(paste0(data_dir, "CropParamCRB.csv")))
#data = data.table()

conn = file(paste0(data_dir, file_list), open = "r")
locations = readLines(conn)

args = commandArgs(trailingOnly=TRUE)
category = args[1]
#for( category in categories) {
#version = args[2]
#for(version in c('rcp45', 'rcp85')) {
  #files = list.files(paste0(data_dir, "data/", category, "/", version, "/"))
  for(location in locations) {
  #for( file in files) {
    #location = gsub("data_", "", file)
    
    if(category == "historical") {
      start_year = 1979
      end_year = 2015
      filename = paste0("data/", category, "/", file_prefix, location)
    }
    else {
      start_year = 2006
      end_year = 2099
      filename = paste0("data/", category, "/", version, "/", file_prefix, location)
    }
    
    #temp <- prepareData(filename, data_dir, start_year, end_year)
    temp <- prepareData_1(filename, data_dir, start_year, end_year)
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
    # must add state name/id and county name/id
    #temp_data$County <- as.character(unique(cellByCounty[lat == temp_data$latitude[1] & long == temp_data$longitude[1], countyname]))
    if(category != "historical") {
      #write.table(temp_data, file = paste0(data_dir, category, "/rcp45/CMPOP_", location), sep = ",", row.names = FALSE, col.names = TRUE)
      write_dir = paste0(write_path, "data_processed/", category, "/", version)
      dir.create(file.path(write_dir), recursive = TRUE)
      write.table(temp_data, file = paste0(write_dir, "/CM_", location), sep = ",", row.names = FALSE, col.names = TRUE)
    }
    else {
      write_dir = paste0(write_path, "data_processed/", category, "/")
      dir.create(file.path(write_dir), recursive = TRUE)
      write.table(temp_data, file = paste0(write_dir, "/CM_", location), sep = ",", row.names = FALSE, col.names = TRUE)
    }
    #data <- rbind(data, temp_data)
  }
#}
close(conn)
#data$ClimateGroup <- as.factor(data$ClimateGroup)
#data$County <- as.factor(data$County)

