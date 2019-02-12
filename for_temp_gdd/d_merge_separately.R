######################################################################
##         Merging all files are too big. 
##         I will merge each model, then get their statistics, 
##         then combine those 

##         This function will merge, COMPUTE statistics, and 
##         Write just the STATISTICS to the disk.
##
######################################################################

.libPaths("/data/hydro/R_libs35")
.libPaths()
library(data.table)
library(dplyr)
library(lubridate)
library(tidyverse)

args = commandArgs(trailingOnly=TRUE)
data_type = args[1]

add_countyGroup <- function(data){
	options(digits=9)
	param_dir = "/home/hnoorazar/cleaner_codes/parameters/"
	loc_group_file_name = "LocationGroups.csv"
	locations_list = "local_list"
	loc_grp = data.table(read.csv(paste0(param_dir, loc_group_file_name)))
	loc_grp$latitude = as.numeric(loc_grp$latitude)
	loc_grp$longitude = as.numeric(loc_grp$longitude)

	data$CountyGroup = 0L
	for(i in 1:nrow(loc_grp)) {
		data[latitude == loc_grp[i, latitude] & longitude == loc_grp[i, longitude], ]$CountyGroup = loc_grp[i, locationGroup]
	}
	return (data)
}

clean <- function(data, scenario){
	needed_colomns = c("year", "tmean", "Cum_dd", 
		               "ClimateGroup", "ClimateScenario",
		               "CountyGroup")
	# grab needed cols
	data = subset(data, select=needed_colomns)
    print (colnames(data))
    # drop 2006-2024 years
    data = filter(data, year <=2005 | year >= 2025)
    print (min(data$year))
    print (max(data$year))

    data$ClimateGroup[data$year >= 1979 & data$year <= 2005] <- "Historical"
	data$ClimateGroup[data$year > 2025 & data$year <= 2055] <- "2040's"
	data$ClimateGroup[data$year > 2045 & data$year <= 2075] <- "2060's"
	data$ClimateGroup[data$year > 2065 & data$year <= 2095] <- "2080's"
    
    # rename col names
    colnames(data)[colnames(data) == 'ClimateScenario'] <- 'model'
    data$scenario = scenario
     
    # drop the year columnn
    data = data.table(data)
    return (data)
}

merge_a_model <- function(data_type, input_dir, output_dir, modeled_categories){
	for (cat in modeled_categories){
		data = data.table()
		data_dir = file.path(input_dir, cat, "/", data_type, "/")
		print (paste0("The model is of type ", cat))
		print ("---------------------------------------------------")
		
		# list of .rds files in the directory
		all_files <- list.files(data_dir)
		all_files <- all_files[grep(pattern = "data_", x = all_files)]
		
		for (afile in all_files){
			curr_data <- data.table(readRDS(paste0(data_dir, afile)))
			data <- rbind(data, curr_data)
		}
	print ("line 66 of merge_a_model(), before adding CountyGroup ")
	data <- add_countyGroup(data=data)
	data <- clean(data=data, scenario=data_type)
	print ("---------------------------------------------------")
	print ("line 69 of merge_a_model, before computing stats!")
	print (colnames(data))
	data <- data[, list(mean_tmean = mean(tmean), 
                        mean_cumm_dd = mean(Cum_dd)) , 
                        by = c("ClimateGroup", "model", 
                               "scenario", "CountyGroup")]
    out_dir= paste0(output_dir, cat, "/")
    print ("after computing stats")
    print (colnames(data))

	saveRDS(data, paste0(out_dir, data_type,"_stat.rds"))
	rm(data)
	}
}

merge <- function(data_type){
	# options(digits=9)
	# param_dir = "/home/hnoorazar/cleaner_codes/parameters/"
	# loc_group_file_name = "LocationGroups.csv"
	# locations_list = "local_list"
	# loc_grp = data.table(read.csv(paste0(param_dir, loc_group_file_name)))
	# loc_grp$latitude = as.numeric(loc_grp$latitude)
	# loc_grp$longitude = as.numeric(loc_grp$longitude)

	input_dir = "/data/hydro/users/Hossein/temp_gdd/modeled/"
	output_dir= "/data/hydro/users/Hossein/temp_gdd/modeled/"

	# list of directories in input_dir
	categories = list.dirs(path = input_dir, full.names = F, recursive = F)
	versions = c("historical", "rcp45", "rcp85")
	merge_a_model(data_type=data_type, 
		          input_dir=input_dir, output_dir=output_dir, 
		          modeled_categories=categories)
}

merge(data_type)

