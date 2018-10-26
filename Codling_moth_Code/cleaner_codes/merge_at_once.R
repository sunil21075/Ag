#!/share/apps/R-3.2.2_gcc/bin/Rscript
#library(chron)
library(data.table)
library(dplyr)
library(foreach)
library(doParallel)


merge_at_once <- function(input_dir, 
	                      write_dir, 
	                      param_dir, 
	                      location_fname, 
	                      locationGroup_fname, 
	                      categories, 
	                      file_prefix, 
	                      version){
	data = data.table()
	conn = file(paste0(param_dir, "/", locations_list), open = "r")
	locations = readLines(conn)
	close(conn)

	for( category in categories){
		for( location in locations) {
			if(category != "historical") {
				filename <- paste0(input_dir, "/", category, "/rcp45/", file_prefix, location)
			}
			else {
				filename <- paste0(input_dir, "/", category, "/", file_prefix, location)
			}
		data <- rbind(data, read.table(filename, header = TRUE, sep = ","))
	    }
	}


	loc_grp = data.table(read.csv(paste0(param_dir, "LocationGroups.csv")))
	loc_grp$latitude = as.numeric(loc_grp$latitude)
	loc_grp$longitude = as.numeric(loc_grp$longitude)

	data$CountyGroup = 0L

	for(i in 1:nrow(loc_grp)) {
		data[latitude == loc_grp[i, latitude] & longitude == loc_grp[i, longitude], ]$CountyGroup = loc_grp[i, locationGroup]
	}

	return(data)
	}
