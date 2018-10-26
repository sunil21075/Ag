
add_countyGroup <- function(input_dir, rds_file_name, param_dir, group_locations_name="LocationGroups.csv" ){
	filename <- paste0(input_dir, file_name)
	data = data.table(readRDS(filename))

	loc_grp = data.table(read.csv(paste0(param_dir, group_locations_name )))
    loc_grp$latitude = as.numeric(loc_grp$latitude)
    loc_grp$longitude = as.numeric(loc_grp$longitude)

    combined_CMPOP_rcp45$CountyGroup = 0L
    for(i in 1:nrow(loc_grp)) {
    	data[latitude == loc_grp[i, latitude] & longitude == loc_grp[i, longitude], ]$CountyGroup = loc_grp[i, locationGroup]
    }
    return(data)
}

add_climateScenario <- function(input_dir, categories, ){
	data = data.table()
	for(category in categories) {
		if(category != "historical") {
			filename <- paste0(data_dir, "/", category, "Data_rcp45.rds")
		}
		else {
			filename <- paste0(data_dir, "/", category, "Data.rds")
		}
	temp <- data.table(readRDS(filename))
	temp$ClimateScenario <- category
	data <- rbind(data, temp)
	}
}

data_dir = "/data/hydro/users/giridhar/giridhar/codmoth_pop"
categories = c("historical", "BNU-ESM", "CanESM2", "GFDL-ESM2G", "bcc-csm1-1-m", "CNRM-CM5", "GFDL-ESM2M")
data = data.table()



saveRDS(data, paste0(data_dir, "/", "allData_rcp45.rds"))






