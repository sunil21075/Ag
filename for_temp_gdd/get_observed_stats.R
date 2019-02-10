
input_dir = "/Users/hn/Documents/GitHub/Kirti/for_temp_gdd/"

observed = data.table(readRDS(paste0(input_dir, "observed.rds")))
options(digits=9)

param_dir = "/Users/hn/Documents/GitHub/Kirti/codling_moth/code/parameters/"
loc_group_file_name = "LocationGroups.csv"
locations_list = "local_list"
loc_grp = data.table(read.csv(paste0(param_dir, loc_group_file_name)))
loc_grp$latitude = as.numeric(loc_grp$latitude)
loc_grp$longitude = as.numeric(loc_grp$longitude)

for(i in 1:nrow(loc_grp)) {
	observed[latitude == loc_grp[i, latitude] & longitude == loc_grp[i, longitude], ]$CountyGroup = loc_grp[i, locationGroup]
}