#!/share/apps/R-3.2.2_gcc/bin/Rscript
#library(chron)
library(data.table)

#data_dir = getwd()
data_dir = "/data/hydro/users/giridhar/giridhar/codmoth_pop"
#filename <- paste0(data_dir, "/allData.rds")
filename <- paste0(data_dir, "/allData_rcp45.rds")
data <- data.table(readRDS(filename))


loc_grp = data.table(read.csv("LocationGroups2.csv"))
loc_grp$latitude = as.numeric(loc_grp$latitude)
loc_grp$longitude = as.numeric(loc_grp$longitude)

data$CountyGroup = 0L

for(i in 1:nrow(loc_grp)) {
	data[latitude == loc_grp[i, latitude] & longitude == loc_grp[i, longitude], ]$CountyGroup = loc_grp[i, locationGroup]
}

saveRDS(data, paste0(data_dir, "/", "allData_grouped_counties_rcp45.rds"))
