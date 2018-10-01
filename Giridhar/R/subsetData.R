#!/share/apps/R-3.2.2_gcc/bin/Rscript
#library(chron)
library(data.table)

#data_dir = getwd()
data_dir = "/data/hydro/users/giridhar/giridhar/codmoth_pop"
#filename <- paste0(data_dir, "/allData_revised.rds")
#filename <- paste0(data_dir, "/allData.rds")
filename <- paste0(data_dir, "/allData_rcp45.rds")
filename <- paste0(data_dir, "/allData_grouped_counties_rcp45.rds")

data <- data.table(readRDS(filename))

#saveRDS(p, paste0(data_dir, "/", "popplot.rds"))


#data = data[, .SD[.N], by=c("ClimateGroup", "ClimateScenario", "latitude", "longitude", "County", "year", "month")]
#saveRDS(data, paste0(data_dir, "/", "subData_rcp45.rds"))

#data = subset(data, select = c("ClimateGroup", "ClimateScenario", "latitude", "longitude", "County", "year", "month", "DailyDD"))
#data$MonthGroup = 0
#data[month == 1 | month == 2 | month == 3, MonthGroup := 3]
#data[month == 4 | month == 5 | month == 6, MonthGroup := 6]
#data[month == 7 | month == 8 | month == 9, MonthGroup := 9]
#data[month == 10 | month == 11 | month == 12, MonthGroup := 12]
#data = data[, .(monthlyDD = sum(DailyDD)), 
#             by = c("ClimateGroup", "ClimateScenario", "latitude", "longitude", "County", "year", "MonthGroup")]
#data = data[, .(monthlyDD = sum(DailyDD)), 
#             by = c("ClimateGroup", "ClimateScenario", "latitude", "longitude", "County", "year", "month")]
#saveRDS(data, paste0(data_dir, "/", "subDDData_rcp45.rds"))
#saveRDS(data, paste0(data_dir, "/", "subDDData_month_groups.rds"))
#saveRDS(data, paste0(data_dir, "/", "subDDData_month_groups_rcp45.rds"))

data = data[latitude == 46.34375 & longitude == -119.21875]
#data = data[latitude == 48.96875 & longitude == -119.46875]
saveRDS(data, paste0(data_dir, "/", "allData_one_location_rcp45.rds"))
