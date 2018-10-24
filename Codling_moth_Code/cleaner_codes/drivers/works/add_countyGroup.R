#!/share/apps/R-3.2.2_gcc/bin/Rscript
library(data.table)
library(reshape2)
library(dplyr)
library(foreach)
library(iterators)
library(chron)

input_dir = "/data/hydro/users/Hossein/codling_moth/local/processed/"
param_dir = "/home/hnoorazar/cleaner_codes/parameters/"

loc_grp = data.table(read.csv(paste0(param_dir, "LocationGroups.csv")))
loc_grp$latitude = as.numeric(loc_grp$latitude)
loc_grp$longitude = as.numeric(loc_grp$longitude)


filename <- paste0(input_dir, "combined_CMPOP_rcp45.rds")
combined_CMPOP_rcp45 <- data.table(readRDS(filename))
#loc = tstrsplit(data$location, "_")
#combined_CMPOP_rcp45$latitude <- as.numeric(unlist(loc[1]))
#combined_CMPOP_rcp45$longitude <- as.numeric(unlist(loc[2]))


filename <- paste0(input_dir, "combined_CM_rcp45.rds")
combined_CM_rcp45 <- data.table(readRDS(filename))

filename <- paste0(input_dir, "combined_CM_rcp85.rds")
combined_CM_rcp85 <- data.table(readRDS(filename))


combined_CMPOP_rcp45$CountyGroup = 0L
combined_CM_rcp45$CountyGroup = 0L

for(i in 1:nrow(loc_grp)) {
  combined_CMPOP_rcp45[latitude == loc_grp[i, latitude] & longitude == loc_grp[i, longitude], ]$CountyGroup = loc_grp[i, locationGroup]
  #combined_CM_rcp45[latitude == loc_grp[i, latitude] & longitude == loc_grp[i, longitude], ]$CountyGroup = loc_grp[i, locationGroup]
  #combined_CM_rcp85[latitude == loc_grp[i, latitude] & longitude == loc_grp[i, longitude], ]$CountyGroup = loc_grp[i, locationGroup]
}

saveRDS(combined_CMPOP_rcp45, paste0(input_dir, "/", "combined_CMPOP_rcp45_countyG.rds"))
#saveRDS(combined_CM_rcp45, paste0(input_dir, "/", "combined_CM_rcp45_countyG.rds"))
#saveRDS(combined_CM_rcp85, paste0(input_dir, "/", "combined_CM_rcp85_countyG.rds"))
