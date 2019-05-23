
.libPaths("/data/hydro/R_libs35")
.libPaths()
library(tidyverse)
library(lubridate)
library(dplyr)
library(data.table)

options(digit=9)
options(digits=9)

st_time <- Sys.time()

data_dir <- "/data/hydro/users/Hossein/analog/usa/data_bases/before_biofix/"
combined_CMPOP <- data.table(readRDS(paste0(data_dir, "combined_CMPOP.rds")))
combined_CMPOP$location <- paste0(combined_CMPOP$latitude, "_", combined_CMPOP$longitude)

saveRDS(combined_CMPOP, paste0(data_dir, "combined_CMPOP.rds"))

print (sort(colnames(combined_CMPOP)))
print("_______________________________")
print ("unique(ClimateGroup) ")
print (unique(combined_CMPOP$ClimateGroup))

print("_______________________________")
print ("unique(ClimateScenario)")
print (unique(combined_CMPOP$ClimateScenario))

print("_______________________________")
print ("unique(CountyGroup)")
print (unique(combined_CMPOP$CountyGroup))

print("_______________________________")

CMPOP_loc_ddd_acDD <- subset(combined_CMPOP, 
	                         select = c("location", "year", "DailyDD", "CumDDinC"))

saveRDS(CMPOP_loc_ddd_acDD, paste0(data_dir, "CMPOP_loc_ddd_acDD.rds"))

print ("It took goddamn: ")
print (Sys.time() - st_time)




