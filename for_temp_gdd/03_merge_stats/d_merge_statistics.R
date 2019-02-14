######################################################################
##         
##         Merge statistics of all models into one.
##         
######################################################################

.libPaths("/data/hydro/R_libs35")
.libPaths()
library(data.table)
library(dplyr)
library(lubridate)
library(tidyverse)

input_dir = "/data/hydro/users/Hossein/temp_gdd/modeled/"

categories = list.dirs(path = input_dir, full.names = F, recursive = F)
versions = c("historical", "rcp45", "rcp85")
print (categories)
data = data.table()

for (cat in categories){
	data_dir = file.path(input_dir, cat, "/")
	print (data_dir)
	
	histo = data.table(readRDS(paste0(data_dir, "historical_stat.rds")))
	rcp45 = data.table(readRDS(paste0(data_dir, "rcp45_stat.rds")))
	rcp85 = data.table(readRDS(paste0(data_dir, "rcp85_stat.rds")))
	
	data = rbind(data, histo, rcp45, rcp85)
	rm(histo, rcp45, rcp85)
}
output_dir= "/data/hydro/users/Hossein/temp_gdd/cleaned_data_for_plot/"
saveRDS(data, paste0(output_dir, "all_modeled_stat.rds"))
rm(data)

