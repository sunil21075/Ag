
"""
This is the one that worked before 
putting merge into the core.R
"""
#!/share/apps/R-3.2.2_gcc/bin/Rscript
#library(chron)
library(data.table)
#library(reshape2)
library(dplyr)
library(foreach)
library(doParallel)
#library(iterators)

read_data_dir = "/data/hydro/users/Hossein/codling_moth/local/processed/"
write_path    = "/data/hydro/users/Hossein/codling_moth/local/processed/"
param_dir     = "/home/hnoorazar/cleaner_codes/parameters/"


categories = c("historical", "BNU-ESM", "CanESM2", "GFDL-ESM2G", "bcc-csm1-1-m", "CNRM-CM5", "GFDL-ESM2M")
file_prefix = "CMPOP_"
# file_prefix = "CM_"
locations_list = "local_list"
data = data.table()
conn = file(paste0(param_dir, locations_list), open = "r")
locations = readLines(conn)
close(conn)

#args = commandArgs(trailingOnly=TRUE)
#category = args[1]

for( category in categories) {
    for( location in locations) {
	    if(category != "historical") {
		    filename <- paste0(read_data_dir, "future_CMPOP/", category, "/rcp45/", file_prefix, location)
	    }
	    else {
		    filename <- paste0(read_data_dir, "historical_CMPOP/", file_prefix, location)
	        }
	#print(filename)
	data <- rbind(data, read.table(filename, header = TRUE, sep = ","))
    }
}

#cl <- makeCluster(6)
#registerDoParallel(cl)
#data <- foreach(category = categories, .combine = rbind) %:% 
#  foreach(location = locations, .combine = rbind) %dopar% {
#    filename <- paste0(data_dir, "/", category, "/", file_prefix, location)
#    print(filename)
#    read.table(filename, header = TRUE, sep = ",")
#  }
#stopCluster(cl)
#saveRDS(data, paste0(data_dir, "/", category, "Data_rcp45.rds"))
saveRDS(data, paste0(write_path, "/combined_CMPOP_rcp45.rds"))
