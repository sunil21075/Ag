#!/share/apps/R-3.2.2_gcc/bin/Rscript

library(data.table)
library(dplyr)
library(foreach)

read_data_dir= "/data/hydro/users/Hossein/chill/a_limited_locs/"
write_dir   = "/data/hydro/users/Hossein/chill/"
file_names = c("met_hourly_data_43.53125_-116.59375.rds", 
	           "met_hourly_data_45.65625_-121.15625.rds")
weather_type = c("warm", "cold")

climate_scenarios = list.files(read_data_dir, all.files=F, include.dirs=F)
print (climate_scenarios)
projection_type = c("historical", "rcp45", "rcp85")

observed = data.table()
modeled_hist = data.table()
rcp45 = data.table()
rcp85 = data.table()

for (cs in climate_scenarios){
	if (cs == "historical"){
		curr_path = file.path(read_data_dir, cs)
		f = data.table(readRDS(paste0(curr_path, "/", file_names[1])))
		f$climateScenario = "observed"
		f$CountyGroup = "warm"
		f$location = "43.53125_-116.59375"
		observed <- rbind(observed, f)
		
		f = data.table(readRDS(paste0(curr_path, "/", file_names[2])))
		f$climateScenario = "observed"
		f$CountyGroup = "cold"
		f$location = "45.65625_-121.15625"
		observed <- rbind(observed, f)
	} else{		
		for (proj in projection_type){
			curr_path = file.path(read_data_dir, cs, proj)
			for (file in file_names){
				f = data.table(readRDS(paste0(curr_path, "/",file)))
			    f$climateScenario = cs
			    
			    if (file==file_names[1]){
			    	f$CountyGroup = "warm"
			    	f$location = "43.53125_-116.59375"
			    	} else{
			    		f$CountyGroup = "cold"
			    		f$location = "45.65625_-121.15625"
			    	}
			    
			    if (proj=="historical"){
			    	modeled_hist <- rbind(modeled_hist, f)
			    	} else if (proj=="rcp45"){
			    	      rcp45 <- rbind(rcp45, f)
			    	} else {
			    	    rcp85 <- rbind(rcp85, f)
			    }
			}
		}
	}
}
needed_cols = c("Year", "Month", "Temp",
	            "Chill_season", "climateScenario", 
	            "CountyGroup", "location")

observed = subset(observed, select=needed_cols)
rcp45 = subset(rcp45, select=needed_cols)
rcp85 = subset(rcp85, select=needed_cols)
modeled_hist = subset(modeled_hist, select=needed_cols)

saveRDS(observed, paste0(write_dir, "observed", ".rds"))
saveRDS(modeled_hist, paste0(write_dir, "modeled_hist.rds"))
saveRDS(rcp45, paste0(write_dir, "rcp45.rds"))
saveRDS(rcp85, paste0(write_dir, "rcp85.rds"))




