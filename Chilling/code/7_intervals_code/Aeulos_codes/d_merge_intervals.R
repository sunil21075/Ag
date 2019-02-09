#!/share/apps/R-3.2.2_gcc/bin/Rscript

library(data.table)
library(dplyr)
library(foreach)

read_data_dir= "/data/hydro/users/Hossein/chill/7_time_intervals/a_limited_locs/"
write_dir   = "/data/hydro/users/Hossein/chill/7_time_intervals/"
file_names = c("met_hourly_data_43.53125_-116.59375.rds", 
	           "met_hourly_data_45.65625_-121.15625.rds")
weather_type = c("Warmer", "Cooler")

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
		f$CountyGroup = "Warmer"
		f$location = "43.53125_-116.59375"
		observed <- rbind(observed, f)
		
		f = data.table(readRDS(paste0(curr_path, "/", file_names[2])))
		f$climateScenario = "observed"
		f$CountyGroup = "Cooler"
		f$location = "45.65625_-121.15625"
		observed <- rbind(observed, f)
	} else{		
		for (proj in projection_type){
			curr_path = file.path(read_data_dir, cs, proj)
			for (file in file_names){
				f = data.table(readRDS(paste0(curr_path, "/",file)))
			    f$climateScenario = cs
			    
			    if (file==file_names[1]){
			    	f$CountyGroup = "Warmer"
			    	f$location = "43.53125_-116.59375"
			    	} else{
			    		f$CountyGroup = "Cooler"
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
# pick up the variables we need
needed_cols = c("Year", "Month", "Temp",
	            "Chill_season", "climateScenario", 
	            "CountyGroup", "location")

observed = subset(observed, select=needed_cols)
rcp45 = subset(rcp45, select=needed_cols)
rcp85 = subset(rcp85, select=needed_cols)
modeled_hist = subset(modeled_hist, select=needed_cols)

## pick up the months we are interested in:
# to make the files smaller
# Months of Interest:
mos = c(9, 10, 11, 12, 1, 2, 3)
observed <- observed  %>% filter(Month %in% mos)
rcp45 <- rcp45  %>% filter(Month %in% mos)
rcp85 <- rcp85  %>% filter(Month %in% mos)
modeled_hist <- modeled_hist  %>% filter(Month %in% mos)

observed$scenario = "observed"
colnames(observed)[colnames(observed) == 'climateScenario'] <- 'model'
saveRDS(observed, paste0(write_dir, "observed", ".rds"))

colnames(modeled_hist)[colnames(modeled_hist) == 'climateScenario'] <- 'model'
colnames(rcp45)[colnames(rcp45) == 'climateScenario'] <- 'model'
colnames(rcp85)[colnames(rcp85) == 'climateScenario'] <- 'model'

modeled_hist$scenario = "historical"
rcp45$scenario = "rcp45"
rcp85$scenario = "rcp85"

saveRDS(modeled_hist, paste0(write_dir, "modeled_hist.rds"))
saveRDS(rcp45, paste0(write_dir, "rcp45.rds"))
saveRDS(rcp85, paste0(write_dir, "rcp85.rds"))

modeled = bind_rows(modeled_hist, rcp45, rcp85)
saveRDS(modeled, paste0(write_dir, "modeled.rds"))
rm(rcp85, rcp45, modeled_hist)

mos = c(9, 10, 11, 12, 1, 2, 3)
month_names = c("Jan", "Feb", "Mar", "Apr", 
	            "May", "Jun", "Jul", "Aug" ,
	            "Sept", "Oct", "Nov", "Dec")
for (month in mos){
	data <- modeled %>% filter(Month == month)
	saveRDS(data, paste0(write_dir, month_names[month], ".rds"))
	rm(data)
	data <- observed %>% filter(Month == month)
	saveRDS(data, paste0(write_dir, "observed_", month_names[month], ".rds"))
}

mos = c(9, 10, 11, 12)
modeled <- modeled  %>% filter(Month %in% mos)
saveRDS(modeled, paste0(write_dir, "sept_through_dec_modeled.rds"))

observed <- observed  %>% filter(Month %in% mos)
saveRDS(observed, paste0(write_dir, "sept_through_dec_observed.rds"))
