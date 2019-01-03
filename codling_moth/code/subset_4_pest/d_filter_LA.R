#!/share/apps/R-3.2.2_gcc/bin/Rscript
#library(chron)
library(data.table)
library(dplyr)

data_dir  = "/data/hydro/users/Hossein/codling_moth_new/local/processed/section_46_Pest/"
output_dir= "/data/hydro/users/Hossein/codling_moth_new/local/processed/section_46_Pest/quantiles_data/"
name_pref = "larva_data"

models <- c("45.rds", "85.rds")

args = commandArgs(trailingOnly=TRUE)
quan = as.double(args[1])

min_pop_cut_off <- c(0.005, 0.01, 0.02, 0.04, 0.05)

for (model in models){
	curr_data = data.table(readRDS(paste0(data_dir, name_pref, model)))

	gen_1 <- subset(curr_data, select = c(ClimateGroup, CountyGroup, dayofyear, year, PercLarvaGen1, latitude, longitude))
	gen_2 <- subset(curr_data, select = c(ClimateGroup, CountyGroup, dayofyear, year, PercLarvaGen2, latitude, longitude))
	gen_3 <- subset(curr_data, select = c(ClimateGroup, CountyGroup, dayofyear, year, PercLarvaGen3, latitude, longitude))
	gen_4 <- subset(curr_data, select = c(ClimateGroup, CountyGroup, dayofyear, year, PercLarvaGen4, latitude, longitude))
	rm(curr_data) # free up memory
	for (cut in min_pop_cut_off){
		gen_1_cut <- gen_1[gen_1$PercLarvaGen1 >= cut, ]
		gen_2_cut <- gen_2[gen_2$PercLarvaGen2 >= cut, ]
		gen_3_cut <- gen_3[gen_3$PercLarvaGen3 >= cut, ]
		gen_4_cut <- gen_4[gen_4$PercLarvaGen4 >= cut, ]

		filtered_gen_1 = gen_1_cut %>% group_by(latitude, longitude, year, ClimateGroup, CountyGroup) %>% arrange(abs(PercLarvaGen1 - quan)) %>% slice(1)
		filtered_gen_2 = gen_2_cut %>% group_by(latitude, longitude, year, ClimateGroup, CountyGroup) %>% arrange(abs(PercLarvaGen2 - quan)) %>% slice(1)
		filtered_gen_3 = gen_3_cut %>% group_by(latitude, longitude, year, ClimateGroup, CountyGroup) %>% arrange(abs(PercLarvaGen3 - quan)) %>% slice(1)
		filtered_gen_4 = gen_4_cut %>% group_by(latitude, longitude, year, ClimateGroup, CountyGroup) %>% arrange(abs(PercLarvaGen4 - quan)) %>% slice(1)

		filtered_gen_1 <- subset(filtered_gen_1, select=c(ClimateGroup, CountyGroup, dayofyear, year, PercLarvaGen1))
		filtered_gen_2 <- subset(filtered_gen_2, select=c(ClimateGroup, CountyGroup, dayofyear, year, PercLarvaGen2))
		filtered_gen_3 <- subset(filtered_gen_3, select=c(ClimateGroup, CountyGroup, dayofyear, year, PercLarvaGen3))
		filtered_gen_4 <- subset(filtered_gen_4, select=c(ClimateGroup, CountyGroup, dayofyear, year, PercLarvaGen4))

		filtered_gen_1 <- melt(filtered_gen_1, id=c("ClimateGroup", "CountyGroup", "dayofyear", "year"))
		filtered_gen_2 <- melt(filtered_gen_2, id=c("ClimateGroup", "CountyGroup", "dayofyear", "year"))
		filtered_gen_3 <- melt(filtered_gen_3, id=c("ClimateGroup", "CountyGroup", "dayofyear", "year"))
		filtered_gen_4 <- melt(filtered_gen_4, id=c("ClimateGroup", "CountyGroup", "dayofyear", "year"))
        
        data <- rbind(filtered_gen_1, filtered_gen_2, filtered_gen_3, filtered_gen_4)

		saveRDS(data, paste0(output_dir, "Larva_", as.character(quan*100), "%_", as.character(cut), "_", model))
	}
}