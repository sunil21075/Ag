#!/share/apps/R-3.2.2_gcc/bin/Rscript
#library(chron)
library(data.table)
library(ggplot2)

source_path = "/home/hnoorazar/cleaner_codes/core.R"
source(source_path)


write_dir = "/data/hydro/users/Hossein/codling_moth/local/processed/"
input_dir = "/data/hydro/users/Hossein/codling_moth/local/processed/"
param_dir = "/home/hnoorazar/cleaner_codes/parameters/"

args = commandArgs(trailingOnly=TRUE)
version = args[1]

file_name = paste0("combined_CMPOP_", version)
output = diapause_abs_rel(input_dir, 
	                      file_name,
                          param_dir, 
                          location_group_name="LocationGroups.csv")

RelData = data.table(output[[1]])
AbsData = data.table(output[[2]])

saveRDS(RelData, paste0(data_dir, "/", "diapause_rel_data_", version, ".rds"))
saveRDS(AbsData, paste0(data_dir, "/", "diapause_abs_data_", version, ".rds"))