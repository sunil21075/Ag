#!/share/apps/R-3.2.2_gcc/bin/Rscript
#library(chron)
library(data.table)
library(ggplot2)

source_path = "/home/hnoorazar/cleaner_codes/core.R"
source(source_path)
write_dir = "/data/hydro/users/Hossein/codling_moth_new/local/diap_shift_flat/"
input_dir = "/data/hydro/users/Hossein/codling_moth_new/local/processed/"
param_dir = "/home/hnoorazar/cleaner_codes/parameters/"

args = commandArgs(trailingOnly=TRUE)
version = args[1]
shift_amount = as.numeric(args[2])

file_name = paste0("combined_CMPOP_", version)
output = diap_sense_flat(input_dir = input_dir, 
	                     file_name = file_name,
                         param_dir = param_dir, 
                         location_group_name="LocationGroups.csv",
                         shift_amount = shift_amount)

RelData = data.table(output[[1]])
AbsData = data.table(output[[2]])
sub1 = data.table(output[[3]])

saveRDS(RelData, paste0(write_dir, "diapause_rel_",  version, "shift", shift_amount, ".rds"))
saveRDS(AbsData, paste0(write_dir, "diapause_abs_",  version, "shift", shift_amount, ".rds"))
saveRDS(sub1,    paste0(write_dir, "diapause_plot_", version, "shift", shift_amount, ".rds"))