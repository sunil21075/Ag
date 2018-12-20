#!/share/apps/R-3.2.2_gcc/bin/Rscript
#library(chron)
library(data.table)
library(ggplot2)

source_path = "/home/hnoorazar/cleaner_codes/core.R"
source(source_path)
write_dir = "/data/hydro/users/Hossein/codling_moth_new/local/diapause_sensitivity/new_funcs/"
input_dir = "/data/hydro/users/Hossein/codling_moth_new/local/diapause_sensitivity/original_data/"
param_dir = "/home/hnoorazar/cleaner_codes/parameters/"

args = commandArgs(trailingOnly=TRUE)
version = args[1]
	
file_name = paste0("combined_CMPOP_", version)

dp_orig = c(102.6077, 1.306483, 16.95815) # original
dp_1 = c(103, 0.8, 15.95) # red solid one
dp_2 = c(103, 0.95, 16.5) # blue solid
dp_3 = c(104, 0.8, 16.3)  # bright green solid
dp_4 = c(104, 0.7, 15.5)  # dot line
dp_5 = c(104, 0.6, 15.2)  # red dashed

diap_params = list(dp_orig, dp_1, dp_2, dp_3, dp_4, dp_5)
for (ii in 1:6){
	output = diapause_abs_rel_shifts(input_dir, 
		                             file_name,
	                                 param_dir,
	                                 diap_param=unlist(diap_params[ii]),
	                                 shift=0,
	                                 location_group_name="LocationGroups.csv")

	RelData = data.table(output[[1]])
	AbsData = data.table(output[[2]])
	sub1 = data.table(output[[3]])
	# pre_diap_plot = data.table(output[[4]])

	saveRDS(RelData, paste0(write_dir, "diapause_rel_", version, "_", ii, ".rds"))
	saveRDS(AbsData, paste0(write_dir, "diapause_abs_", version, "_", ii, ".rds"))
	saveRDS(sub1,    paste0(write_dir, "diapause_plot_", version, "_", ii, ".rds"))
	# saveRDS(pre_diap_plot, paste0(write_dir, "pre_diap_plot_", version, ".rds"))
}

