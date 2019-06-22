.libPaths("/data/hydro/R_libs35")
.libPaths()

library(data.table)
library(ggplot2)
source_path1 = "/home/hnoorazar/cleaner_codes/core.R"
source_path2 = "/home/hnoorazar/cleaner_codes/core_plot.R"
source(source_path1)
source(source_path2)

args = commandArgs(trailingOnly=TRUE)
version = args[1]

data_dir = "/data/hydro/users/Hossein/codling_moth_new/local/processed/vertdd_with_new_normal_params/"
plot_path = "/data/hydro/users/Hossein/codling_moth_new/local/processed/plots/new_params/"

if (dir.exists(file.path(plot_path)) == F) {
  dir.create(path = plot_path, recursive = T)
}

if (version == "rcp45"){
	x_limits = c(70, 135)
} else {x_limits = c(60, 135)}

plot_bloom(data_dir, 
	       file_name = "vertdd_combined_CMPOP_", 
	       version, 
	       plot_path, 
	       output_name="bloom", 
	       x_limits)