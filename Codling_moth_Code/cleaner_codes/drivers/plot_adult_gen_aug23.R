#!/share/apps/R-3.2.2_gcc/bin/Rscript
#library(chron)
library(data.table)
library(ggplot2)
source_path1 = "/home/hnoorazar/cleaner_codes/core.R"
source_path2 = "/home/hnoorazar/cleaner_codes/core_plot.R"
source(source_path1)
source(source_path2)


data_dir = "/data/hydro/users/Hossein/codling_moth/local/processed/"
plot_path = "/data/hydro/users/Hossein/codling_moth/local/processed/plots/"

file_name = "generations_combined_CMPOP_rcp45.rds"
plot_output_name = "Adult_Gen_Aug23_rcp45.png"
color_ord = c("grey70", "dodgerblue", "olivedrab4", "red")
plot_generations_Aug23(input_dir=data_dir, 
	                   file_name, 
	                   box_width=.25, 
	                   plot_path, 
	                   plot_output_name, 
	                   color_ord)

file_name = "generations_combined_CMPOP_rcp85.rds"
output_name = "Adult_Gen_Aug23_rcp85.png"

plot_adult_generations_Aug23(input_dir=data_dir, 
	                         file_name, 
	                         box_width=.25, 
	                         plot_path, 
	                         plot_output_name, 
	                         color_ord)