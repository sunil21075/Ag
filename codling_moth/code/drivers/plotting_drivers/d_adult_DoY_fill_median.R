#!/share/apps/R-3.2.2_gcc/bin/Rscript
#library(chron)
library(data.table)
library(ggplot2)
source_path1 = "/home/hnoorazar/cleaner_codes/core.R"
source_path2 = "/home/hnoorazar/cleaner_codes/core_plot.R"
source(source_path1)
source(source_path2)

#########
######### Running this is gonna take at least 40 minutes, 
######### IF it is successful! R memory allocation problem!
#########
args = commandArgs(trailingOnly=TRUE)
version = args[1]

data_dir = "/data/hydro/users/Hossein/codling_moth_new/local/processed/"
plot_path = "/data/hydro/users/Hossein/codling_moth_new/local/processed/plots/"

plot_adult_DoY_filling_median(input_dir=data_dir, 
	                          file_name ="combined_CMPOP_", 
                              version=version, 
                              output_dir=plot_path)