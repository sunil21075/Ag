#!/share/apps/R-3.2.2_gcc/bin/Rscript
#library(chron)
library(data.table)
library(ggplot2)
source_path = "/home/hnoorazar/cleaner_codes/core.R"
source(source_path)


args = commandArgs(trailingOnly=TRUE)
version = args[1]
output_type = args[2]


data_dir = "/data/hydro/users/Hossein/codling_moth/local/processed/"
plot_path = "/data/hydro/users/Hossein/codling_moth/local/processed/plots/"

plot_cumdd(input_dir=data_dir, file_name ="combined_CMPOP_", version, output_dir=plot_path, output_type)
