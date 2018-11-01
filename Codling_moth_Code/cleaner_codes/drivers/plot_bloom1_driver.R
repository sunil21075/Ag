#!/share/apps/R-3.2.2_gcc/bin/Rscript
#library(chron)
library(data.table)
library(ggplot2)
source_path = "/home/hnoorazar/cleaner_codes/core.R"
source(source_path)


args = commandArgs(trailingOnly=TRUE)
version = args[1]


data_dir = "/data/hydro/users/Hossein/codling_moth/local/processed/"
plot_path = "/data/hydro/users/Hossein/codling_moth/local/processed/plots/"


plot_bloom1(data_dir, file_name = "vertdd_combined_CMPOP_", version, plot_path, output_name="bloom1")