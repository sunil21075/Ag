rm(list=ls())
library(chron)
library(data.table)
library(reshape2)
library(dplyr)
library(foreach)
library(iterators)
library(ggplot2)

input_dir = "/Users/hn/Desktop/Kirti/check_point/my_aeolus_2015/all_local/diapause/"
plot_path = input_dir
#### Relative

file_name_extension = "diapause_rel_data_rcp85.rds"
version = "rcp85"
plot_rel_diapause(input_dir, file_name_extension, version, plot_path)

file_name_extension = "diapause_rel_data_rcp45.rds"
version = "rcp45"
plot_rel_diapause(input_dir, file_name_extension, version, plot_path)

#### Absolute
file_name_extension = "diapause_abs_data_rcp85.rds"
version = "rcp85"
plot_abs_diapause(input_dir, file_name_extension, version, plot_path)

file_name_extension = "diapause_abs_data_rcp45.rds"
version = "rcp45"
plot_abs_diapause(input_dir, file_name_extension, version, plot_path)

