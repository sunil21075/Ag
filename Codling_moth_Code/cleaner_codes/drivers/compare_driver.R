#!/share/apps/R-3.2.2_gcc/bin/Rscript
#library(chron)
library(data.table)
#library(reshape2)
library(dplyr)
library(foreach)
library(doParallel)
#library(iterators)
source_path = "/home/hnoorazar/cleaner_codes/core.R"
source(source_path)


input_1_dir = "/data/hydro/users/Hossein/codling_moth/local/processed/"
input_1_name = "combined_CMPOP_rcp45.rds"

input_2_dir = "/data/hydro/users/giridhar/giridhar/codmoth_pop/"
input_2_name = "allData_grouped_counties_rcp45.rds"

write_dir = input_1_dir

my_file_name = paste0(input_1_dir, input_1_name)
girid_file_name = paste0(input_2_dir, input_2_name)

my_file = readRDS(my_file_name)
girid_file = readRDS(girid_file_name)

compare_result = grep(x = my_file == girid_file, pattern = "TRUE")


write.table(data.frame(compare_result, my_file_name, girid_file_name), paste0(write_dir, "compare_result" , ".txt"), sep="\t")