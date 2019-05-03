#!/share/apps/R-3.2.2_gcc/bin/Rscript

library(data.table)
library(dplyr)
library(foreach)
library(doParallel)

source_path = "/home/hnoorazar/cleaner_codes/core.R"
source(source_path)
             
args = commandArgs(trailingOnly=TRUE)
version = args[1]
bloom_cut_off = args[2]



data_dir  = "/data/hydro/users/Hossein/codling_moth_new/local/processed/"
write_dir = "/data/hydro/users/Hossein/codling_moth_new/local/processed/"


filename <- paste0(data_dir, "vertdd_combined_CMPOP_", version, ".rds")

data <- data.table(readRDS(filename))

data = bloom(data, bloom_cut_off)

saveRDS(data, paste0(write_dir, "bloom_", version, "_", bloom_cut_off, ".rds"))
