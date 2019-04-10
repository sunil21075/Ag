.libPaths("/data/hydro/R_libs35")
.libPaths()

library(data.table)
library(dplyr)
library(raster)
library(FNN)
library(RColorBrewer)
library(colorRamps)
library(EnvStats, lib.loc = "~/.local/lib/R3.5.1")

source_path = "/home/hnoorazar/analog_codes/core_analog.R"
source(source_path)
options(digit=9)
options(digits=9)

################################################################################
# 
#                   Directories
# 
################################################################################
main_dir <- "/data/hydro/users/Hossein/analog/"

main_us_dir <- file.path(main_dir, "usa/ready_features/")
main_local_dir <- file.path(main_dir, "local/ready_features/one_file_4_all_locations/")
main_out <- file.path(main_dir, "03_analogs/no_gen_3/")
################################################################################
################################################################################
# 
#                   Terminal arguments
# 
################################################################################
args = commandArgs(trailingOnly=TRUE)
model_type = args[1]
emission_type = args[2]

################################################################################
# 
#                   Read Data
# 
################################################################################

all_dt_usa <- data.table(readRDS(paste0(main_us_dir, "all_data_usa.rds")))
local_dt <- data.table(readRDS(paste0(main_local_dir, "feat_", model_type, "_", emission_type, ".rds")))

###########################################################################
################################################################################
# 
#                   Set parameters and run the code
# 
################################################################################
n_nghs = 500
precip = FALSE
# create subdirectory for specific emission types

out_dir = file.path(main_out, "no_precip", n_nghs, emission_type)
if (dir.exists(out_dir) == F) {
  dir.create(path = out_dir, recursive = T)
}
information = find_NN_info_W4G_ICV(ICV=all_dt_usa, historical_dt=all_dt_usa, 
                                   future_dt=local_dt, n_neighbors=n_nghs, precipitation=precip)

NN_dist_tb = information[[1]]
NN_loc_year_tb = information[[2]]
NN_sigma_tb = information[[3]]

saveRDS(NN_dist_tb, paste0(out_dir, "/NN_dist_tb_", model_type, ".rds"))
saveRDS(NN_loc_year_tb, paste0(out_dir, "/NN_loc_year_tb_", model_type, ".rds"))
saveRDS(NN_sigma_tb, paste0(out_dir, "/NN_sigma_tb_", model_type, ".rds"))

print (head(colnames(NN_dist_tb)), 10)
print (head(colnames(NN_loc_year_tb)), 10)
print (head(colnames(NN_sigma_tb)), 10)



