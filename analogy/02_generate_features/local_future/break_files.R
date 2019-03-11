.libPaths("/data/hydro/R_libs35")
.libPaths()
library(data.table)
library(dplyr)

source_path = "/home/hnoorazar/analog_codes/core_analog.R"
source(source_path)
options(digit=9)
options(digits=9)

#########################################################################
main_in = "/data/hydro/users/Hossein/analog/local/ready_features/"
print ("line 13 main_in:")
print (main_in)
print ("________________________")

dirs <- c("averaged_data", "bcc_csm1_1_m", "BNU_ESM", "CanESM2", "CNRM_CM5", "GFDL_ESM2G", "GFDL_ESM2M")

for (curr_sub in dirs){
  curr_dir = paste0(main_in, curr_sub, "/")
  print ("line 21 main_in:")
  print (curr_dir)
  print ("________________________")
  files_list <- dir(file.path(curr_dir))
  print ("line 24 main_in:")
  print (files_list)
  print ("________________________")
  rds_files <- files_list[grep(pattern = ".rds", x = files_list)]
  print ("line 29 main_in:")
  print (rds_files)
  print ("________________________")

  for (file in rds_files){
    carbon_type = unlist(strsplit(tail(unlist(strsplit(file, "_")), n=1), ".rds"))
    print ("line 35 main_in:")
    print (carbon_type)
    print ("________________________")

    curr_out = file.path(curr_dir, carbon_type)
    print ("line 40 main_in:")
    print (curr_out)
    print ("________________________")

    if (dir.exists(file.path(curr_out)) == F){
      dir.create(path = curr_out, recursive = T)
    }
    curr_out = paste0(curr_out, "/")

    curr_file <- data.table(readRDS(paste0(curr_dir, file)))
    all_locations = unique(curr_file$location)
    for (loc in all_locations){
      curr_loc_data <- curr_file %>% filter(location == loc)
      saveRDS(curr_loc_data, paste0(curr_out, "/features_", loc, ".rds"))
    }
  }
}

