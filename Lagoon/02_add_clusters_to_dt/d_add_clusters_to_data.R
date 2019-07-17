.libPaths("/data/hydro/R_libs35")
.libPaths()
library(data.table)
library(dplyr)

options(digit=9)
options(digits=9)

# Time the processing of this batch of files
start_time <- Sys.time()

###########################################################

main_in <- "/data/hydro/users/Hossein/lagoon/01_storm_cumPrecip/"
param_dir <- "/home/hnoorazar/lagoon_codes/parameters/"
###########################################################
obs_clusters <- read.csv(paste0(param_dir, "observed_clusters.csv"),
                         header=T, as.is=T)
obs_clusters <- subset(obs_clusters, select = c("location", "cluster")) %>%
                data.table()

# subdir <- c("cum_precip/annual/")
# subdir <- c("cum_precip/chunky/")
# subdir <- c("cum_precip/monthly/")
# subdir <- c("cum_precip/wtr_yr/")
# subdir <- c("storm/")

for (sub in subdir){
  in_dir <- file.path(paste0(main_in, sub))
  files_list <- list.files(path=in_dir, pattern=".rds")
  print(in_dir)
  for (file in files_list){
    print(file)
    A <- data.table(readRDS(paste0(in_dir, file)))
    A <- merge(A, obs_clusters, by="location", all.x=T)
    saveRDS(A, paste0(in_dir, file))
  }
}

end_time <- Sys.time()
print( end_time - start_time)







