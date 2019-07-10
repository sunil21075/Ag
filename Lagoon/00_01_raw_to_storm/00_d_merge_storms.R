.libPaths("/data/hydro/R_libs35")
.libPaths()
library(data.table)
library(lubridate)
library(dplyr)

options(digit=9)
options(digits=9)

# Time the processing of this batch of files
start_time <- Sys.time()

################################################################
main_in <- "/data/hydro/users/Hossein/lagoon/00_headache/"
out_dir <- "/data/hydro/users/Hossein/lagoon/01/storm/parallel_obtained/"
if (dir.exists(out_dir) == F) {dir.create(path = out_dir, recursive = T)}
################################################################
model_names <- c("bcc-csm1-1", "CanESM2", "CSIRO-Mk3-6-0",
                 "HadGEM2-CC365", "IPSL-CM5A-LR", "MIROC5",
                 "NorESM1-M", "bcc-csm1-1-m", "CCSM4", 
                 "GFDL-ESM2G", "HadGEM2-ES365", "IPSL-CM5A-MR",
                 "MIROC-ESM-CHEM", "BNU-ESM", "CNRM-CM5",
                 "GFDL-ESM2M", "inmcm4", "IPSL-CM5B-LR",
                 "MRI-CGCM3")

emission <- c("historical", "rcp45", "rcp85")

rcp45_data <- data.table()
rcp85_data <- data.table()
hist_data <- data.table()

for (model in model_names){
  current_45 <- readRDS(paste0(main_in, model, "/rcp45/", "storm.rds"))
  current_85 <- readRDS(paste0(main_in, model, "/rcp85/", "storm.rds"))
  current_hist<-readRDS(paste0(main_in, model, "/historical/", "storm.rds"))

  current_45$model <- gsub("-", "_", current_45$model)
  current_85$model <- gsub("-", "_", current_85$model)
  current_hist$model <- gsub("-", "_", current_hist$model)

  rcp45_data <- rbind(rcp45_data, current_45)
  rcp85_data <- rbind(rcp85_data, current_85)
  hist_data <- rbind(hist_data, current_hist)
}
hist_data <- unique(hist_data)
rcp45_data$emission <- "RCP 4.5"
rcp85_data$emission <- "RCP 8.5"

hist_data_45 <- hist_data
hist_data_85 <- hist_data
hist_data_45$emission <- "RCP 4.5"
hist_data_85$emission <- "RCP 8.5"

all_storms <- rbind(rcp45_data, rcp85_data, hist_data_45, hist_data_85)
saveRDS(all_storms, paste0(out_dir, "all_storms.rds"))
# saveRDS(rcp45_data, paste0(out_dir, "storm_RCP45.rds"))
# saveRDS(rcp85_data, paste0(out_dir, "storm_RCP85.rds"))
# saveRDS(hist_data, paste0(out_dir, "storm_modeled_hist.rds"))







