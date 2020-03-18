######
######   Compute Grant double peak area after filtering unwanted plants out
######
rm(list=ls())
library(data.table)
library(dplyr)

data_dir <- paste0("/Users/hn/Documents/01_research_data/", 
                   "remote_sensing/01_NDVI_TS/Grant/No_EVI/", 
                   "Grant_10_cloud/Grant_2017/")


grant_2017_double_peak <- read.csv(paste0(data_dir, "double_polygons.csv"), as.is=TRUE)
grant_2017_double_peak <- within(grant_2017_double_peak, remove("geo"))

double_crop_potential_plants <- read.csv(paste0(data_dir, "double_crop_potential_plants.csv"), as.is=TRUE)


grant_2017_double_peak <- grant_2017_double_peak %>% 
                          filter(CropTyp %in% double_crop_potential_plants$Crop_Type) %>%
                          data.table()

sum(grant_2017_double_peak$ExctAcr) # 4797.035








