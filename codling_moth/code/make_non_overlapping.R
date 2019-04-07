
.libPaths("/data/hydro/R_libs35")
.libPaths()
library(data.table)
library(dplyr)
options(digit=9)
options(digits=9)

input_dir <- "/data/hydro/users/Hossein/codling_moth_new/local/processed/overlaping/"
out_dir <- "/data/hydro/users/Hossein/codling_moth_new/local/processed/"

make_non_overlapping <- function(overlap_dt){
  old_time_periods <- c("2040's", "2060's", "2080's", "Historical")
  new_time_periods <- c("2026-2050", "2051-2075", "2076-2095", "Historical")

  ########## Historical
  hist <- overlap_dt %>% filter(ClimateGroup == "Historical")

  ########## 2040
  F1 <- overlap_dt %>% filter(ClimateGroup == "2040's")
  F1 <- F1 %>% filter(year>=2025 & year<=2050)
  F1$ClimateGroup <- new_time_periods[1]

  ########## 2060
  F2 <- overlap_dt %>% filter(ClimateGroup == "2060's")
  F2 <- F2 %>% filter(year>=2051 & year<=2075)
  F2$ClimateGroup <- new_time_periods[2]

  ########## 2080
  F3 <- overlap_dt %>% filter(ClimateGroup == "2080's")
  F3 <- F3 %>% filter(year>=2076)
  F3$ClimateGroup <- new_time_periods[3]

  non_overlap_dt <- rbind(hist, F1, F2, F3)
  non_overlap_dt$ClimateGroup <- factor(non_overlap_dt$ClimateGroup, levels=new_time_periods, order=T)

}

########## RCP45 
file_n <- "combined_CM_rcp45.rds"
file_name <- paste0(input_dir, file_n)
data <- data.table(readRDS(file_name))

combined_CM_rcp45 <- make_non_overlapping(data)

saveRDS(combined_CM_rcp45, paste0(out_dir, "combined_CM_rcp45.rds"))
rm(combined_CM_rcp45, data)

##########
########## rcp85
##########
file_n <- "combined_CM_rcp85.rds"
file_name <- paste0(input_dir, file_n)
data <- data.table(readRDS(file_name))

combined_CM_rcp85 <- make_non_overlapping(data)

saveRDS(combined_CM_rcp85, paste0(out_dir, "combined_CM_rcp85.rds"))
rm(combined_CM_rcp85, data)
#############################
#
#          CMPOP
#
#############################
.libPaths("/data/hydro/R_libs35")
.libPaths()
library(data.table)
library(dplyr)
options(digit=9)
options(digits=9)

input_dir <- "/data/hydro/users/Hossein/codling_moth_new/local/processed/overlaping/"
out_dir <- "/data/hydro/users/Hossein/codling_moth_new/local/processed/"

########## RCP45

file_n <- "combined_CMPOP_rcp45.rds"
file_name <- paste0(input_dir, file_n)
data <- data.table(readRDS(file_name))

########## RCP45 - Historical

hist <- data %>% filter(ClimateGroup == "Historical")
saveRDS(hist, paste0(out_dir, "CMPOP_hist_45.rds"))
rm(hist)

########## RCP45 - 2040
F1 <- data %>% filter(ClimateGroup == "2040's")
F1 <- F1 %>% filter(year>=2025 & year<=2050)
F1$ClimateGroup <- "2026-2050"
saveRDS(F1, paste0(out_dir, "CMPOP_F1_45.rds"))
rm(F1)

########## RCP45 - 2060
F2 <- data %>% filter(ClimateGroup == "2060's")
F2 <- F2 %>% filter(year>=2051 & year<=2075)
F2$ClimateGroup <- "2051-2075"
saveRDS(F2, paste0(out_dir, "CMPOP_F2_45.rds"))
rm(F2)

########## RCP45 - 2080
F3 <- data %>% filter(ClimateGroup == "2080's")
F3 <- F3 %>% filter(year>=2076)
F3$ClimateGroup <- "2076-2095"
saveRDS(F3, paste0(out_dir, "CMPOP_F3_45.rds"))
rm(F3, data)

##############################
##########
########## rcp85
##########
##############################

file_n <- "combined_CMPOP_rcp85.rds"
file_name <- paste0(input_dir, file_n)
data <- data.table(readRDS(file_name))

########## rcp85 - Historical
hist <- data %>% filter(ClimateGroup == "Historical")
saveRDS(hist, paste0(out_dir, "CMPOP_hist_85.rds"))
rm(hist)

########## RCP85 - 2040
F1 <- data %>% filter(ClimateGroup == "2040's")
F1 <- F1 %>% filter(year>=2025 & year<=2050)
F1$ClimateGroup <- "2026-2050"
saveRDS(F1, paste0(out_dir, "CMPOP_F1_85.rds"))
rm(F1)

########## RCP85 - 2060
F2 <- data %>% filter(ClimateGroup == "2060's")
F2 <- F2 %>% filter(year>=2051 & year<=2075)
F2$ClimateGroup <- "2051-2075"
saveRDS(F2, paste0(out_dir, "CMPOP_F2_85.rds"))
rm(F2)

########## RCP85 - 2080
F3 <- data %>% filter(ClimateGroup == "2080's")
F3 <- F3 %>% filter(year>=2076)
F3$ClimateGroup <- "2076-2095"
saveRDS(F3, paste0(out_dir, "CMPOP_F3_85.rds"))
rm(F3, data)

###################################

# Merge

rm(list=ls())
.libPaths("/data/hydro/R_libs35")
.libPaths()
library(data.table)
library(dplyr)
options(digit=9)
options(digits=9)

in_dir <- "/data/hydro/users/Hossein/codling_moth_new/local/processed/"
out_dir <- in_dir

new_time_periods <- c("Historical", "2026-2050", "2051-2075", "2076-2095")

CMPOP_F1_45 <- data.table(readRDS(paste0(in_dir, "CMPOP_F1_45.rds")))
CMPOP_F2_45 <- data.table(readRDS(paste0(in_dir, "CMPOP_F2_45.rds")))

CMPOP_rcp45 <- rbind(CMPOP_F1_45, CMPOP_F2_45)
rm(CMPOP_F2_45, CMPOP_F1_45)

CMPOP_F3_45 <- data.table(readRDS(paste0(in_dir, "CMPOP_F3_45.rds")))
CMPOP_rcp45 <- rbind(CMPOP_rcp45, CMPOP_F3_45)
rm(CMPOP_F3_45)

CMPOP_hist_45 <- data.table(readRDS(paste0(in_dir, "CMPOP_hist_45.rds")))
CMPOP_rcp45 <- rbind(CMPOP_hist_45, CMPOP_rcp45)
rm(CMPOP_hist_45)

CMPOP_rcp45$ClimateGroup <- factor(CMPOP_rcp45$ClimateGroup, levels = new_time_periods, order=T)
saveRDS(CMPOP_rcp45, paste0(out_dir, "CMPOP_rcp45.rds"))
rm(CMPOP_rcp45)

#################### RCP 85
rm(list=ls())
.libPaths("/data/hydro/R_libs35")
.libPaths()
library(data.table)
library(dplyr)
options(digit=9)
options(digits=9)

in_dir <- "/data/hydro/users/Hossein/codling_moth_new/local/processed/"
out_dir <- in_dir
new_time_periods <- c("Historical", "2026-2050", "2051-2075", "2076-2095")

CMPOP_F1_85 <- data.table(readRDS(paste0(in_dir, "CMPOP_F1_85.rds")))
CMPOP_F2_85 <- data.table(readRDS(paste0(in_dir, "CMPOP_F2_85.rds")))

CMPOP_rcp85 <- rbind(CMPOP_F1_85, CMPOP_F2_85)
rm(CMPOP_F2_85, CMPOP_F1_85)

CMPOP_F3_85 <- data.table(readRDS(paste0(in_dir, "CMPOP_F3_85.rds")))
CMPOP_rcp85 <- rbind(CMPOP_rcp85, CMPOP_F3_85)
rm(CMPOP_F3_85)

CMPOP_hist_85 <- data.table(readRDS(paste0(in_dir, "CMPOP_hist_85.rds")))
CMPOP_rcp85 <- rbind(CMPOP_hist_85, CMPOP_rcp85)
rm(CMPOP_hist_85)

CMPOP_rcp85$ClimateGroup <- factor(CMPOP_rcp85$ClimateGroup, levels = new_time_periods, order=T)

saveRDS(CMPOP_rcp85, paste0(out_dir, "CMPOP_rcp85.rds"))
rm(CMPOP_rcp85)


