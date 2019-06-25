library(data.table)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(maps)
library(rgdal)    # for readOGR and others
library(sp)       # for spatial objects
library(leaflet)  # for interactive maps (NOT leafletR here)
library(dplyr)    # for working with data frames
library(ggplot2)  # for plotting
library(reshape2)
library(RColorBrewer)
# library(plotly)
# library(Hmisc)

data_dir = "/data/codmoth_data/"
d = data.table(readRDS(paste0(data_dir,"/combinedData.rds")))
# ordering time frame levels 
d$timeFrame <-as.factor(d$timeFrame)
d$timeFrame <- factor(d$timeFrame, levels = levels(d$timeFrame)[c(4,1,2,3)])

d_rcp45 = data.table(readRDS(paste0(data_dir,"/combinedData_rcp45.rds")))
names(d_rcp45)[names(d_rcp45) == "ClimateGroup"] = "timeFrame"
d_rcp45$location = paste0(d_rcp45$latitude, "_", d_rcp45$longitude)


d1 <- data.table(readRDS(paste0(data_dir, "/subData.rds")))
d1$month = as.factor(d1$month)
levels(d1$month) = c("January", "February", "March", 
                     "April", "May", "June", 
                     "July", "August", "September", 
                     "October", "November", "December")

d1$location = paste0(d1$latitude, "_", d1$longitude)

d1_rcp45 <- data.table(readRDS(paste0(data_dir, "/subData_rcp45.rds")))
d1_rcp45$month = as.factor(d1_rcp45$month)
levels(d1_rcp45$month) = c("January", "February", "March",
                           "April", "May", "June", 
                           "July", "August", "September", 
                           "October", "November", "December")

d1_rcp45$location = paste0(d1_rcp45$latitude, "_", d1_rcp45$longitude)


RdBu_reverse <- rev(brewer.pal(11, "RdBu"))
head(d1)

diap <- data.table(readRDS(paste0(data_dir, "/diapause_map_data1.rds")))
diap_rcp45 <- data.table(readRDS(paste0(data_dir, "/diapause_map_data1_rcp45.rds")))

# bloom <- data.table(readRDS(paste0(data_dir, "/bloom_data.rds")))
# bloom_rcp45 <- data.table(readRDS(paste0(data_dir, "/bloom_data_rcp45.rds")))

bloom <- data.table(readRDS(paste0(data_dir, "/bloom_rcp85_50_new.rds")))
bloom_rcp45 <- data.table(readRDS(paste0(data_dir, "/bloom_rcp45_50_new.rds")))

print (colnames(bloom_rcp45))
##########################

bloom_rcp85_100 <- data.table(readRDS(paste0(data_dir, "/bloom_rcp85_100_new.rds")))
bloom_rcp45_100 <- data.table(readRDS(paste0(data_dir, "/bloom_rcp45_100_new.rds")))

bloom_rcp85_95 <- data.table(readRDS(paste0(data_dir, "/bloom_rcp85_95_new.rds")))
bloom_rcp45_95 <- data.table(readRDS(paste0(data_dir, "/bloom_rcp45_95_new.rds")))

bloom_rcp85_50 <- data.table(readRDS(paste0(data_dir, "/bloom_rcp85_50_new.rds")))
bloom_rcp45_50 <- data.table(readRDS(paste0(data_dir, "/bloom_rcp45_50_new.rds")))

##########################
########################## For Analog Map
##########################

# Analog Plot Menu variables on pop-up page
emissions <- c("RCP 8.5" = "rcp85",
               "RCP 4.5" = "rcp45")

climate_models <- c("bcc-csm1-1-m" = "bcc",
                    "BNU-ESM" = "BNU", 
                    "CanESM2" = "Can", 
                    "CNRM-CM5"= "CNRM",
                    "GFDL-ESM2G" = "GFG",
                    "GFDL-ESM2M" = "GFM")

time_periods <- c("2026-2050" = "F1",
                  "2051-2075" = "F2",
                  "2076-2095" = "F3")





