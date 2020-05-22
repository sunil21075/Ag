rm(list=ls())
library(data.table)
library(rgdal)
library(dplyr)
library(sp)
# library(sf)
library(foreign)

source_1 = "/Users/hn/Documents/00_GitHub/Ag/remote_sensing/remote_core.R"
source(source_1)
options(digits=9)
options(digit=9)

##########
########## Directories
##########

data_dir <- paste0("/Users/hn/Documents/01_research_data/remote_sensing/", 
                   "00_shapeFiles/01_not_correct_years/", 
                   "01_true_shapefiles_separate_years/WSDACrop_2018/")


needed_counties <- c("Skagit", "Snohomish", "Whatcom")


WSDACrop_2018 <- readOGR(paste0(data_dir, "WSDACrop_2018.shp"),
                        layer = "WSDACrop_2018", 
                        GDAL1_integer64_policy = TRUE)


WSDACrop_2018 <- WSDACrop_2018@data
WSDACrop_2018 <- WSDACrop_2018 %>% 
                 filter(county %in% needed_counties)%>% 
                 data.table()

write.csv(WSDACrop_2018, 
            file = paste0("/Users/hn/Documents/01_research_data/", 
                          "remote_sensing/files_for_Kirti/Skagit_Snohomish_Whatcom_2018.csv"), 
            row.names = FALSE)


WSDACrop_2018 <- WSDACrop_2018[grepl('2018', WSDACrop_2018$LstSrvD), ]
WSDACrop_2018$year <- 2018



