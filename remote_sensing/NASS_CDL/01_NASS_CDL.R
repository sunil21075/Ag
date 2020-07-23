rm(list=ls())
library(foreign)
library(data.table)
library(dplyr)
library(rgdal)
library(sp)


download_dir <- paste0("/Users/hn/Documents/01_research_data/", 
                       "remote_sensing/NASS_CDL/download_by_cdlTools/")
WA_2019 <- cdlTools::getCDL(x = 'Washington', year = 2019, location = download_dir, ssl.verifypeer = FALSE)

# you can get counts with table
WA_2018_table <- table(raster::getValues(WA_2018[[1]]))
WA_2018_table

crop_count <- data.frame( crop = updateNamesCDL(names(WA_2018_table)), 
                          WA_2018_table , 
                          stringsAsFactors=FALSE)