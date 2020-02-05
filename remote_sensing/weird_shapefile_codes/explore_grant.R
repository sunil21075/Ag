rm(list=ls())
library(data.table)
library(rgdal)
library(dplyr)
# library(sp) # rgdal appears to load this already
library(foreign)


base_dir <- "/Users/hn/Documents/01_research_data/Ag_check_point/remote_sensing/03_cleaned_shapeFiles/"
weird_dir <- paste0(base_dir, "Grant_2012_2018_weird/")
latLong_dir <- paste0(base_dir, "Grant_2012_2018_LatLong/")

##########################################
#######
#######
#######
##########################################
grant_SF <- rgdal::readOGR(paste0(weird_dir),
                           layer = "Grant_2012_2018_weird", 
                           GDAL1_integer64_policy = TRUE)
