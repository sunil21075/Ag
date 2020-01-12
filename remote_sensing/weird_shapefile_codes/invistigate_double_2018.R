rm(list=ls())
library(data.table)
library(rgdal)
library(dplyr)
library(sp)
library(sf)
library(foreign)

##########
########## Directories
##########

remote_dir <- paste0("/Users/hn/Desktop/Desktop/Ag/",
                     "check_point/remote_sensing/")

double_dir <- paste0(remote_dir,
                     "filtered_shape_files/double_dbl_2018/")

double_2018 <- readOGR(paste0(double_dir, "/double_dbl_2018.shp"),
                       layer = "double_dbl_2018", 
                       GDAL1_integer64_policy = TRUE)
