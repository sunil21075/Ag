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
data_dir <- paste0("/Users/hn/Desktop/Desktop/Ag/", 
                   "check_point/remote_sensing/",
                   "2012_2018_true_shapefiles/")

WSDACrop_2018_dir <- paste0(data_dir, "/WSDACrop_2018/")

WSDACrop_2018 <- readOGR(paste0(WSDACrop_2018_dir, "/WSDACrop_2018.shp"),
                         layer = "WSDACrop_2018", 
                         GDAL1_integer64_policy = TRUE)

doublecrop_2018_double <- WSDACrop_2018[grepl('double',WSDACrop_2018$Notes), ]
doublecrop_2018_dbl <- WSDACrop_2018[grepl('dbl',WSDACrop_2018$Notes), ]

double_dbl_2018 <- rbind(doublecrop_2018_double, doublecrop_2018_dbl)

write_dir <- paste0("/Users/hn/Desktop/Desktop/", 
                    "Ag/check_point/remote_sensing/", 
                    "filtered_shape_files")

if (dir.exists(file.path(write_dir)) == F){
  dir.create(path=file.path(write_dir), recursive=T)
}

writeOGR(obj = double_dbl_2018, 
         dsn = paste0(write_dir, "/double_dbl_2018/"), 
         layer="double_dbl_2018", 
         driver="ESRI Shapefile")







