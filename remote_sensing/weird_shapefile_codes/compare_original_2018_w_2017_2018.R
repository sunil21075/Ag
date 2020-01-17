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
                   "check_point/remote_sensing/")

weird_2012_2018_dir <- paste0(data_dir, 
                              "2012_2018_weird_shapefile.gdb")

original_2018_dir <- paste0(data_dir,
                            "2018_weird_shape_file/2018WSDACrop.gdb")


ogrListLayers(weird_2012_2018_dir);
weird_2012_2018_gdb <- path.expand(weird_2012_2018_dir)
weird_2018 <- readOGR(weird_2012_2018_gdb, "WSDACrop_2018")

ogrListLayers(original_2018_dir);
original_2018_gdb <- path.expand(original_2018_dir)
original_2018 <- readOGR(original_2018_gdb, "WSDACrop_2018")


