rm(list=ls())
library(data.table)
library(rgdal)
library(dplyr)
library(sp)
library(sf)
library(foreign)

data_dir <- paste0("/Users/hn/Downloads/Grant_2018_NDVI_ShapeFile/")


WSDACrop_all <- readOGR(paste0(data_dir, "flatten_finallyyy.shp"),
                        layer = "flatten_finallyyy",
                        GDAL1_integer64_policy = TRUE)

