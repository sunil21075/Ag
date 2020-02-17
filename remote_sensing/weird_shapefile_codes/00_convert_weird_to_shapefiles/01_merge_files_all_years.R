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
data_dir <- paste0("/Users/hn/Documents/01_research_data/", 
                   "Ag_check_point/remote_sensing/", 
                   "01_true_shapefiles_separate_years/")

WSDACrop_2012 <- readOGR(paste0(data_dir, 
                                "WSDACrop_2012/WSDACrop_2012.shp"),
                        layer = "WSDACrop_2012", 
                        GDAL1_integer64_policy = TRUE)

WSDACrop_2013 <- readOGR(paste0(data_dir, 
                                "WSDACrop_2013/WSDACrop_2013.shp"),
                        layer = "WSDACrop_2013", 
                        GDAL1_integer64_policy = TRUE)

WSDACrop_2014 <- readOGR(paste0(data_dir, 
                                "WSDACrop_2014/WSDACrop_2014.shp"),
                        layer = "WSDACrop_2014", 
                        GDAL1_integer64_policy = TRUE)

WSDACrop_2015 <- readOGR(paste0(data_dir, 
                                "WSDACrop_2015/WSDACrop_2015.shp"),
                        layer = "WSDACrop_2015", 
                        GDAL1_integer64_policy = TRUE)

WSDACrop_2016 <- readOGR(paste0(data_dir, 
                                "WSDACrop_2016/WSDACrop_2016.shp"),
                        layer = "WSDACrop_2016", 
                        GDAL1_integer64_policy = TRUE)

WSDACrop_2017 <- readOGR(paste0(data_dir, 
                                "WSDACrop_2017/WSDACrop_2017.shp"),
                        layer = "WSDACrop_2017", 
                        GDAL1_integer64_policy = TRUE)

WSDACrop_2018 <- readOGR(paste0(data_dir, 
                                "WSDACrop_2018/WSDACrop_2018.shp"),
                        layer = "WSDACrop_2018", 
                        GDAL1_integer64_policy = TRUE)

WSDACrop_2017@data$Notes <- "No Notes 2017"

dim(WSDACrop_2018@data)
dim(WSDACrop_2017@data)
dim(WSDACrop_2016@data)
dim(WSDACrop_2015@data)
dim(WSDACrop_2014@data)
dim(WSDACrop_2013@data)
dim(WSDACrop_2012@data)

head(WSDACrop_2018@data, 2)
head(WSDACrop_2012@data, 2)
WSDACrop_2018 <- pick_proper_cols_w_notes(WSDACrop_2018)
WSDACrop_2017 <- pick_proper_cols_w_notes(WSDACrop_2017)
WSDACrop_2016 <- pick_proper_cols_w_notes(WSDACrop_2016)
WSDACrop_2015 <- pick_proper_cols_w_notes(WSDACrop_2015)
WSDACrop_2014 <- pick_proper_cols_w_notes(WSDACrop_2014)
WSDACrop_2013 <- pick_proper_cols_w_notes(WSDACrop_2013)
WSDACrop_2012 <- pick_proper_cols_w_notes(WSDACrop_2012)
dim(WSDACrop_2018@data)
dim(WSDACrop_2016@data)
dim(WSDACrop_2015@data)
dim(WSDACrop_2014@data)
dim(WSDACrop_2013@data)
dim(WSDACrop_2012@data)

## Add column year to indicate which shapefile they belong to
WSDACrop_2012@data$year <- paste0("2012_shapeFile")
WSDACrop_2013@data$year <- paste0("2013_shapeFile")
WSDACrop_2014@data$year <- paste0("2014_shapeFile")
WSDACrop_2015@data$year <- paste0("2015_shapeFile")
WSDACrop_2016@data$year <- paste0("2016_shapeFile")
WSDACrop_2017@data$year <- paste0("2017_shapeFile")
WSDACrop_2018@data$year <- paste0("2018_shapeFile")


head(WSDACrop_2018@data, 2)
head(WSDACrop_2012@data, 2)

WSDACrop_2012_2018 <- rbind(WSDACrop_2012, WSDACrop_2013,
                            WSDACrop_2014, WSDACrop_2015,
                            WSDACrop_2016, WSDACrop_2017,
                            WSDACrop_2018)

write_dir <- paste0("/Users/hn/Documents/01_research_data/", 
                    "Ag_check_point/remote_sensing/02_2012_2018_all_years/weird_projections/")
if (dir.exists(file.path(write_dir)) == F){
  dir.create(path=file.path(write_dir), recursive=T)
}


writeOGR(obj = WSDACrop_2012_2018, 
         dsn = paste0(write_dir), 
         layer="WSDACrop_2012_2018_weird_proj_all_years", 
         driver="ESRI Shapefile")


rm(WSDACrop_2012_2018)




