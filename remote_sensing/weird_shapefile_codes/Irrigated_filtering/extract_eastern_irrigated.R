rm(list=ls())
library(data.table)
library(rgdal)
library(dplyr)
library(sp)
# library(sf)
library(foreign)

source_1 = "/Users/hn/Documents/00_GitHub/Ag/remote_sensing/R/remote_core.R"
source(source_1)
options(digits=9)
options(digit=9)

# write_dir <- paste0("/Users/hn/Documents/01_research_data/remote_sensing/", 
#                     "00_shapeFiles/0002_final_shapeFiles/0003_Grant_irrigated/")
# if (dir.exists(file.path(write_dir)) == F){
#   dir.create(path=file.path(write_dir), recursive=T)
# }

##########
########## Directories
##########

data_dir <- paste0("/Users/hn/Documents/01_research_data/", 
                   "remote_sensing/00_shapeFiles/01_not_correct_years/", 
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

WSDACrop_2018 <- readOGR(paste0(data_dir, 
                                "WSDACrop_2018/WSDACrop_2018.shp"),
                        layer = "WSDACrop_2018", 
                        GDAL1_integer64_policy = TRUE)

####################################################################################
######
###### Transform projections
######
####################################################################################

WSDACrop_2012 <- transfer_projection_to_lat_long(WSDACrop_2012)
WSDACrop_2013 <- transfer_projection_to_lat_long(WSDACrop_2013)
WSDACrop_2014 <- transfer_projection_to_lat_long(WSDACrop_2014)
WSDACrop_2015 <- transfer_projection_to_lat_long(WSDACrop_2015)
WSDACrop_2016 <- transfer_projection_to_lat_long(WSDACrop_2016)
WSDACrop_2018 <- transfer_projection_to_lat_long(WSDACrop_2018)

nrow(WSDACrop_2018) # 212985
####################################################################################
######
######   Filter irrigated
###### 
####################################################################################


WSDACrop_2012 <- filter_out_non_irrigated_shapefile(WSDACrop_2012)
WSDACrop_2013 <- filter_out_non_irrigated_shapefile(WSDACrop_2013)
WSDACrop_2014 <- filter_out_non_irrigated_shapefile(WSDACrop_2014)
WSDACrop_2015 <- filter_out_non_irrigated_shapefile(WSDACrop_2015)
WSDACrop_2016 <- filter_out_non_irrigated_shapefile(WSDACrop_2016)
WSDACrop_2018 <- filter_out_non_irrigated_shapefile(WSDACrop_2018)
nrow(WSDACrop_2018) # 84717

#################################
#########
#########    Write irrigated shapefiles 
#########
#################################

irrigated_dir <- paste0("/Users/hn/Documents/01_research_data/", 
                        "remote_sensing/00_shapeFiles/0002_final_shapeFiles/", 
                        "0001_irrigated/")


writeOGR(obj = WSDACrop_2012, 
         dsn = paste0(irrigated_dir, "/WSDA_2012_irrigated/"), 
         layer="WSDA_2012_irrigated", 
         driver="ESRI Shapefile")

writeOGR(obj = WSDACrop_2013, 
         dsn = paste0(irrigated_dir, "/WSDA_2013_irrigated/"), 
         layer="WSDA_2013_irrigated", 
         driver="ESRI Shapefile")


writeOGR(obj = WSDACrop_2014, 
         dsn = paste0(irrigated_dir, "/WSDA_2014_irrigated/"), 
         layer="WSDA_2014_irrigated", 
         driver="ESRI Shapefile")


writeOGR(obj = WSDACrop_2015, 
         dsn = paste0(irrigated_dir, "/WSDA_2015_irrigated/"), 
         layer="WSDA_2015_irrigated", 
         driver="ESRI Shapefile")

writeOGR(obj = WSDACrop_2016, 
         dsn = paste0(irrigated_dir, "/WSDA_2016_irrigated/"), 
         layer="WSDA_2016_irrigated", 
         driver="ESRI Shapefile")

writeOGR(obj = WSDACrop_2018, 
         dsn = paste0(irrigated_dir, "/WSDA_2018_irrigated/"), 
         layer="WSDA_2018_irrigated", 
         driver="ESRI Shapefile")


####################################################################################
######
######   Filter Eastern Counties
###### 
####################################################################################

nrow(WSDACrop_2018) # 84717
WSDACrop_2012 <- pick_eastern_counties(WSDACrop_2012)
WSDACrop_2013 <- pick_eastern_counties(WSDACrop_2013)
WSDACrop_2014 <- pick_eastern_counties(WSDACrop_2014)
WSDACrop_2015 <- pick_eastern_counties(WSDACrop_2015)
WSDACrop_2016 <- pick_eastern_counties(WSDACrop_2016)
WSDACrop_2018 <- pick_eastern_counties(WSDACrop_2018)

nrow(WSDACrop_2018) # 76001

###################################
#########
#########    Write irrigated eastern counties 
#########
###################################
irrigated_dir <- paste0("/Users/hn/Documents/01_research_data/", 
                        "remote_sensing/00_shapeFiles/0002_final_shapeFiles/", 
                        "0002_irrigated_eastern/")


writeOGR(obj = WSDACrop_2012, 
         dsn = paste0(irrigated_dir, "/Eastern_Irrigated_2012/"), 
         layer="Eastern_Irrigated_2012", 
         driver="ESRI Shapefile")

writeOGR(obj = WSDACrop_2013, 
         dsn = paste0(irrigated_dir, "/Eastern_Irrigated_2013/"), 
         layer="Eastern_Irrigated_2013", 
         driver="ESRI Shapefile")


writeOGR(obj = WSDACrop_2014, 
         dsn = paste0(irrigated_dir, "/Eastern_Irrigated_2014/"), 
         layer="Eastern_Irrigated_2014", 
         driver="ESRI Shapefile")


writeOGR(obj = WSDACrop_2015, 
         dsn = paste0(irrigated_dir, "/Eastern_Irrigated_2015/"), 
         layer="Eastern_Irrigated_2015", 
         driver="ESRI Shapefile")

writeOGR(obj = WSDACrop_2016, 
         dsn = paste0(irrigated_dir, "/Eastern_Irrigated_2016/"), 
         layer="Eastern_Irrigated_2016", 
         driver="ESRI Shapefile")

writeOGR(obj = WSDACrop_2018, 
         dsn = paste0(irrigated_dir, "/Eastern_Irrigated_2018/"), 
         layer="Eastern_Irrigated_2018", 
         driver="ESRI Shapefile")

####################################################################################
######
######   Filter Grant
###### 
####################################################################################

Grant_2012 <- WSDACrop_2012[grepl('Grant', WSDACrop_2012$county), ]
Grant_2013 <- WSDACrop_2013[grepl('Grant', WSDACrop_2013$county), ]
Grant_2014 <- WSDACrop_2014[grepl('Grant', WSDACrop_2014$county), ]
Grant_2015 <- WSDACrop_2015[grepl('Grant', WSDACrop_2015$county), ]
Grant_2016 <- WSDACrop_2016[grepl('Grant', WSDACrop_2016$county), ]
Grant_2018 <- WSDACrop_2018[grepl('Grant', WSDACrop_2018$county), ]

writeOGR(obj = Grant_2012, 
         dsn = paste0(irrigated_dir, "/Grant_irrigated_2012/"), 
         layer="Grant_irrigated_2012", 
         driver="ESRI Shapefile")

writeOGR(obj = Grant_2013, 
         dsn = paste0(irrigated_dir, "/Grant_irrigated_2013/"), 
         layer="Grant_irrigated_2013", 
         driver="ESRI Shapefile")


writeOGR(obj = Grant_2014, 
         dsn = paste0(irrigated_dir, "/Grant_irrigated_2014/"), 
         layer="Grant_irrigated_2014", 
         driver="ESRI Shapefile")


writeOGR(obj = Grant_2015, 
         dsn = paste0(irrigated_dir, "/Grant_irrigated_2015/"), 
         layer="Grant_irrigated_2015", 
         driver="ESRI Shapefile")

writeOGR(obj = Grant_2016, 
         dsn = paste0(irrigated_dir, "/Grant_irrigated_2016/"), 
         layer="Grant_irrigated_2016", 
         driver="ESRI Shapefile")

writeOGR(obj = Grant_2018, 
         dsn = paste0(irrigated_dir, "/Grant_irrigated_2018/"), 
         layer="Grant_irrigated_2018", 
         driver="ESRI Shapefile")



####################################################################################
######
######   Filter Eastern Counties no Grant
###### 
####################################################################################


nrow(WSDACrop_2018) # 76001
WSDACrop_2012 <- pick_eastern_counties_noGrant(WSDACrop_2012)
WSDACrop_2013 <- pick_eastern_counties_noGrant(WSDACrop_2013)
WSDACrop_2014 <- pick_eastern_counties_noGrant(WSDACrop_2014)
WSDACrop_2015 <- pick_eastern_counties_noGrant(WSDACrop_2015)
WSDACrop_2016 <- pick_eastern_counties_noGrant(WSDACrop_2016)
WSDACrop_2018 <- pick_eastern_counties_noGrant(WSDACrop_2018)

nrow(WSDACrop_2018) # 62209

##################################
#########
#########    Write irrigated eastern counties no Grant
#########
##################################

writeOGR(obj = WSDACrop_2012, 
         dsn = paste0(irrigated_dir, "/Eastern_noGrant_Irrigated_2012/"), 
         layer="Eastern_noGrant_Irrigated_2012", 
         driver="ESRI Shapefile")

writeOGR(obj = WSDACrop_2013, 
         dsn = paste0(irrigated_dir, "/Eastern_noGrant_Irrigated_2013/"), 
         layer="Eastern_noGrant_Irrigated_2013", 
         driver="ESRI Shapefile")


writeOGR(obj = WSDACrop_2014, 
         dsn = paste0(irrigated_dir, "/Eastern_noGrant_Irrigated_2014/"), 
         layer="Eastern_noGrant_Irrigated_2014", 
         driver="ESRI Shapefile")


writeOGR(obj = WSDACrop_2015, 
         dsn = paste0(irrigated_dir, "/Eastern_noGrant_Irrigated_2015/"), 
         layer="Eastern_noGrant_Irrigated_2015", 
         driver="ESRI Shapefile")

writeOGR(obj = WSDACrop_2016, 
         dsn = paste0(irrigated_dir, "/Eastern_noGrant_Irrigated_2016/"), 
         layer="Eastern_noGrant_Irrigated_2016", 
         driver="ESRI Shapefile")

writeOGR(obj = WSDACrop_2018, 
         dsn = paste0(irrigated_dir, "/Eastern_noGrant_Irrigated_2018/"), 
         layer="Eastern_noGrant_Irrigated_2018", 
         driver="ESRI Shapefile")


