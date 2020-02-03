rm(list=ls())
library(data.table)
library(rgdal)
library(dplyr)
library(sp)
# library(sf)
library(foreign)

source_1 = "/Users/hn/Documents/GitHub/Ag/remote_sensing/remote_core.R"
source(source_1)
options(digits=9)
options(digit=9)
##########
########## Directories
##########
data_dir <- paste0("/Users/hn/Desktop/Desktop/Ag/", 
                   "check_point/remote_sensing/", 
                   "2012_2018_true_shapefiles/")

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

dim(WSDACrop_2018@data)
dim(WSDACrop_2017@data)
dim(WSDACrop_2016@data)
dim(WSDACrop_2015@data)
dim(WSDACrop_2014@data)
dim(WSDACrop_2013@data)
dim(WSDACrop_2012@data)

#######################################################################
######
######       Change names to be consistent 
######
#######################################################################

setnames(WSDACrop_2012@data, old=c("Rt1CrpT"), new=c("RtCrpTy"))

setnames(WSDACrop_2014@data, old=c("Rt1CrpT", "LstSrvy"), 
                             new=c("RtCrpTy", "LstSrvD"))

setnames(WSDACrop_2015@data, old=c("SHAPE_A", "SHAPE_L"), 
                             new=c("Shap_Ar", "Shp_Lng"))

setnames(WSDACrop_2016@data, old=c("SHAPE_A", "SHAPE_L"), 
                             new=c("Shap_Ar", "Shp_Lng"))

setnames(WSDACrop_2017@data, old=c("SHAPE_A", "SHAPE_L", "RttnCrT"), 
                            new=c("Shap_Ar", "Shp_Lng", "RtCrpTy"))

setnames(WSDACrop_2018@data, old=c("RttnCrp"), new=c("RtCrpTy"))
#######################################################################
######
######       pick correct year
######
#######################################################################
WSDACrop_2018 <- pick_correct_year(WSDACrop_2018, year=2018)
WSDACrop_2017 <- pick_correct_year(WSDACrop_2017, year=2017)
WSDACrop_2016 <- pick_correct_year(WSDACrop_2016, year=2016)
WSDACrop_2015 <- pick_correct_year(WSDACrop_2015, year=2015)
WSDACrop_2014 <- pick_correct_year(WSDACrop_2014, year=2014)
WSDACrop_2013 <- pick_correct_year(WSDACrop_2013, year=2013)
WSDACrop_2012 <- pick_correct_year(WSDACrop_2012, year=2012)

dim(WSDACrop_2018@data)
dim(WSDACrop_2017@data)
dim(WSDACrop_2016@data)
dim(WSDACrop_2015@data)
dim(WSDACrop_2014@data)
dim(WSDACrop_2013@data)
dim(WSDACrop_2012@data)
#######################################################################
######
######       pick proper cols (columns that are in common among all)
######
#######################################################################
WSDACrop_2018 <- pick_proper_cols_w_notes(WSDACrop_2018)
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

# WSDACrop_2018 <- transfer_projection_to_lat_long(WSDACrop_2018)
# WSDACrop_2017 <- transfer_projection_to_lat_long(WSDACrop_2017)
# WSDACrop_2016 <- transfer_projection_to_lat_long(WSDACrop_2016)
# WSDACrop_2015 <- transfer_projection_to_lat_long(WSDACrop_2015)
# WSDACrop_2014 <- transfer_projection_to_lat_long(WSDACrop_2014)
# WSDACrop_2013 <- transfer_projection_to_lat_long(WSDACrop_2013)
# WSDACrop_2012 <- transfer_projection_to_lat_long(WSDACrop_2012)

WSDACrop_2012_2018_no_2017 <- rbind(WSDACrop_2012, WSDACrop_2013,
                                    WSDACrop_2014, WSDACrop_2015,
                                    WSDACrop_2016,
                                    WSDACrop_2018)

#######################################################################
######
######       Drop Notes column (does not exist in 2017)
######
#######################################################################
WSDACrop_2018 <- pick_proper_cols_no_notes(WSDACrop_2018)
WSDACrop_2017 <- pick_proper_cols_no_notes(WSDACrop_2017)
WSDACrop_2016 <- pick_proper_cols_no_notes(WSDACrop_2016)
WSDACrop_2015 <- pick_proper_cols_no_notes(WSDACrop_2015)
WSDACrop_2014 <- pick_proper_cols_no_notes(WSDACrop_2014)
WSDACrop_2013 <- pick_proper_cols_no_notes(WSDACrop_2013)
WSDACrop_2012 <- pick_proper_cols_no_notes(WSDACrop_2012)

WSDACrop_2012_2018 <- rbind(WSDACrop_2012, WSDACrop_2013,
                            WSDACrop_2014, WSDACrop_2015,
                            WSDACrop_2016, WSDACrop_2017,
                            WSDACrop_2018)

write_dir <- paste0(data_dir, "weird_projections")
if (dir.exists(file.path(write_dir)) == F){
  dir.create(path=file.path(write_dir), recursive=T)
}

writeOGR(obj = WSDACrop_2012_2018, 
         dsn = paste0(write_dir, "/WSDACrop_2012_2018/"), 
         layer="WSDACrop_2012_2018", 
         driver="ESRI Shapefile")

writeOGR(obj = WSDACrop_2012_2018_no_2017, 
         dsn = paste0(write_dir, "/WSDACrop_2012_2018_no_2017/"), 
         layer="WSDACrop_2012_2018_no_2017", 
         driver="ESRI Shapefile")

########################################################################

double_dbl_2018 <- readOGR(paste0("/Users/hn/Desktop/Desktop/", 
                                  "Ag/check_point/remote_sensing/", 
                                  "filtered_shape_files/double_dbl_2018/", 
                                  "double_dbl_2018.shp"),
                        layer = "double_dbl_2018", 
                        GDAL1_integer64_policy = TRUE)

double_dbl_2018 <- transfer_projection_to_lat_long(double_dbl_2018)

writeOGR(obj = double_dbl_2018, 
         dsn = paste0("/Users/hn/Desktop/Desktop/", 
                      "Ag/check_point/remote_sensing/", 
                      "filtered_shape_files/double_dbl_2018"), 
         layer="double_dbl_2018", 
         driver="ESRI Shapefile")

