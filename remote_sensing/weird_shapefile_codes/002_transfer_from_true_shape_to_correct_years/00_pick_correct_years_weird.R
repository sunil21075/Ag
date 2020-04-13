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
                   "/remote_sensing/00_shapeFiles/01_not_correct_years/", 
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

dim(WSDACrop_2018@data)
dim(WSDACrop_2017@data)
dim(WSDACrop_2016@data)
dim(WSDACrop_2015@data)
dim(WSDACrop_2014@data)
dim(WSDACrop_2013@data)
dim(WSDACrop_2012@data)

#######################################################################
######
######       Change names to be consistent - Moved to 00_convert to ttue shapefile
######
#######################################################################
# setnames(WSDACrop_2012@data, old=c("Rt1CrpT", "County"), new=c("RtCrpTy", "county"))

# setnames(WSDACrop_2013@data, old=c("County"), new=c("county"))

# setnames(WSDACrop_2014@data, old=c("Rt1CrpT", "LstSrvy", "County"), 
#                              new=c("RtCrpTy", "LstSrvD", "county"))

# setnames(WSDACrop_2015@data, old=c("SHAPE_A", "SHAPE_L", "County"), 
#                              new=c("Shap_Ar", "Shp_Lng", "county"))

# setnames(WSDACrop_2016@data, old=c("SHAPE_A", "SHAPE_L", "County"), 
#                              new=c("Shap_Ar", "Shp_Lng", "county"))

# setnames(WSDACrop_2017@data, old=c("SHAPE_A", "SHAPE_L", "RttnCrT", "County"), 
#                              new=c("Shap_Ar", "Shp_Lng", "RtCrpTy", "county"))

# setnames(WSDACrop_2018@data, old=c("RttnCrp", "County"), new=c("RtCrpTy", "county"))
#######################################################################
######
######       pick correct year
######
#######################################################################
head(WSDACrop_2018@data, 2)
head(WSDACrop_2012@data, 2)

WSDACrop_2018 <- pick_correct_year(WSDACrop_2018, year=2018)
WSDACrop_2017 <- pick_correct_year(WSDACrop_2017, year=2017)
WSDACrop_2016 <- pick_correct_year(WSDACrop_2016, year=2016)
WSDACrop_2015 <- pick_correct_year(WSDACrop_2015, year=2015)
WSDACrop_2014 <- pick_correct_year(WSDACrop_2014, year=2014)
WSDACrop_2013 <- pick_correct_year(WSDACrop_2013, year=2013)
WSDACrop_2012 <- pick_correct_year(WSDACrop_2012, year=2012)


write_dir <- paste0("/Users/hn/Documents/01_research_data/remote_sensing/00_shapeFiles", 
                    "/02_correct_years/03_correct_years_separate/weird_projections/")

if (dir.exists(file.path(write_dir)) == F){
  dir.create(path=file.path(write_dir), recursive=T)
}

writeOGR(obj = WSDACrop_2012, 
         dsn = paste0(write_dir, "/WSDACrop_2012/"), 
         layer="WSDACrop_2012", 
         driver="ESRI Shapefile")

writeOGR(obj = WSDACrop_2013, 
         dsn = paste0(write_dir, "/WSDACrop_2013/"), 
         layer="WSDACrop_2013", 
         driver="ESRI Shapefile")

writeOGR(obj = WSDACrop_2014, 
         dsn = paste0(write_dir, "/WSDACrop_2014/"), 
         layer="WSDACrop_2014", 
         driver="ESRI Shapefile")

writeOGR(obj = WSDACrop_2015, 
         dsn = paste0(write_dir, "/WSDACrop_2015/"), 
         layer="WSDACrop_2015", 
         driver="ESRI Shapefile")

writeOGR(obj = WSDACrop_2016, 
         dsn = paste0(write_dir, "/WSDACrop_2016/"), 
         layer="WSDACrop_2016", 
         driver="ESRI Shapefile")

writeOGR(obj = WSDACrop_2017, 
         dsn = paste0(write_dir, "/WSDACrop_2017/"), 
         layer="WSDACrop_2017", 
         driver="ESRI Shapefile")

writeOGR(obj = WSDACrop_2018, 
         dsn = paste0(write_dir, "/WSDACrop_2018/"), 
         layer="WSDACrop_2018", 
         driver="ESRI Shapefile")

#######################################################################
######
######       pick proper cols (columns that are in common among all)
######
#######################################################################

#
#    commented out on March 19th. After adding ID. We do not need the combined version
#


# WSDACrop_2018 <- pick_proper_cols_w_notes(WSDACrop_2018)
# WSDACrop_2017 <- pick_proper_cols_w_notes(WSDACrop_2017)
# WSDACrop_2016 <- pick_proper_cols_w_notes(WSDACrop_2016)
# WSDACrop_2015 <- pick_proper_cols_w_notes(WSDACrop_2015)
# WSDACrop_2014 <- pick_proper_cols_w_notes(WSDACrop_2014)
# WSDACrop_2013 <- pick_proper_cols_w_notes(WSDACrop_2013)
# WSDACrop_2012 <- pick_proper_cols_w_notes(WSDACrop_2012)
# dim(WSDACrop_2018@data)
# dim(WSDACrop_2016@data)
# dim(WSDACrop_2015@data)
# dim(WSDACrop_2014@data)
# dim(WSDACrop_2013@data)
# dim(WSDACrop_2012@data)


# head(WSDACrop_2018@data, 2)
# head(WSDACrop_2012@data, 2)

# WSDACrop_2012_2018 <- rbind(WSDACrop_2012, WSDACrop_2013,
#                             WSDACrop_2014, WSDACrop_2015,
#                             WSDACrop_2016, WSDACrop_2017,
#                             WSDACrop_2018)

# head(WSDACrop_2012_2018@data, 2)

# write_dir <- paste0("/Users/hn/Documents/01_research_data/Ag_check_point/", 
#                     "remote_sensing/03_cleaned_shapeFiles/WSDACrop_2012_2018_weird_projection/")

# if (dir.exists(file.path(write_dir)) == F){
#   dir.create(path=file.path(write_dir), recursive=T)
# }

# writeOGR(obj = WSDACrop_2012_2018, 
#          dsn = paste0(write_dir), 
#          layer="WSDACrop_2012_2018_weird_projection", 
#          driver="ESRI Shapefile")
# rm(WSDACrop_2012_2018)
