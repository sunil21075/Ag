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
                         "2012_2018_weird_shapefile.gdb"
                         )

# list the layer names in their to read desired layer
ogrListLayers(weird_2012_2018_dir);
gdb <- path.expand(weird_2012_2018_dir)
WSDACrop_2012 <- readOGR(gdb, "WSDACrop_2012")
WSDACrop_2013 <- readOGR(gdb, "WSDACrop_2013")
WSDACrop_2014 <- readOGR(gdb, "WSDACrop_2014")
WSDACrop_2015 <- readOGR(gdb, "WSDACrop_2015")
WSDACrop_2016 <- readOGR(gdb, "WSDACrop_2016")
WSDACrop_2017 <- readOGR(gdb, "WSDACrop_2017")
WSDACrop_2018 <- readOGR(gdb, "WSDACrop_2018")

##########
########## write TRUE shapefiles
##########

write_dir <- paste0(data_dir, "/2012_2018_true_shapefiles")
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

################################################################
#####
#####            Merge if possible
#####
################################################################

# 2015, 2016 and 2018 are kind of mergable:
# 2018 do not have "Organic" column and instead has "CoverCrop"
# We will drop those columns from 2015-2016 and 2018 files respectively
# and then merge, write on disk, for easier future use.
#

WSDACrop_2015 <- WSDACrop_2015[,!(names(WSDACrop_2015) %in% c("Organic"))]
WSDACrop_2016 <- WSDACrop_2016[,!(names(WSDACrop_2016) %in% c("Organic"))]
WSDACrop_2018 <- WSDACrop_2018[,!(names(WSDACrop_2018) %in% c("CoverCrop"))]


setnames(WSDACrop_2015@data, old=c("RotCropType"), new=c("RotCrop"))
setnames(WSDACrop_2016@data, old=c("RotCropType"), new=c("RotCrop"))
setnames(WSDACrop_2018@data, old=c("RotationCrop"), new=c("RotCrop"))

setnames(WSDACrop_2015@data, 
         old=c("SHAPE_Length", "SHAPE_Area"), 
         new=c("Shape_Length", "Shape_Area"))

setnames(WSDACrop_2016@data, 
         old=c("SHAPE_Length", "SHAPE_Area"), 
         new=c("Shape_Length", "Shape_Area"))

WSDACrop_2015_2016_2018 <- rbind(WSDACrop_2015, 
                                 WSDACrop_2016, 
                                 WSDACrop_2018)
########################################################
######
######        2013 and 2014 
######
########################################################
setnames(WSDACrop_2014@data, old=c("Rot1Crop", "Rot1CropType", 
                                   "TotalAcres", "InitialSur", 
                                   "LastSurvey"), 
                             
                             new=c("RotCropGroup", "RotCrop", 
                                   "Acres", "InitialSurveyDate", 
                                   "LastSurveyDate"))

setnames(WSDACrop_2013@data, old=c("RotCropType", "TotalAcres"), 
                             new=c("RotCrop", "Acres"))

ddd <- c("Organic", "RotCropGroup")
WSDACrop_2014 <- WSDACrop_2014[,!(names(WSDACrop_2014) %in% ddd)]
WSDACrop_2013 <- WSDACrop_2013[,!(names(WSDACrop_2013) %in% ddd)]

############### 2012

setnames(WSDACrop_2012@data, 
         old=c("TotalAcres", "Rot1CropGroup", "Rot1CropType"), 
         new=c("Acres", "RotCropGroup", "RotCrop"))

ddd <- c("Organic", "RotCropGroup", "NLCD_Cat")
WSDACrop_2012 <- WSDACrop_2012[,!(names(WSDACrop_2012) %in% ddd)]

#######################################################
#####
#####   2017 does not have the Note column in it.
#####
#######################################################
WSDACrop_2012_2018_no_2017 <- rbind(WSDACrop_2012, 
                                    WSDACrop_2013,
                                    WSDACrop_2014,
                                    WSDACrop_2015, 
                                    WSDACrop_2016,
                                    WSDACrop_2018)

writeOGR(obj = WSDACrop_2012_2018_no_2017, 
         dsn = paste0(write_dir, "/WSDACrop_no_2017/"), 
         layer="WSDACrop_no_2017", 
         driver="ESRI Shapefile")







