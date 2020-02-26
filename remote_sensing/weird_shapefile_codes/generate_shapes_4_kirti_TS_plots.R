rm(list=ls())
library(data.table)
library(dplyr)
library(foreign)
library(rgdal)
# library(sp) # rgdal appears to load this already


data_dir <- paste0("/Users/hn/Documents/01_research_data/", 
            "Ag_check_point/remote_sensing/00_shapeFiles/", 
            "02_correct_years/04_cleaned_shapeFiles/WSDACrop_2012_2018_lat_long/")

WSDACrop <- rgdal::readOGR(paste0(data_dir, "/WSDACrop_2012_2018_lat_long.shp"),
                           layer = "WSDACrop_2012_2018_lat_long", 
                           GDAL1_integer64_policy = TRUE)
 
WSDACrop_Benton <- WSDACrop[grepl('Benton', WSDACrop$county), ]

WSDACrop_Benton_2012 <- WSDACrop_Benton[WSDACrop_Benton@data$year == 2012, ]
# WSDACrop_Benton_2013 <- WSDACrop_Benton[WSDACrop_Benton@data$year == 2013, ]
# WSDACrop_Benton_2014 <- WSDACrop_Benton[WSDACrop_Benton@data$year == 2014, ]
# WSDACrop_Benton_2015 <- WSDACrop_Benton[WSDACrop_Benton@data$year == 2015, ]
# WSDACrop_Benton_2016 <- WSDACrop_Benton[WSDACrop_Benton@data$year == 2016, ]
# WSDACrop_Benton_2017 <- WSDACrop_Benton[WSDACrop_Benton@data$year == 2017, ]
# WSDACrop_Benton_2018 <- WSDACrop_Benton[WSDACrop_Benton@data$year == 2018, ]

# length(WSDACrop_Benton_2012@polygons)
# length(WSDACrop_Benton_2013@polygons)
# length(WSDACrop_Benton_2014@polygons)
# length(WSDACrop_Benton_2015@polygons)
# length(WSDACrop_Benton_2016@polygons)
# length(WSDACrop_Benton_2017@polygons)
# length(WSDACrop_Benton_2018@polygons)

needed_crops <- c("Alfalfa Hay", "Apple", "Barley", 
                  "Blueberry", "Corn, Sweet", "Hops", "Oat", 
                  "Onion", "Potato", "Timothy", "Wheat")

# Sentinel 2 came to North America after 2015. 
needed_crops %in% WSDACrop_Benton_2015@data$CropTyp
View(sort(unique(WSDACrop_Benton_2012@data$CropTyp)))


write_dir <- paste0("/Users/hn/Documents/01_research_data/Ag_check_point/", 
                    "remote_sensing/00_shapeFiles/02_correct_years/", 
                    "05_filtered_shapefiles/Benton/")

if (dir.exists(file.path(write_dir)) == F){
  dir.create(path=file.path(write_dir), recursive=T)
}

writeOGR(obj = WSDACrop_Benton, 
         dsn = paste0(write_dir, "/Benton_2012_2018"), 
         layer="Benton_2012_2018", 
         driver="ESRI Shapefile")


writeOGR(obj = WSDACrop_Benton_2012, 
         dsn = paste0(write_dir, "/Benton_2012"), 
         layer="Benton_2012", 
         driver="ESRI Shapefile")

writeOGR(obj = WSDACrop_Benton_2013, 
         dsn = paste0(write_dir, "/Benton_2013"), 
         layer="Benton_2013", 
         driver="ESRI Shapefile")

writeOGR(obj = WSDACrop_Benton_2014, 
         dsn = paste0(write_dir, "/Benton_2014"), 
         layer="Benton_2014", 
         driver="ESRI Shapefile")

writeOGR(obj = WSDACrop_Benton_2015, 
         dsn = paste0(write_dir, "/Benton_2015"), 
         layer="Benton_2015", 
         driver="ESRI Shapefile")

writeOGR(obj = WSDACrop_Benton_2016, 
         dsn = paste0(write_dir, "/Benton_2016"), 
         layer="Benton_2016", 
         driver="ESRI Shapefile")

writeOGR(obj = WSDACrop_Benton_2017, 
         dsn = paste0(write_dir, "/Benton_2017"), 
         layer="Benton_2017", 
         driver="ESRI Shapefile")


writeOGR(obj = WSDACrop_Benton_2018, 
         dsn = paste0(write_dir, "/Benton_2018"), 
         layer="Benton_2018", 
         driver="ESRI Shapefile")


###########################################################################################

kirti_shape <- WSDACrop_Benton_2018[WSDACrop_Benton_2018@data$CropTyp %in% needed_crops, ]
rownames(kirti_shape@data) <- 1:dim(kirti_shape@data)[1]

kirti_shape@data$row_no <- rownames(kirti_shape@data)
View(kirti_shape@data)

needed_rows <- c(4, 13, 65, # alfa
                 2, 7, 20,  # apple
                 684,       # barley
                 974, 1046, 1102, # bluebbery
                 854, 904, 960,   # corn
                 402, 413, 529,   # hops
                 # , # oat
                 1585, 1590, 1602, # onion
                 392, 409, 434, # potato 
                 128, 166, 404, # timothy
                 3, 7, 8) # wheat

kirti_shape <- kirti_shape[kirti_shape@data$row_no %in% needed_rows, ]

write_dir <- paste0("/Users/hn/Documents/01_research_data/Ag_check_point/", 
                    "remote_sensing/00_shapeFiles/02_correct_years/", 
                    "05_filtered_shapefiles/Kirti_plot_shapes/")

if (dir.exists(file.path(write_dir)) == F){
  dir.create(path=file.path(write_dir), recursive=T)
}

writeOGR(obj = kirti_shape, 
         dsn = paste0(write_dir, "/kirti_shape_3_fileds_each_plant"), 
         layer="three_fileds_each_plant", 
         driver="ESRI Shapefile")

################ one field of each


alfa <- kirti_shape[kirti_shape@data$row_no == 4, ]
apple <- kirti_shape[kirti_shape@data$row_no == 2, ]
barley <- kirti_shape[kirti_shape@data$row_no == 684, ]
blueberry <- kirti_shape[kirti_shape@data$row_no == 974, ]
corn <- kirti_shape[kirti_shape@data$row_no == 854, ]
hops <- kirti_shape[kirti_shape@data$row_no == 402, ]
# oat <- kirti_shape[kirti_shape@data$row_no == 1455, ]
onion <- kirti_shape[kirti_shape@data$row_no == 1585, ]
potato <- kirti_shape[kirti_shape@data$row_no == 392, ]
timothy <- kirti_shape[kirti_shape@data$row_no == 128, ]
wheat <- kirti_shape[kirti_shape@data$row_no == 3, ]

alfa@data$CropTyp
apple@data$CropTyp
barley@data$CropTyp
blueberry@data$CropTyp
corn@data$CropTyp
hops@data$CropTyp
# oat@data$CropTyp
onion@data$CropTyp
wheat@data$CropTyp


writeOGR(obj = alfa, 
         dsn = paste0(write_dir, "/alfa"), 
         layer="alfa", 
         driver="ESRI Shapefile")

writeOGR(obj = apple, 
         dsn = paste0(write_dir, "/apple"), 
         layer="apple", 
         driver="ESRI Shapefile")

writeOGR(obj = barley, 
         dsn = paste0(write_dir, "/barley"), 
         layer="barley", 
         driver="ESRI Shapefile")

writeOGR(obj = bluebery, 
         dsn = paste0(write_dir, "/bluebery"), 
         layer="bluebery", 
         driver="ESRI Shapefile")

writeOGR(obj = hops, 
         dsn = paste0(write_dir, "/hops"), 
         layer="hops", 
         driver="ESRI Shapefile")

writeOGR(obj = corn, 
         dsn = paste0(write_dir, "/corn"), 
         layer="corn", 
         driver="ESRI Shapefile")

# writeOGR(obj = oat, 
#          dsn = paste0(write_dir, "/oat"), 
#          layer="oat", 
#          driver="ESRI Shapefile")

writeOGR(obj = onion, 
         dsn = paste0(write_dir, "/onion"), 
         layer="onion", 
         driver="ESRI Shapefile")

writeOGR(obj = timothy, 
         dsn = paste0(write_dir, "/timothy"), 
         layer="timothy", 
         driver="ESRI Shapefile")

writeOGR(obj = potato, 
         dsn = paste0(write_dir, "/potato"), 
         layer="potato", 
         driver="ESRI Shapefile")

writeOGR(obj = wheat, 
         dsn = paste0(write_dir, "/wheat"), 
         layer="wheat", 
         driver="ESRI Shapefile")



