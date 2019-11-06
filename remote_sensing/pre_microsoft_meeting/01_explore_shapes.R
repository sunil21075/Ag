############      packages      #############

library(data.table)
library(rgdal)
library(dplyr)
library(sp)
library(sf)

############      Directories      #############

data_dir <- paste0("/Users/hn/Desktop/Desktop/Ag/", 
                   "check_point/pre_microsoft_meeting/")

shape_file_dir <- paste0(data_dir, "2018WSDACrop.gdb") 

##############################################
############                     #############
############   read fips data    #############

some_counties <- read.csv(paste0(data_dir, 
                                 "counties_to_look_at.csv"),
                          as.is=TRUE)

ogrListLayers(shape_file_dir)
gdb <- path.expand(shape_file_dir)
WSDACrop <- readOGR(gdb, "WSDACrop_2018")

##############################################
############                     #############
############  add LastSurveyYear #############

WSDACrop@data$LastSurveyYear <- substr(WSDACrop@data$LastSurveyDate, 1, 4)

WSDACrop2018 <- WSDACrop[WSDACrop@data$LastSurveyYear == "2018", ]

###############################################
############                      #############
############  subset double crops #############
WSDACrop2018_doublecrop <- subset(WSDACrop2018, RotationCrop != " ")

#################################################
############                        #############
############  subset double crops   #############

WSDACrop2018_doublecrop <- subset(WSDACrop2018, 
                                  RotationCrop != " ")

###############################################
############                      #############
############   subset some crops  #############
some_crops <- c("Alfalfa Hay", "Alfalfa Seed",
                "Alfalfa/Grass Hay", 
                "Apple", "Blueberry", 
                "Cherry", 
                "Grape, Juice", "Grape, Table",
                "Grape, Wine", "Potato")

WSDACrop2018_some_crops <- subset(WSDACrop2018, 
                                  CropType %in% some_crops)

double_crops_true_centroids <- rgeos::gCentroid(WSDACrop2018_doublecrop, 
                                                byid=TRUE)

some_crops_true_centroids <- rgeos::gCentroid(WSDACrop2018_some_crops, 
                                              byid=TRUE)



