#global.R holds the main data processing and setup for the application
#packages
library(corpcor)
library(data.table)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(leaflet)
library(lubridate)
library(maps)
library(maptools)
library(ncdf4)
library(plotly)
library(rbokeh)
library(rgdal)
library(rgeos)
library(RSQLite)
library(shiny)
library(shinyBS)
library(sp)
library(stringr)
library(tidyr)

# Project package
library(cbcct)

#checkpoint for final version (uncomment when done)
#library(checkpoint)
#checkpoint(format(date(), "%Y-%m-%d"))

###########################
# Settings
###########################
#Colors and theming
circle_color = '#981e32'
r = 2000 #radius

#precip_colors <- c("#333333", "#333333", "#333333", "#333333", "#333333")
#gdd_colors <- c("#333333", "#333333", "#333333", "#333333", "#333333")
precip_colors <- c("#1808FF", "#0734E8", "#0578FF", "#07AAE8", "#07BBDD")
gdd_colors <- c("#FF200D", "#E83D0C", "#FF5E00", "#E87D0C", "#FFA70D")
color_scheme <- cbcct::palette3

#today's date
today = as.Date("2016-09-16")
#today = Sys.Date() - 1
#if(hour(Sys.time()) <= 6) today <- today - 1 #if before 6:59am, use yesterday

this_year <- as.numeric(format(today, "%Y"))
last_day_of_year <- as.Date(paste(this_year, "12-31", sep="-"))

#Data location
data_dir = "data/"

#####################
# Initial Data
#####################

future_model <- "CNRM-CM5"
future_rcp <- "rcp85"
mahalanobis_vars <- "all"


#Spatial data
spatial_historical <- readRDS("data/distance/historical.rds")
analogue_number <- 10
analogue_vars <- list(
  "Maximum Temperature" = "temp_max",
  "Minimum Temperature" = "temp_min",
  "Number of Days Above 30C" = "days_above_hi_cutoff",
  "Number of Days Below 0C" = "days_below_low_cutoff",
  "Cumulative GDD" = "cum_gdd",
  "Cumulative Precipitation" = "cum_precip" 
  )

#Years available
future_years = c(2035,2085)
historical_years = seq(1979,2016,1)

###########################################
#Load Initial Data for use in all locations
###########################################

#Locations and Crops
data(crops)
selected_crops <- crops
data(crop_locations)
data(crb_locations)

default_crop_name <- "Alfalfa"
crop_names <- data.table("id" = crops$id, "name" = crops$name)
crop_names <- na.omit(crop_names)$name

irrigation <-read.table("data/CropParamCRB.txt", header=TRUE, sep=",")
irrigation <- data.table(irrigation)
irrigation$lat = substr(irrigation$filename, 12, 19)
irrigation$long = substr(irrigation$filename, 21, 30)

###temporary hack
# avail_locations <- irrigation[, c("lat", "long"), with=FALSE]
# avail_locations <- avail_locations[!duplicated(avail_locations), ]
# avail_locations$filter = paste0(avail_locations$lat, avail_locations$long)
# #print(avail_locations[!duplicated(avail_locations$filter), ])
# crop_locations$filter = paste0(crop_locations$latitude, crop_locations$longitude)
# crop_locations <- subset(crop_locations, filter %in% avail_locations$filter)
###temporary hack


#GEO Layers
states <- readOGR(paste0(data_dir, "geo/states/cb_2014_us_state_20m.shp"), layer = "cb_2014_us_state_20m", verbose = FALSE)
cbStates <- subset(states, states$STUSPS %in% c("WA", "OR", "ID"))
#waCounties <- readOGR(paste0(data_dir, "geo/WACounties/cb_2014_53_cousub_500k.shp"), layer = "cb_2014_53_cousub_500k", verbose = FALSE)
#orCounties <- readOGR(paste0(data_dir, "geo/ORCounties/cb_2014_41_cousub_500k.shp"), layer = "cb_2014_41_cousub_500k", verbose = FALSE)
#idCounties <- readOGR(paste0(data_dir, "geo/IDCounties/cb_2014_16_cousub_500k.shp"), layer = "cb_2014_16_cousub_500k", verbose = FALSE)

#Historical Data for Spatial Analogues
spatial_historical <- readRDS(paste0(data_dir, "/distance/historical.rds"))








