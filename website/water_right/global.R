# Water Rights

library(scales)
library(lattice)
library(jsonlite)
library(raster)

library(data.table)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(maps)
library(rgdal)    # for readOGR and others
library(sp)       # for spatial objects
library(leaflet)  # for interactive maps (NOT leafletR here)
library(dplyr)    # for working with data frames
library(ggplot2)  # for plotting
library(reshape2)
library(RColorBrewer)

############################################################
############################################################
#####
#####       Read Shape files
#####
############################################################
shapefile_dir <- "/data/hnoorazar/water_right/shapefiles/"

##################
##
## all streams
##
##################
all_streams_sp <- rgdal::readOGR(dsn=path.expand(
                                          paste0(shapefile_dir, 
                                                 "all_streams/")),
                                layer = "all_streams")
##################
##
## all basins
##
##################

all_basins_sp <- rgdal::readOGR(dsn=path.expand(
                                        paste0(shapefile_dir, 
                                              "all_basins/")),
                                layer = "all_basins")
##################
##
## all subbasins
##
##################
all_subbasins_sp <- rgdal::readOGR(dsn=path.expand(
                                          paste0(shapefile_dir, 
                                                 "all_subbasins/")),
                                layer = "all_subbasins")

############################################################
#####
#####       Water Right data
#####
############################################################

wtr_right_dir <- "/data/hnoorazar/water_right/"
data_dir <- paste0(wtr_right_dir, "data/")

spatial_wtr_right <- readRDS(paste0(data_dir, 
                                    "water_right_attributes.rds"))
spatial_wtr_right$colorr <- "#ffff00"


# curr_spatial <- spatial_wtr_right
all_basins <- sort(unique(spatial_wtr_right$county_type))

subbasins <- c("Ahtanum Creek", 
               "Lmumu-Burbank",
               "Lower Yakima tributaries",
               "tributaries", 
               "Satus Creek",
               "Taneum-Manastash",
               "Toppenish Creek",
               "Wilson-Cherry")


########################################
######
######       Functions
######
########################################

###########################
######
######   construct map
######
###########################
build_map <- function(data_dt, sub_bas){
  print ('class(sub_bas) from function')
  print (class(sub_bas))
  print (sub_bas)
  print (unique(data_dt$county_type))
  print ("^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
  
  data_dt <- data_dt %>%
             filter(subbasin %in% sub_bas) %>%
             data.table()

  mean_lat <- mean(data_dt$lat) 
  mean_long <- mean(data_dt$long)

  map <- leaflet() %>%
         addTiles(urlTemplate = "http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                  attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>',
                  layerId = "Satellite",
                  options= providerTileOptions(opacity = 0.9)) %>%
         setView(lat = mean_lat, lng = mean_long, zoom = 7) %>%
         addCircleMarkers(data = data_dt, 
                          lng = ~ long, lat = ~lat,
                          label = ~ popup,
                          # layerId = ~ location,
                          radius = 3,
                          color = ~ colorr,
                          stroke  = FALSE,
                          fillOpacity = .95 
                           )
  return(map)
}
