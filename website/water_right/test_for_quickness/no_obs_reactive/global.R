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

######################################################
# RD <- c("1916-06-30", "1884-10-30", 
#         "1905-05-10", "1903-05-10",
#         "1902-05-10", "1974-08-02",
#         "1933-08-25", "1901-06-30", 
#         "2010-07-30", "2009-07-30")

# lat <- c(47.10483, 47.10483, 47.10483,
#          47.10483, 47.10483, 47.10483,
#          47.33486, 47.33486, 47.33486, 
#          47.33486)

# long <- c(-120.8522, -121.0577,
#           -121.1509,-121.2570, -121.3508,
#           -121.4569,

#           -120.8522, -121.0577,
#           -121.1509,-121.2570)

# WRS <- c("surfaceWater", "surfaceWater", "surfaceWater", 
#          "surfaceWater", "surfaceWater", "surfaceWater", 
#          "groundwater", "groundwater", "groundwater",
#          "groundwater")


# spatial_wtr_right = data.table(right_date = RD,
#                                lat = lat,
#                                long = long,
#                                WaRecRCWCl = WRS
#                                )
# spatial_wtr_right$location <- paste0(spatial_wtr_right$lat, spatial_wtr_right$long)
# spatial_wtr_right$right_date <- as.Date(spatial_wtr_right$right_date)

# spatial_wtr_right$popup <- 1

# spatial_wtr_right$colorr <- "#ffff00"

# spatial_wtr_right_surface <- spatial_wtr_right %>% 
#                              filter(WaRecRCWCl == "surfaceWater") %>%
#                              data.table()

# spatial_wtr_right_ground <- spatial_wtr_right %>% 
#                              filter(WaRecRCWCl == "groundwater") %>%
#                              data.table()

# spatial_wtr_right_both <- spatial_wtr_right %>% data.table()

d <- "/Users/hn/Desktop/test_for_quickness/data/water_right_attributes.rds"
spatial_wtr_right <- readRDS(d) %>% data.table()
spatial_wtr_right$color <- "#ffff00"

all_basins <- sort(unique(spatial_wtr_right$county_type))
state.name <- c("Washington", "Oregon")

subbasins <- c("Ahtanum Creek", 
               "Lmumu-Burbank",
               "Lower Yakima tributaries",
               "tributaries", 
               "Satus Creek",
               "Taneum-Manastash",
               "Toppenish Creek",
               "Wilson-Cherry")


