# Lagoon

library(scales)
library(lattice)
# library(ggmap)
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
# library(plotly)
# library(Hmisc)

data_dir = "/data/hnoorazar/codling_moth/"

######################################
###################################### clear above
######################################
##########################
########################## For Analog Map
##########################

#########################################################
# read county shapefile
shapefile_dir <- "/data/hnoorazar/bloom/shape_files/tl_2017_us_county/"
shapefile_dir <- "/data/hnoorazar/bloom/shape_files/tl_2017_us_county_simple/"
counties <- rgdal::readOGR(dsn=path.expand(shapefile_dir), 
                           layer = "tl_2017_us_county")

# Extract just the three states OR: 41, WA:53, ID: 16
counties <- counties[counties@data$STATEFP %in% c("16", "41", "53"), ]

#
# Compute states like so, to put border around states
states <- aggregate(counties[, "STATEFP"], 
                    by = list(ID = counties@data$STATEFP), 
                    FUN = unique, dissolve = T)

interest_counties <- c("16027", "53001", "53021", "53071",
                       "41021", "53005", "53025", "53077", 
                       "41027", "53007", "53037",  
                       "41049", "53013", "53039", 
                       "41059", "53017", "53047")

counties <- counties[counties@data$GEOID %in% interest_counties, ]

spatial_bcf_dir <- "/data/hnoorazar/bloom/just_CM_locs/"
spatial_bcf <- readRDS(paste0(spatial_bcf_dir, "cm_spatial_bcf.rds")) %>% 
               group_by(location, lat, long)

################################################################################


analog_param_dir <- "/data/hnoorazar/bloom/params/"
st_cnty_names <- read.csv(paste0(analog_param_dir, 
                                 "17_counties_fips_unique.csv"),
                          header=T,
                          as.is=T) %>% 
                 data.table()
#############################################
#
# Analog Plot Menu variables on pop-up page
#
#############################################
detail_levels <- c("All Models Analogs" = "all_models", 
                   "More Details" = "more_details")

emissions <- c("RCP 8.5" = "rcp85",
               "RCP 4.5" = "rcp45")

climate_models <- c("BNU-ESM" = "BNU_ESM", 
                    "CanESM2" = "CanESM2",
                    "GFDL-ESM2G" = "GFDL_ESM2G",
                    "CNRM-CM5"= "CNRM_CM5",
                    "bcc-csm1-1-m" = "bcc_csm1_1_m",
                    "GFDL-ESM2M" = "GFDL_ESM2M")

time_periods <- c("2026-2050" = "F1",
                  "2051-2075" = "F2",
                  "2076-2095" = "F3")

######################################################



