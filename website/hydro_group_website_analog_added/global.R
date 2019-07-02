

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

data_dir = "/data/codmoth_data/"
d = data.table(readRDS(paste0(data_dir,"/combinedData.rds")))
# ordering time frame levels 
d$timeFrame <-as.factor(d$timeFrame)
d$timeFrame <- factor(d$timeFrame, levels = levels(d$timeFrame)[c(4,1,2,3)])

d_rcp45 = data.table(readRDS(paste0(data_dir,"/combinedData_rcp45.rds")))
names(d_rcp45)[names(d_rcp45) == "ClimateGroup"] = "timeFrame"
d_rcp45$location = paste0(d_rcp45$latitude, "_", d_rcp45$longitude)

d1 <- data.table(readRDS(paste0(data_dir, "/subData.rds")))
d1$month = as.factor(d1$month)
levels(d1$month) = c("January", "February", "March", 
                     "April", "May", "June", 
                     "July", "August", "September", 
                     "October", "November", "December")

d1$location = paste0(d1$latitude, "_", d1$longitude)

d1_rcp45 <- data.table(readRDS(paste0(data_dir, "/subData_rcp45.rds")))
d1_rcp45$month = as.factor(d1_rcp45$month)
levels(d1_rcp45$month) = c("January", "February", "March",
                           "April", "May", "June", 
                           "July", "August", "September", 
                           "October", "November", "December")

d1_rcp45$location = paste0(d1_rcp45$latitude, "_", d1_rcp45$longitude)

RdBu_reverse <- rev(brewer.pal(11, "RdBu"))
head(d1)

diap <- data.table(readRDS(paste0(data_dir, "/diapause_map_data1.rds")))
diap_rcp45 <- data.table(readRDS(paste0(data_dir, "/diapause_map_data1_rcp45.rds")))

# bloom <- data.table(readRDS(paste0(data_dir, "/bloom_data.rds")))
# bloom_rcp45 <- data.table(readRDS(paste0(data_dir, "/bloom_data_rcp45.rds")))

bloom <- data.table(readRDS(paste0(data_dir, "/bloom_rcp85_50_new.rds")))
bloom_rcp45 <- data.table(readRDS(paste0(data_dir, "/bloom_rcp45_50_new.rds")))

print (colnames(bloom_rcp45))
##########################
bloom_rcp85_100 <- data.table(readRDS(paste0(data_dir, "/bloom_rcp85_100_new.rds")))
bloom_rcp45_100 <- data.table(readRDS(paste0(data_dir, "/bloom_rcp45_100_new.rds")))

bloom_rcp85_95 <- data.table(readRDS(paste0(data_dir, "/bloom_rcp85_95_new.rds")))
bloom_rcp45_95 <- data.table(readRDS(paste0(data_dir, "/bloom_rcp45_95_new.rds")))

bloom_rcp85_50 <- data.table(readRDS(paste0(data_dir, "/bloom_rcp85_50_new.rds")))
bloom_rcp45_50 <- data.table(readRDS(paste0(data_dir, "/bloom_rcp45_50_new.rds")))

######################################
###################################### clear above
######################################
##########################
########################## For Analog Map
##########################

#########################################################
# read county shapefile
shapefile_dir <- "/data/codmoth_data/analog/tl_2017_us_county/"
counties <- rgdal::readOGR(dsn=path.expand(shapefile_dir), layer = "tl_2017_us_county")
# counties <- rgdal::readOGR( paste0(shapefile_dir, "/tl_2017_us_county.shp"),
#                              layer = "tl_2017_us_county", GDAL1_integer64_policy = TRUE)

# Extract just the three states OR: 41, WA:53, ID: 16
counties <- counties[counties@data$STATEFP %in% c("16", "41", "53"), ]

## counties <- rmapshaper::ms_simplify(counties)

# Compute states like so, to put border around states
states <- aggregate(counties[, "STATEFP"], by = list(ID = counties@data$STATEFP), 
                    FUN = unique, dissolve = T)

interest_counties <- c("16027", "53001", "53021", "53071",
                       "41021", "53005", "53025", "53077", 
                       "41027", "53007", "53037",  
                       "41049", "53013", "53039", 
                       "41059", "53017", "53047")
counties <- counties[counties@data$GEOID %in% interest_counties, ]

################################################################################


analog_param_dir <- "/home/hnoorazar/ShinyApps/hydro_group_website/params/"
st_cnty_names <- read.csv(paste0(analog_param_dir, "17_counties_fips_unique.csv"),
                          header=T,
                          as.is=T) %>% data.table()
print (st_cnty_names)

# Analog Plot Menu variables on pop-up page
emissions <- c("RCP 8.5" = "rcp85",
               "RCP 4.5" = "rcp45")

climate_models <- c("Select a model" = "NULL" ,
                    "bcc-csm1-1-m" = "bcc_csm1_1_m",
                    "BNU-ESM" = "BNU_ESM", 
                    "CanESM2" = "CanESM2", 
                    "CNRM-CM5"= "CNRM_CM5",
                    "GFDL-ESM2G" = "GFDL_ESM2G",
                    "GFDL-ESM2M" = "GFDL_ESM2M")

time_periods <- c("2026-2050" = "F1",
                  "2051-2075" = "F2",
                  "2076-2095" = "F3")





