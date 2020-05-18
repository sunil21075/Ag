rm(list=ls())
library(data.table)
library(rgdal)
library(dplyr)
library(sp)
# library(sf)
library(foreign)

source_1 = "/Users/hn/Documents/00_GitHub/Ag/remote_sensing/R/remote_core.R"
source(source_1)
options(digits=9)
options(digit=9)

##############################################################################

data_dir <- paste0("/Users/hn/Documents/01_research_data/", 
                   "remote_sensing/00_shapeFiles/02_correct_years/", 
                   "05_filtered_shapefiles/potentials_No_Nass/No_Nass_2016/")

##############################################################################

Grant2016_No_Nass <- readOGR(paste0(data_dir, "/No_Nass_2016.shp"),
                             layer = "No_Nass_2016", 
                             GDAL1_integer64_policy = TRUE)


Grant2016_No_Nass <- Grant2016_No_Nass@data

# filter grant
Grant2016_No_Nass <- Grant2016_No_Nass %>% 
                     filter(county == "Grant") %>% 
                     data.table()