#####################################
# frost data include 2358 locations in it.
# we need to pick up the selected limited locations
# to plot. There are 19 models in the frost data, however, there are 6
# in the bloom data

# Do we want to be consistent and have only 6 models 
# for both bloom and frost? or keep 19 models of frost?
# or we fucking have to comoute blooms for all 19 models?

#
# We could/should create two sets of data for each (of the first two) 
# scenarios above or, we can take care of NAs
# -introduced to data by merging frost and bloom-
# in the plotting functions?

# This shit is getting crazier by minute.
#
#####################################
rm(list=ls())
library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggpubr)

options(digits=9)
options(digit=9)

source_path_1 = "/Users/hn/Documents/GitHub/Ag/chilling/chill_core.R"
source_path_2 = "/Users/hn/Documents/GitHub/Ag/chilling/chill_plot_core.R"
source(source_path_1)
source(source_path_2)

param_dir <- "/Users/hn/Documents/GitHub/Ag/chilling/parameters/"
LOI <- data.table(read.csv(paste0(param_dir, "limited_locations.csv"), as.is=T))

bloom_dir <- "/Users/hn/Desktop/Desktop/Ag/check_point/chilling/frost_bloom/bloom_new_params/for_TS_with_frost/"

bloom_limited_cities <- readRDS(paste0(bloom_dir, "bloom_limited_50Percent.rds"))
bloom_limited_cities$city <- as.character(bloom_limited_cities$city)
# compute medians per location, time_periods

dues <- c("Feb") # "Dec", "Jan",
due <- "Feb"

######## Read bloom to filter the frost data by model
six_models <- c("Observed", "BNU-ESM", "CanESM2", 
                "GFDL-ESM2G", "bcc-csm1-1-m", "CNRM-CM5",
                "GFDL-ESM2M")

for (due in dues){
  #######################################################################################
  # Read Data
  
  frost_dir <- "/Users/hn/Desktop/Desktop/Ag/check_point/chilling/frost_bloom/"
  frost_dir <- paste0(frost_dir, due, "/")

  first_frost <- data.table(readRDS(paste0(frost_dir, "first_frost_till_", due, ".rds")))
  fifth_frost <- data.table(readRDS(paste0(frost_dir, "fifth_frost_till_", due, ".rds")))

  first_frost <- pick_single_cities_by_location(dt=first_frost, city_info=LOI)
  fifth_frost <- pick_single_cities_by_location(dt=fifth_frost, city_info=LOI)

  first_frost <- within(first_frost, remove(location, month))
  fifth_frost <- within(fifth_frost, remove(location, month))

  first_frost <- first_frost %>% filter(city %in% bloom_limited_cities$city)
  fifth_frost <- fifth_frost %>% filter(city %in% bloom_limited_cities$city)

  first_frost <- within(first_frost, remove(location))
  fifth_frost <- within(fifth_frost, remove(location))

  first_frost <- first_frost %>% 
                 filter(time_period != "1950-2005" & model %in% six_models & time_period != "2006-2025" ) %>% 
                 data.table()
  
  fifth_frost <- fifth_frost %>% 
                 filter(time_period != "1950-2005" & model %in% six_models & time_period != "2006-2025" ) %>% 
                 data.table()

  first_frost <- within(first_frost, remove(year, month, day, tmin, model))
  fifth_frost <- within(fifth_frost, remove(year, month, day, tmin, model))

  first_frost$time_period[first_frost$time_period == "2076-2099"] <- "2076-2095"
  fifth_frost$time_period[fifth_frost$time_period == "2076-2099"] <- "2076-2095"

  setcolorder(bloom_limited_cities, c("city", "time_period", "emission", "apple_type", "medDoY"))
  setnames(bloom_limited_cities, old=c("medDoY"), new=c("fifty_perc_DoY"))

  setcolorder(first_frost, c("city", "time_period", "emission", "chill_dayofyear", "extended_DoY"))
  setcolorder(fifth_frost, c("city", "time_period", "emission", "chill_dayofyear", "extended_DoY"))
  
  bloom_limited_cities <- data.table(bloom_limited_cities)
  
  first_frost <- merge(first_frost, bloom_limited_cities, by=c("city", "time_period", "emission"))
  fifth_frost <- merge(fifth_frost, bloom_limited_cities, by=c("city", "time_period", "emission"))
  
}

