# We could/should create two sets of data for each (of the first two) 
# scenarios above or, we can take care of NAs
# -introduced to data by merging frost and bloom-
# in the plotting functions?
#####################################
rm(list=ls())
library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggpubr)

options(digits=9)
options(digit=9)
############################################################
###
###             Computer source
###
############################################################

source_path_1 = "/Users/hn/Documents/GitHub/Ag/bloom/bloom_core.R"
source_path_2 = "/Users/hn/Documents/GitHub/Ag/bloom/bloom_plot_core.R"
source(source_path_1)
source(source_path_2)

in_dir <- "/Users/hn/Desktop/Desktop/Ag/check_point/bloom/03_merge_02_Step/"
plot_dir <- in_dir
############################################################
###
###             Aeolus source
###
############################################################
source_dir <- "/home/hnoorazar/bloom_codes/"
source_1 = paste0(source_dir, "bloom_core.R")
source_2 = paste0(source_dir, "bloom_plot_core.R")
source(source_1)
source(source_2)
############################################################
###
###             Directories source
###

base <- "/data/hydro/users/Hossein/bloom/"
in_dir <- paste0(base, "03_merge_02_Step/")
plot_dir <- paste0(base, "04_bloom_vs_frost_plot/")
start_time <- Sys.time()
#############################################################
###
###      Read data
###

bloom <- readRDS(paste0(in_dir, "fullbloom_50percent_day.rds"))
frost <- readRDS(paste0(in_dir, "first_frost.rds"))
#############################################################
emissions <- c("RCP 4.5", "RCP 8.5")
apple_types <- c("Cripps Pink", "Gala", "Red Deli")

bloom <- pick_obs_and_F(bloom)
frost <- pick_obs_and_F(frost)

# apple, Cherry, Pear; Cherry 14 days shift, Pear 7 days shift
fruit_type <- "apple"
remove_NA <- "yes" 

# shift the bloom days
if (fruit_type == "Cherry"){
   bloom$dayofyear <- bloom$dayofyear-14
   bloom <- bloom %>% filter(dayofyear>=0)
   # apple_types <- c("Cripps Pink") # This is done just for purpose of for loop
   } else if (fruit_type == "Pear"){
    bloom$dayofyear <- bloom$dayofyear-7
    bloom <- bloom %>% filter(dayofyear>=0)
    # apple_types <- c("Cripps Pink") # This is done just for purpose of for loop
}

setnames(bloom, old=c("dayofyear"), new=c("fifty_perc_DoY"))
bloom <- data.table(bloom)

# obs_years <- c(1979:2015)
# future_years <- c(2026:2095)
# complete_1st_frost <- CJ(years, cities, six_models, emission)
# future <- CJ(future_years, cities, emissions, six_models)

####################################################################################
#
#         Threshold stuff
#
chill_limited <- read.csv(paste0(param_dir, "/limited_locations.csv"), as.is=TRUE)
chill_limited$location <- paste0(chill_limited$lat, "_", chill_limited$long)
suppressWarnings({ chill_limited <- within(chill_limited, remove(lat, long))})
chill_limited <- chill_limited %>% filter(city %in% cities) %>% data.table()

thresh_dir <- "/Users/hn/Desktop/Desktop/Ag/check_point/chilling/"
thresh_dt <- readRDS(paste0(thresh_dir, "/sept_summary_comp.rds"))
thresh_dt$year <- thresh_dt$year - 1
thresh_dt <- thresh_dt %>% filter(year >= 2025) %>% data.table()
thresh_dt <- thresh_dt %>% 
                filter(location %in% chill_limited$location # & model %in% all_models_observed
                       ) %>% 
                data.table()

thresh_dt <- merge(thresh_dt, chill_limited, all.x=TRUE)
suppressWarnings({ thresh_dt <- within(thresh_dt, 
                       remove(location, start, sum_J1, sum_F1, sum_M1,
                              sum_A1, sum))})

thresh_dt <- remove_modeled_historical_add_time_period(thresh_dt)
thresh_dt <- remove_F0(thresh_dt)
thresh_dt <- thresh_dt %>% filter(year <= 2094)%>% data.table()

####################################################################################
# compute medians per location, time_periods
dues <- c("Feb") # "Dec", "Jan",
due <- "Feb"

for (due in dues){
  #######################################################################################
  # Read Data
  frost_dir <- "/Users/hn/Desktop/Desktop/Ag/check_point/chilling/frost_bloom/"
  frost_dir <- paste0(frost_dir, due, "/")
  first_frost <- data.table(readRDS(paste0(frost_dir, "first_frost_till_", due, ".rds")))
  first_frost <- first_frost %>% filter(year >= 2025) %>% data.table()
  suppressWarnings({ first_frost <- within(first_frost, remove(location, month, day, tmin))})
  first_frost <- first_frost %>% filter(city %in% bloom_LC$city)
  suppressWarnings({ first_frost <- within(first_frost, remove(extended_DoY))})

  first_frost <- first_frost %>% 
                 filter(time_period != "1950-2005" & 
                        model %in% all_models_Observed & 
                        time_period != "2006-2025" ) %>% 
                 data.table()

  first_frost <- first_frost %>% filter(year <= 2094) %>% data.table()
  first_frost <- first_frost %>% filter(year >= 1980) %>% data.table()

  # change the time periods to observed and future so we can drop
  # the line connecting 2015 to 2026
  first_frost$time_period[first_frost$time_period == "1979-2015"] <- "observed"
  first_frost$time_period[first_frost$time_period == "2076-2099"] <- "future"
  first_frost$time_period[first_frost$time_period == "2026-2050"] <- "future"
  first_frost$time_period[first_frost$time_period == "2051-2075"] <- "future"

  # setcolorder(bloom_LC, c("city", "year", "time_period", "emission", "apple_type", "dayofyear", "model"))
  # setnames(bloom_LC, old=c("dayofyear"), new=c("fifty_perc_DoY"))
  # bloom_LC <- data.table(bloom_LC)
  
  # change the days so they match chill calendar year thing!!! dammit
  # bloom_LC$fifty_perc_DoY <- bloom_LC$fifty_perc_DoY + 365
  
  ct <- cities[1]
  em <- emissions[1]
  app_tp <- apple_types[1]
  lim <- 20

  for (lim in seq(50, 75, 5)){
    col_name <- paste0("thresh_", lim)
    frost_plot_dir <- paste0(frost_dir, "cloudy/", fruit_type, "/no_obs/just_frost/")
    if (dir.exists(frost_plot_dir) == F) {dir.create(path = frost_plot_dir, recursive = T)}

    for (ct in cities){
      for (em in emissions){
        curr_frost <- first_frost %>% filter(city==ct & emission==em) %>% data.table()

        frost_plt <- cloudy_frost(d1=curr_frost, 
                                  colname="chill_dayofyear", 
                                  fil="first frost") + 
                     ggtitle(lab=paste0("first frost shift"))

        ggsave(plot = frost_plt,
               filename = paste0(ct, "_", em, ".png"), 
               width = 8, height=4, units = "in", 
               dpi=400, device = "png",
               path=frost_plot_dir)

        ##### Threshold care
        curr_thresh <- subset(thresh_dt, select=c("city", "year", "chill_season", 
                                                  "time_period",
                                                  "model", "emission", col_name))
        curr_thresh <- curr_thresh %>% 
                       filter(city==ct & emission==em) %>% 
                       data.table()

        ######################################################################
        #
        #        REMOVE NAs
        #
        if (remove_NA == "yes"){
          curr_thresh <- curr_thresh %>% 
                       filter(get(col_name)<365) %>% 
                       data.table()
        }
        ######################################################################
        thresh_plt <- cloudy_frost(d1=curr_thresh, 
                                   colname=paste0("thresh_", lim), 
                                   fil=paste0(lim, " CP ", "threshold")) +
                      ggtitle(lab=paste0(lim," CP shift"))

        just_thresh_plot_dir <- paste0(frost_dir, "cloudy/", 
                                       fruit_type , 
                                       "/no_obs/just_thresh/thresh_", 
                                       lim, "/")

        if (dir.exists(just_thresh_plot_dir) == F) {
            dir.create(path = just_thresh_plot_dir, recursive = T)}

        ggsave(plot = thresh_plt,
               filename = paste0(ct, "_", em, ".png"), 
               width = 8, height=6, units = "in", 
               dpi=400, device = "png",
               path=just_thresh_plot_dir)

        #^^^^^^^^^^^ Threshold care
        for (app_tp in apple_types){
          curr_bloom <- bloom_LC %>% 
                        filter(city==ct & emission==em & 
                              apple_type == gsub("\ ", "_", tolower(app_tp))) %>% 
                        data.table()
          if (fruit_type=="apple"){
               title_ <- paste0(app_tp, " bloom shift")
             } else {
               title_ <- paste0(fruit_type, " bloom shift")
          }

          just_bloom <- cloudy_frost(d1=curr_bloom, colname="fifty_perc_DoY", fil="bloom") +
                        ggtitle(lab=title_)

          just_bloom_plot_dir <- paste0(frost_dir, "cloudy/", fruit_type, 
                                        "/no_obs/just_bloom/")
          if (dir.exists(just_bloom_plot_dir) == F) {
              dir.create(path = just_bloom_plot_dir, recursive = T)}
          
          ggsave(plot=just_bloom,
                 filename = paste0(ct, "_", em, "_", app_tp, ".png"), 
                 width = 8, height=6, units = "in", 
                 dpi=400, device = "png",
                 path=just_bloom_plot_dir)
          ########################################################################
          #                                                                      #
          # layover -- layover -- layover -- layover -- layover -- layover       #
          # organize for layover plot                                            #
          # layover -- layover -- layover -- layover -- layover -- layover       #
          #                                                                      #
          ########################################################################
          curr_bloom$time_period <- tolower(curr_bloom$time_period)
          curr_thresh$time_period <- tolower(curr_thresh$time_period)
          curr_bloom$time_period[curr_bloom$time_period=="historical"] <- "observed"

          suppressWarnings({ curr_bloom <- within(curr_bloom, remove(apple_type))})
          suppressWarnings({ curr_thresh <- within(curr_thresh, remove(chill_season))})
          setcolorder(curr_bloom, c("city", "year", "time_period", 
                                    "model", "emission", "fifty_perc_DoY"))
          if (paste0("thresh_", lim) %in% colnames(curr_thresh)){
            setnames(curr_thresh, old=c(paste0("thresh_", lim)), new=c("thresh"))
          }
          # merged_dt <- merge(curr_thresh, curr_bloom, all.x=TRUE)
          # merged_dt <- na.omit(merged_dt)
          # merged_dt$thresh <- as.numeric(merged_dt$thresh)
          # merged_dt$fifty_perc_DoY <- as.numeric(merged_dt$fifty_perc_DoY)
          # merged_dt <- melt(merged_dt, id=c("city", "year", "time_period", 
          #                                   "model", "emission"))
          curr_thresh_melt <- melt(curr_thresh, 
                                   id=c("city", "year", "time_period", "model", "emission"))
          curr_bloom_melt <- melt(curr_bloom, 
                                  id=c("city", "year", "time_period", "model", "emission"))
          merged_dt <- rbind(curr_thresh_melt, curr_bloom_melt)
          
          merged_plt <- double_cloud(d1 = merged_dt) + 
                        ggtitle(lab=title_)
                        
          if (fruit_type=="apple"){
             title_ <- paste0(lim, " CP threshold ", " and bloom shifts", "(", app_tp,")")
             } else{
              title_ <- paste0(lim, " CP threshold and ", fruit_type, " bloom shifts")
          }
          
          bloom_thresh_plot_dir <- paste0(frost_dir,
                                          "cloudy/", fruit_type,
                                          "/no_obs/bloom_thresh_in_one/",
                                          col_name, "/")
          if (dir.exists(bloom_thresh_plot_dir) == F) {
              dir.create(path = bloom_thresh_plot_dir, recursive = T)}
          ggsave(plot=merged_plt,
                 filename = paste0(ct, "_", em, "_", app_tp, ".png"), 
                 width = 8, height=6, units = "in", 
                 dpi=400, device = "png",
                 path=bloom_thresh_plot_dir)
        }
      }
    }
  }
}

print(Sys.time() - start_time)