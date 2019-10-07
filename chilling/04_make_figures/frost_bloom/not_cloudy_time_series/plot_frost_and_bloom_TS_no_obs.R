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
chill_doy_map <- read.csv("/Users/hn/Documents/GitHub/Ag/chill_DoY_map.csv", as.is=TRUE)
#####################################################################################
the <- theme(plot.title = element_text(size=17, face="bold"),
             plot.margin = margin(t=.2, r=.5, b=.2, l=.2, "cm"),
             legend.position="bottom",
             panel.grid.minor = element_blank(),
             panel.spacing = unit(.5, "cm"),
             panel.grid.major = element_line(size = 0.1),
             axis.ticks = element_line(color = "black", size = .2),
             strip.text.x = element_text(size = 20, face = "bold"),
             strip.text.y = element_text(size = 20, face = "bold"),
             axis.text.x = element_text(size = 16, face = "bold", color="black"), # angle=-30
             axis.text.y = element_text(size = 16, color="black", face = "bold"),
             axis.title.x = element_blank(),
             axis.title.y = element_blank(), #element_text(size = 28, face = "bold", margin = margin(t=0, r=12, b=0, l=0)),
              # axis.ticks.y = element_blank()
             )
#####################################################################################

bloom_dir <- "/Users/hn/Desktop/Desktop/Ag/check_point/chilling/frost_bloom/bloom_new_params/for_TS_with_frost/"
bloom_LC <- readRDS(paste0(bloom_dir, "bloom_limited_50Percent.rds"))
bloom_LC <- bloom_LC %>% filter(year>=2025) %>% data.table()
bloom_LC$city <- as.character(bloom_LC$city)
bloom_LC <- within(bloom_LC, remove(location))
bloom_LC$time_period[bloom_LC$time_period == "1979-2015"] <- "observed"
bloom_LC$time_period[bloom_LC$time_period == "2026-2050"] <- "future"
bloom_LC$time_period[bloom_LC$time_period == "2051-2075"] <- "future"
bloom_LC$time_period[bloom_LC$time_period == "2076-2095"] <- "future"

########## Add/Correct chill calendar type thing
# reduce the years of days in {1, 2, ..., 243}
# add 122 to the days between 1 and 243.
bloom_LC$year[bloom_LC$medDoY %in% c(1:243)] <- bloom_LC$year[bloom_LC$medDoY %in% c(1:243)] - 1
bloom_LC <- bloom_LC %>% filter(year>=1979)
bloom_LC$medDoY[bloom_LC$medDoY %in% c(1:243)] <- bloom_LC$medDoY[bloom_LC$medDoY %in% c(1:243)] + 122

# compute medians per location, time_periods
dues <- c("Feb") # "Dec", "Jan",
due <- "Feb"

######## Read bloom to filter the frost data by model
all_models_Observed <- c("Observed", "BNU-ESM", "CanESM2", 
                         "GFDL-ESM2G", "bcc-csm1-1-m", "CNRM-CM5",
                         "GFDL-ESM2M")

all_models_observed <- c("observed", "BNU-ESM", "CanESM2", 
                         "GFDL-ESM2G", "bcc-csm1-1-m", "CNRM-CM5",
                         "GFDL-ESM2M")

cities <- c("Hood River", "Walla Walla", "Richland", "Yakima", "Wenatchee", "Omak")
emissions <- c("RCP 4.5", "RCP 8.5")
apple_types <- c("Cripps Pink", "Gala", "Red Deli")
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
chill_limited <- within(chill_limited, remove(lat, long))
chill_limited <- chill_limited %>% filter(city %in% cities) %>% data.table()

thresh_dir <- "/Users/hn/Desktop/Desktop/Ag/check_point/chilling/"

thresh_dt <- readRDS(paste0(thresh_dir, "/sept_summary_comp.rds"))
thresh_dt <- thresh_dt %>% filter(year>=2025) %>% data.table()

thresh_dt$location <- paste0(thresh_dt$lat, "_", thresh_dt$long)
thresh_dt <- thresh_dt %>% 
                filter(location %in% chill_limited$location &
                       model %in% all_models_observed) %>% 
                data.table()

thresh_dt <- merge(thresh_dt, chill_limited, all.x=TRUE)
thresh_dt <- within(thresh_dt, 
                       remove(location, lat, long, start, sum_J1, sum_F1, sum_M1,
                              sum_A1, sum))

thresh_dt <- remove_modeled_historical_add_time_period(thresh_dt)
thresh_dt <- remove_F0(thresh_dt)
thresh_dt <- thresh_dt %>% filter(year <= 2094)%>% data.table()

####################################################################################

for (due in dues){
  #######################################################################################
  # Read Data
  frost_dir <- "/Users/hn/Desktop/Desktop/Ag/check_point/chilling/frost_bloom/"
  frost_dir <- paste0(frost_dir, due, "/")
  first_frost <- data.table(readRDS(paste0(frost_dir, "first_frost_till_", due, ".rds")))
  first_frost <- first_frost %>% filter(year>=2025) %>% data.table()
  
  first_frost <- pick_single_cities_by_location(dt=first_frost, city_info=LOI)
  first_frost <- within(first_frost, remove(location, month, day, tmin))
  first_frost <- first_frost %>% filter(city %in% bloom_LC$city)
  first_frost <- within(first_frost, remove(extended_DoY))

  first_frost <- first_frost %>% 
                 filter(time_period != "1950-2005" & 
                        model %in% all_models_Observed & 
                        time_period != "2006-2025" ) %>% 
                 data.table()

  first_frost <- first_frost %>% filter(year <= 2094) %>% data.table()
  first_frost <- first_frost %>% filter(year >= 1950) %>% data.table()

  # change the time periods to observed and future so we can drop
  # the line connecting 2015 to 2026
  first_frost$time_period[first_frost$time_period == "1979-2015"] <- "observed"
  first_frost$time_period[first_frost$time_period == "2076-2099"] <- "future"
  first_frost$time_period[first_frost$time_period == "2026-2050"] <- "future"
  first_frost$time_period[first_frost$time_period == "2051-2075"] <- "future"

  # The bloom data is median of models. So, we have to make medians
  # here, the cloud thing can wait till later.
  first_frost <- first_frost[, .(median_DoY = mean(chill_dayofyear)), 
                             by = c("year", "city", "time_period", "emission")]

  setcolorder(bloom_LC, c("city", "year", "time_period", "emission", "apple_type", "medDoY"))
  setnames(bloom_LC, old=c("medDoY"), new=c("fifty_perc_DoY"))
  bloom_LC <- data.table(bloom_LC)
  
  # change the days so they match chill calendar year thing!!! dammit
  # bloom_LC$fifty_perc_DoY <- bloom_LC$fifty_perc_DoY + 365
  
  setcolorder(first_frost, c("city",  "year", "time_period", "emission", "median_DoY"))
  ct <- cities[1]
  em <- emissions[1]
  app_tp <- apple_types[1]
  lim <- 20

  for (lim in seq(20, 75, 5)){
    col_name <- paste0("thresh_", lim)

    frost_plot_dir <- paste0(frost_dir, "not_cloudy/no_obs/just_frost/")
    bloom_thresh_plot_dir <- paste0(frost_dir, "not_cloudy/no_obs/bloom_thresh_in_one/", col_name, "/")
    if (dir.exists(frost_plot_dir) == F) {dir.create(path = frost_plot_dir, recursive = T)}
    if (dir.exists(bloom_thresh_plot_dir) == F) {dir.create(path = bloom_thresh_plot_dir, recursive = T)}

    for (ct in cities){
      for (em in emissions){
        curr_frost <- first_frost %>% filter(city==ct & emission==em) %>% data.table()
        title_ <- paste0(ct, ", first frost shift")
        frost_plt <- ggplot() + 
                     geom_line(data = curr_frost, 
                               aes(x = year, y = median_DoY, group=time_period), color = "red") +
                     geom_smooth(data = curr_frost, aes(x = year, y = median_DoY, group=time_period), 
                                 method = "lm", size=1, se=T, color="black") + 
                     facet_grid(~ emission) + 
                     scale_x_continuous(breaks=seq(1950, 2100, 10)) +
                     scale_y_continuous(breaks = chill_doy_map$day_count_since_sept, 
                                        labels = chill_doy_map$letter_day) +
                     ylab("day of year") + xlab("years") + the + 
                     ggtitle(lab=title_)

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

        curr_thresh <- thresh_median_4_frost_bloom_thresh_plot(data_tb=curr_thresh, 
                                                               colname=col_name)
        #^^^^^^^^^^^ Threshold care
        for (app_tp in apple_types){
          curr_bloom <- bloom_LC %>% 
                        filter(city==ct & emission==em & 
                              apple_type == gsub("\ ", "_", tolower(app_tp))) %>% 
                        data.table()

          title_ <- paste0(ct, ", ", lim, " CP threshold ", " and bloom shifts", "(", app_tp,")")

          bloom_thresh_plt <- ggplot() + 
                              geom_line(data = curr_bloom, 
                                        aes(x = year, y = fifty_perc_DoY, 
                                            group=time_period), color = "blue") + 
                              geom_smooth(data = curr_bloom, aes(x = year, y = fifty_perc_DoY, group=time_period), 
                                          method = "lm", size=1, se=T, color="black") + 
                              geom_line(data = curr_thresh, 
                                        aes(x = year, y = thresh_median_DoY, group=time_period), color = "green") +
                              geom_smooth(data = curr_thresh, 
                                          aes(x = year, y = thresh_median_DoY, 
                                              group=time_period), 
                                          method = "lm", size=1, se=T, color="darkgreen") + 
                              facet_grid(~ emission) + 
                              scale_y_continuous(breaks = chill_doy_map$day_count_since_sept, 
                                                 labels = chill_doy_map$letter_day) +
                              scale_x_continuous(breaks=seq(1950, 2100, 10)) +
                              ylab("day of year") + xlab("years") + the + 
                              ggtitle(lab=title_)
          
          ggsave(plot = bloom_thresh_plt,
                 filename = paste0(ct, "_", em, "_", app_tp, ".png"), 
                 width = 8, height=6, units = "in", 
                 dpi=400, device = "png",
                 path=bloom_thresh_plot_dir)

          bloom_plt <- ggplot() + 
                       geom_line(data = curr_bloom, 
                       aes(x = year, y = fifty_perc_DoY, group=time_period), color = "blue") + 
                       geom_smooth(data = curr_bloom, 
                                   aes(x = year, y = fifty_perc_DoY, group=time_period), 
                                   method = "lm", size=1, se=T, color="black") + 
                       facet_grid(~ emission) + 
                       scale_y_continuous(breaks = chill_doy_map$day_count_since_sept, 
                                          labels = chill_doy_map$letter_day) +
                       scale_x_continuous(breaks=seq(1950, 2100, 10)) +
                       ylab("day of year") + xlab("years") + the + 
                       ggtitle(lab=paste0(ct, ", ", app_tp, " bloom shift"))
          
          just_bloom_plot_dir <- paste0(frost_dir, "not_cloudy/no_obs/just_bloom/")
          if (dir.exists(just_bloom_plot_dir) == F) {
            dir.create(path = just_bloom_plot_dir, recursive = T)}

          ggsave(plot = bloom_plt,
                 filename = paste0(ct, "_", em, "_", app_tp, ".png"), 
                 width = 8, height=6, units = "in", 
                 dpi=400, device = "png",
                 path=paste0(just_bloom_plot_dir))

          just_thresh_plot_dir <- paste0(frost_dir, "not_cloudy/no_obs/just_thresh/thresh_", lim, "/")
          if (dir.exists(just_thresh_plot_dir) == F) {
            dir.create(path = just_thresh_plot_dir, recursive = T)}
          
          thresh_plt <- ggplot() + 
                        geom_line(data = curr_thresh, 
                                  aes(x = year, y = thresh_median_DoY, group=time_period), color = "green") +
                        geom_smooth(data = curr_thresh, 
                                    aes(x = year, y = thresh_median_DoY, 
                                        group=time_period), 
                                    method = "lm", size=1, se=T, color="darkgreen") + 
                        facet_grid(~ emission) + 
                        scale_y_continuous(breaks = chill_doy_map$day_count_since_sept, 
                                           labels = chill_doy_map$letter_day) +
                        scale_x_continuous(breaks=seq(1950, 2100, 10)) +
                        ylab("day of year") + xlab("years") + the + 
                        ggtitle(lab=paste0(ct, ", ", lim, " CP threshold shift"))
                    
          ggsave(plot = thresh_plt,
                filename = paste0(ct, "_", em, "_", app_tp, ".png"), 
                width = 8, height=6, units = "in", 
                dpi=400, device = "png",
                path=just_thresh_plot_dir)
        }
      }
    }
  }
}


