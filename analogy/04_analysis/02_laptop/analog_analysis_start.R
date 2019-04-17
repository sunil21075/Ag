######################################################################
rm(list=ls())
library(lubridate)
library(purrr)
library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)
library(maps)

options(digit=9)
options(digits=9)

######################################################################
####
####         Set up directories
####
######################################################################

data_dir <- "/Users/hn/Desktop/Desktop/Kirti/check_point/analogs/w_gen_w_prec/500/"
param_dir <- "/Users/hn/Documents/GitHub/Kirti/analogy/parameters/"

######################################################################
####
####           global Files
####
######################################################################
local_cnty_fips <- "local_county_fips.csv"
usa_cnty_fips <- "all_us_1300_county_fips_locations.csv"

local_cnty_fips <- data.table(read.csv(paste0(param_dir, local_cnty_fips), header=T, sep=",", as.is=T))
usa_cnty_fips <- data.table(read.csv(paste0(param_dir, usa_cnty_fips), header=T, sep=",", as.is=T))
local_fips <- unique(local_cnty_fips$fips)

########################################################################
####
####           Find frequencies at the county level
####
########################################################################
files <- c("all_close_analogs_unique_2026_2050_rcp45.rds", 
           "all_close_analogs_unique_2051_2075_rcp45.rds",
           "all_close_analogs_unique_2076_2095_rcp45.rds")

############################################################
###
###               Load needed data for plotting
###
############################################################

data(county.fips) # Load the county.fips dataset for plotting
cnty <- map_data("county") # Load the county data from the maps package
cnty2 <- cnty %>%
         mutate(polyname = paste(region, subregion, sep=",")) %>%
         left_join(county.fips, by="polyname")

for (file in files){
  dt_H <- data.table(readRDS(paste0(data_dir, file)))
  output <- find_unique_county_to_county_freq(dt_H, local_cnty_fips, usa_cnty_fips)
  # dt_aggregation <- output[[1]] # all models are here
  # dt_agg_bcc_m <- output[[2]]
  # dt_agg_BNU  <- output[[3]]
  # dt_agg_CanESM2 <- output[[4]]
  # dt_agg_CNRM_CM5 <- output[[5]]
  # dt_agg_GFDLG <- output[[6]]
  # dt_agg_GFDLM <- output[[7]]

  for (ii in 2:7){ # run for different models
  	curr_data <- output[[ii]]
  	for (curr_fip in local_fips){ # run for different local (future) counties
	  one_county_one_model <- curr_data %>% filter(query_fips==curr_fip)
	  model_name <- unique(one_county_one_model$ClimateScenario)

      cnty2_one_county_one_model <- left_join(one_county_one_model, cnty2, by=c("analog_fips" = "fips"))
      cnty_name <- find_county_name(usa_cnty_fips, curr_fip)

	  target_annotation <- cnty2_one_county_one_model
      target_annotation <- target_annotation %>% filter(subregion==cnty_name)
      target_annotation <- target_annotation %>%
                           group_by(subregion, region, polyname, group) %>%
                           summarise_at(vars(long, lat), funs(mean(., na.rm=TRUE))) %>%
                           data.table()

	  curr_plot <- ggplot(cnty2_one_county_one_model, aes(long, lat, group = group)) + 
                     geom_polygon(data = cnty2) + 
                     geom_polygon(aes(fill = freq), colour = rgb(1, 1, 1, 0.2))  +
                     geom_text(data=target_annotation, aes(long, lat, label = subregion), size=2, fontface="bold", color="red") + 
                     coord_quickmap() + 
                     theme(legend.title = element_blank(),
                           axis.text.x = element_blank(),
                           axis.text.y = element_blank(),
                           axis.ticks.x = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.title.x = element_blank(),
                           axis.title.y = element_blank()
                           )
      plot_name <- paste0()
      assign(x = paste(plot_name), value = {curr_plot})
    }
  }
}

# one_county_one_model <- dt_agg_bcc_m %>% filter(query_fips==53047)

# model_name <- unique(one_county_one_model$ClimateScenario)

# cnty2_one_county_one_model <- left_join(one_county_one_model, cnty2, by=c("analog_fips" = "fips"))

# # for_annotation <- cnty2_one_county_one_model
# # for_annotation <- within(for_annotation, remove(query_fips, analog_fips, ClimateScenario, freq, order))
# # for_annotation <- for_annotation %>%
# #                   group_by(subregion, region, polyname, group) %>%
# #                   summarise_at(vars(long, lat), funs(mean(., na.rm=TRUE))) %>%
# #                   data.table()

# cnty_name <- find_county_name(usa_cnty_fips, 53047)

# target_annotation <- cnty2_one_county_one_model
# target_annotation <- target_annotation %>% filter(subregion==cnty_name)
# target_annotation <- target_annotation %>%
#                       group_by(subregion, region, polyname, group) %>%
#                       summarise_at(vars(long, lat), funs(mean(., na.rm=TRUE))) %>%
#                       data.table()

# curr_plot <- ggplot(cnty2_one_county_one_model, aes(long, lat, group = group)) + 
#              geom_polygon(data = cnty2) + 
#              geom_polygon(aes(fill = freq), colour = rgb(1, 1, 1, 0.2))  +
#              geom_text(data=target_annotation, aes(long, lat, label = subregion), size=2, fontface="bold", color="red") + 
#              coord_quickmap() + 
#              theme(legend.title = element_blank(),
#                    #legend.position = "bottom",
#                    axis.text.x = element_blank(),
#                    axis.text.y = element_blank(),
#                    axis.ticks.x = element_blank(),
#                    axis.ticks.y = element_blank(),
#                    axis.title.x = element_blank(),
#                    axis.title.y = element_blank()) + 
#              ggtitle(())













