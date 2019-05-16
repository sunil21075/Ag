
library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggpubr)

options(digits=9)
options(digit=9)

#######################################################################################
                         #                            #
                         #       Functions here       #
                         #                            #
                         ##############################

#######################################################################################

data_dir <- "/Users/hn/Desktop/Desktop/Kirti/check_point/chilling/frost_bloom/"
param_dir <- "/Users/hn/Documents/GitHub/Kirti/chilling/parameters/"

#######################################################################################

LOI <- data.table(read.csv(paste0(param_dir, "limited_locations.csv"), as.is=T))

#######################################################################################

# Read Data
first_frost <- data.table(readRDS(paste0(data_dir, "first_frost.rds")))
fifth_frost <- data.table(readRDS(paste0(data_dir, "fifth_frost.rds")))

#######################################################################################
                         #                            #
                         #    box plot of all locs    #
                         #                            #
                         ##############################

first_frost_all_locs_box <- boxplot_frost_dayofyear(dt=first_frost, kth_day=1)
fifth_frost_all_locs_box <- boxplot_frost_dayofyear(dt=fifth_frost, kth_day=5)

assembeled <- ggarrange(plotlist = list(first_frost_all_locs_box, fifth_frost_all_locs_box), 
                        ncol = 1, nrow = 2, 
                        widths = c(1, 2.3) , heights = 1,
                        common.legend = TRUE, legend = "bottom")

annot_text <- "All locations (2358) and 19 models are included here."
assembeled <- annotate_figure(assembeled,
                              top = text_grob(annot_text, 
                                               color = "black", face = "bold", 
                                               size = 12, hjust = 1.05))

ggsave(plot = assembeled, 
       filename = "frost_all_locs.png", 
       width = 10, height = 8, units = "in", 
       dpi = 400, device = "png",
       path = data_dir)

########################################################
#
#          Filter locations of interest data
#

first_frost_limited <- pick_single_cities_by_location(dt=first_frost, city_info=LOI)
fifth_frost_limited <- pick_single_cities_by_location(dt=fifth_frost, city_info=LOI)

#
#   Box plot of locations of interest
#
first_frost_select_locs_box <- boxplot_frost_dayofyear(dt=first_frost_limited, kth_day=1)
fifth_frost_select_locs_box <- boxplot_frost_dayofyear(dt=fifth_frost_limited, kth_day=5)

assembeled_limited <- ggarrange(plotlist = list(first_frost_select_locs_box, 
                                                fifth_frost_select_locs_box), 
                                                ncol = 1, nrow = 2, 
                                                widths = c(1, 2.3) , heights = 1,
                                                common.legend = TRUE, legend = "bottom")

annot_text <- "Selected locations (10) and 19 models are included here."
assembeled_limited <- annotate_figure(assembeled_limited,
                                      top = text_grob(annot_text, 
                                                      color = "black", face = "bold", 
                                                       size = 12, hjust = 1))
ggsave(plot = assembeled_limited, 
       filename = "frost_selected_locs.png", 
       width = 10, height = 8, units = "in", 
       dpi = 400, device = "png",
       path = data_dir)

###################################################################################
#
#         Time Series of limited locations and limited models
#
###################################################################################

subset_of_models <- c("bcc-csm1-1-m", "BNU-ESM", "CanESM2", 
                      "CNRM-CM5", "GFDL-ESM2G", "GFDL-ESM2M")
##### observed
first_frost_limited_obs <- first_frost_limited  %>% filter(model == "Observed")
fifth_frost_limited_obs <- fifth_frost_limited  %>% filter(model == "Observed")

##### modeled

first_frost_double_limited <- first_frost_limited %>% filter(model %in% subset_of_models)
fifth_frost_double_limited <- fifth_frost_limited %>% filter(model %in% subset_of_models)

first_frost_double_limited <- rbind(first_frost_double_limited, first_frost_limited_obs)
fifth_frost_double_limited <- rbind(fifth_frost_double_limited, fifth_frost_limited_obs)

first_frost_limited_loc_mod_TS <- plot_frost_TS(first_frost_double_limited)
fifth_frost_limited_loc_mod_TS <- plot_frost_TS(fifth_frost_double_limited)

ggsave(plot = first_frost_limited_loc_mod_TS, 
       filename = "first_frost_limited_loc_mod_TS.png", 
       width = 21, height = 40, units = "in", 
       dpi = 400, device = "png",
       path = data_dir)

ggsave(plot = fifth_frost_limited_loc_mod_TS, 
       filename = "fifth_frost_limited_loc_mod_TS.png", 
       width = 21, height = 40, units = "in", 
       dpi = 400, device = "png",
       path = data_dir)

###################################################################################
#
#         Take Median accross models so we have one model per location
#
###################################################################################

# group by time period and location and take medians

first_frost_double_limited <- first_frost_double_limited %>%
                              group_by(time_period, city, emission)


