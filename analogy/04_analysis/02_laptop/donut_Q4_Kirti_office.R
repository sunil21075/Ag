######################################################################
# rm(list=ls())

library(lubridate)
library(purrr)
library(maps)
library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)

# library(swfscMisc) has na.count(.) in it

options(digit=9)
options(digits=9)

######################################################################
####
####           global Files
####
######################################################################
param_dir <- "/Users/hn/Documents/GitHub/Kirti/analogy/parameters/"
new_param_dir <- paste0(param_dir, "new_params_for_analog/")

min_grids <- 5
hist_grid_count <- read.csv(paste0(new_param_dir, "cod_moth_hist_grid_count_within_counties.csv"),
                            header=T, as.is=T)  %>%                            
                            filter(grid_count >= min_grids) %>%
                            data.table()

hist_grid_count$st_county <- paste(hist_grid_count$state, hist_grid_count$county, sep="_" )
hist_grid_count <- within(hist_grid_count, remove(state, county))

###
###

f_loc_fips_st_cnty <- "local_county_fips.csv"
h_loc_fips_st_cnty <- "all_us_1300_county_fips_locations.csv"

f_loc_fips_st_cnty <- data.table(read.csv(paste0(param_dir, f_loc_fips_st_cnty), header=T, sep=",", as.is=T))
h_loc_fips_st_cnty <- data.table(read.csv(paste0(param_dir, h_loc_fips_st_cnty), header=T, sep=",", as.is=T))

f_loc_fips_st_cnty <- f_loc_fips_st_cnty %>% 
                      filter(location %in% h_loc_fips_st_cnty$location) %>% 
                      data.table()

local_fips <- unique(f_loc_fips_st_cnty$fips)
 
# f_grid_count <- f_loc_fips_st_cnty %>%
#                 group_by(fips, st_county) %>%
#                 transmute(grid_count = n_distinct(location)) %>%
#                 unique() %>%
#                 data.table()

######## Filter h_loc_fips_st_cnty, so, it only includes
######## counties with minimum number of grids in it. 
######## Doing so in this step may prevent slipping 
######## not qualified counties in calculations:
######## Doing so, takes us from 1293 locations to 1175. (118 locations are gone)

h_loc_fips_st_cnty <- h_loc_fips_st_cnty %>% filter(fips %in% hist_grid_count$fips)

######################################################
# 
# To extract names of counties associated with each unique fips
# 

Min_fips_st_county <- data.table(read.csv(paste0(param_dir, "Min_fips_st_county_location.csv"), header=T, sep=",", as.is=T)) 
Min_fips_st_county <- within(Min_fips_st_county, remove(location))
Min_fips_st_county <- unique(Min_fips_st_county)

######################################################

######################################################

model_names <- c("bcc-csm1-1-m", "BNU-ESM", "CanESM2", "CNRM-CM5", "GFDL-ESM2G", "GFDL-ESM2M")
time_p <- c("2026_2050", "2051_2075", "2076_2095")
emissions <- c("rcp45", "rcp85")

######################################################################
####
####         Set up directories
####
######################################################################

# main_in <- "/Users/hn/Desktop/Desktop/Kirti/check_point/analogs/"
# data_sub_dirs <- c("no_precip_no_gen3_rcp85/", "w_precip_no_gen3_rcp85/",
#                    "no_precip_no_gen3_rcp45/", "w_precip_no_gen3_rcp45/",
#                    "no_precip_w_gen3_rcp85/", "no_precip_w_gen3_rcp45/",
#                    "w_precip_w_gen3_rcp85/", "w_precip_w_gen3_rcp45/")

main_in <- "/Users/hn/Desktop/Desktop/Kirti/check_point/analogs/"
data_sub_dirs <- c("w_precip_rcp45/", "w_precip_rcp85/")

sub_dir <- data_sub_dirs[1]

for (sub_dir in data_sub_dirs){
  # emission <- substr(unlist(strsplit(sub_dir, "_"))[5], 1, 5)
  emission <- substr(unlist(strsplit(sub_dir, "_"))[3], 1, 5)
  # plot_name_extension <- paste0("_", 
  #                               strsplit(sub_dir, "_")[[1]][1], 
  #                               "_", strsplit(sub_dir, "_")[[1]][3], 
  #                               "_", emission)
  plot_name_extension <- paste0("_", 
                                strsplit(sub_dir, "_")[[1]][1], 
                                "_", emission)

  sigma_bds <- c(1, 2)

  ######################################################################
  # sigma_bd <- sigma_bds[1]
  # target_fip <- local_fips[1]
  # model_n <- model_names[1]

  for (sigma_bd in sigma_bds){
    data_dir <- paste0(main_in, sigma_bd, "_sigma/", sub_dir)
    
    for (target_fip in local_fips){
      for(model_n in model_names){
        # read the data of the three time periods

        analog_file_name_F1 <- paste("analog", model_n, emission, time_p[1], sep="_")
        analog_dat_F1 <- data.table(readRDS(paste0(data_dir, analog_file_name_F1, ".rds")))

        analog_file_name_F2 <- paste("analog", model_n, emission, time_p[2], sep="_")
        analog_dat_F2 <- data.table(readRDS(paste0(data_dir, analog_file_name_F2, ".rds")))

        analog_file_name_F3 <- paste("analog", model_n, emission, time_p[3], sep="_")
        analog_dat_F3 <- data.table(readRDS(paste0(data_dir, analog_file_name_F3, ".rds")))
        #############################################################################

        #############################################################################
        #
        #   Filter out counties with less than "min_grids" grids in them.
        #  *** Let us remove counties with historical #grids < min_grids from the map ***
        
        analog_dat_F1 <- analog_dat_F1 %>% 
                         filter(analog_NNs_county %in% hist_grid_count$fips)
        analog_dat_F2 <- analog_dat_F2 %>% 
                         filter(analog_NNs_county %in% hist_grid_count$fips)
        analog_dat_F3 <- analog_dat_F3 %>% 
                         filter(analog_NNs_county %in% hist_grid_count$fips)

        #############################################################################

        # filter the location of interest:
        analog_dat_F1 <- analog_dat_F1 %>% filter(query_county == target_fip)
        analog_dat_F2 <- analog_dat_F2 %>% filter(query_county == target_fip)
        analog_dat_F3 <- analog_dat_F3 %>% filter(query_county == target_fip)
     
        # remove the stats of not_analog pieces
        analog_dat_F1 <- analog_dat_F1 %>% filter(analog_NNs_county != "no_analog")
        analog_dat_F2 <- analog_dat_F2 %>% filter(analog_NNs_county != "no_analog")
        analog_dat_F3 <- analog_dat_F3 %>% filter(analog_NNs_county != "no_analog")

        # Convert the fips to inetgers so they can be used for plotting:
        analog_dat_F1$analog_NNs_county <- as.integer(analog_dat_F1$analog_NNs_county)
        analog_dat_F1$query_county <- as.integer(analog_dat_F1$query_county)

        analog_dat_F2$analog_NNs_county <- as.integer(analog_dat_F2$analog_NNs_county)
        analog_dat_F2$query_county <- as.integer(analog_dat_F2$query_county)

        analog_dat_F3$analog_NNs_county <- as.integer(analog_dat_F3$analog_NNs_county)
        analog_dat_F3$query_county <- as.integer(analog_dat_F3$query_county)
        
        #############################################################################
        # 
        # Determine how many years are in future data frames, to be used
        # for standardization of maps and data needed for donuts.
        
        f_years_F1 = 1 + as.numeric(unlist(strsplit(time_p[1], "_")))[2] - 
                         as.numeric(unlist(strsplit(time_p[1], "_")))[1]

        f_years_F2 = 1 + as.numeric(unlist(strsplit(time_p[2], "_")))[2] - 
                         as.numeric(unlist(strsplit(time_p[2], "_")))[1]

        f_years_F3 = 1 + as.numeric(unlist(strsplit(time_p[3], "_")))[2] - 
                         as.numeric(unlist(strsplit(time_p[3], "_")))[1]

        #############################################################################
        #############################################################################
        #
        # *** Standardize the freq. using # of grids in each county (historical data) ***
        # hist_counties_with_more_10 has the fips and count of grids within each county.
        # But then we talked in her office and we changed the denominator to all possible
        # pairwise combinations, so, fractions are between 0 and 1.
        # This should be done just for maps.
        analog_dat_F1_4_map <- standardize_by_all_pairs(analog_dt=analog_dat_F1, 
                                                        f_fips_dt=f_loc_fips_st_cnty, 
                                                        h_fips_dt=h_loc_fips_st_cnty, 
                                                        f_years=f_years_F1, h_years=37)
        
        analog_dat_F2_4_map <- standardize_by_all_pairs(analog_dt=analog_dat_F2, 
                                                        f_fips_dt=f_loc_fips_st_cnty, 
                                                        h_fips_dt=h_loc_fips_st_cnty, 
                                                        f_years=f_years_F2, h_years=37)
        
        analog_dat_F3_4_map <- standardize_by_all_pairs(analog_dt=analog_dat_F3, 
                                                        f_fips_dt=f_loc_fips_st_cnty, 
                                                        h_fips_dt=h_loc_fips_st_cnty, 
                                                        f_years=f_years_F3, h_years=37)
        
        most_similar_cnty_F1 <- analog_dat_F1_4_map[[2]]
        most_similar_cnty_F2 <- analog_dat_F2_4_map[[2]]
        most_similar_cnty_F3 <- analog_dat_F3_4_map[[2]]

        analog_dat_F1_4_map <- analog_dat_F1_4_map[[1]]
        analog_dat_F2_4_map <- analog_dat_F2_4_map[[1]]
        analog_dat_F3_4_map <- analog_dat_F3_4_map[[1]]

        #############################################################################

        # produce data for geographical map
        one_mod_map_info_F1 <- produce_dt_for_map(analog_dat_F1_4_map)
        one_mod_map_info_F2 <- produce_dt_for_map(analog_dat_F2_4_map)
        one_mod_map_info_F3 <- produce_dt_for_map(analog_dat_F3_4_map)

        # produce data for donuts
        one_mod_pie_info_F1 <- produce_dt_for_pie_Q4(analog_dt = analog_dat_F1, 
                                                     tgt_fip = target_fip, 
                                                     f_fips = f_loc_fips_st_cnty, 
                                                     h_fips = h_loc_fips_st_cnty, 
                                                     f_years = f_years_F1, h_years=37)

        one_mod_pie_info_F2 <- produce_dt_for_pie_Q4(analog_dt = analog_dat_F2, 
                                                     tgt_fip = target_fip, 
                                                     f_fips = f_loc_fips_st_cnty, 
                                                     h_fips = h_loc_fips_st_cnty, 
                                                     f_years = f_years_F2, h_years=37)

        one_mod_pie_info_F3 <- produce_dt_for_pie_Q4(analog_dt = analog_dat_F3, 
                                                     tgt_fip = target_fip, 
                                                     f_fips = f_loc_fips_st_cnty, 
                                                     h_fips = h_loc_fips_st_cnty, 
                                                     f_years= f_years_F3, h_years=37)

        # Extract name of county of interest for putting in the plots:
        target_cnty_name <- Min_fips_st_county$st_county[Min_fips_st_county$fips==target_fip]
        target_cnty_name <- paste(unlist(strsplit(target_cnty_name, "_"))[2], 
                            unlist(strsplit(target_cnty_name, "_"))[1], sep= ", ")

        if (emission=="rcp45"){ttl_emiss = "RCP 4.5"
          } else { ttl_emiss = "RCP 8.5"
        }

        titlem_F1 <- paste0(target_cnty_name, 
                            " (", 
                            paste(unlist(strsplit(time_p[1], "_"))[1], 
                                  unlist(strsplit(time_p[1], "_"))[2], sep="-"),
                            ", ", model_n, ", ", ttl_emiss, ")" )

        titlem_F2 <- paste0(target_cnty_name, 
                            " (", 
                            paste(unlist(strsplit(time_p[2], "_"))[1], 
                                  unlist(strsplit(time_p[2], "_"))[2], sep="-"),
                            ", ", model_n, ", ", ttl_emiss, ")" )

        titlem_F3 <- paste0(target_cnty_name, 
                            " (", 
                            paste(unlist(strsplit(time_p[3], "_"))[1], 
                                  unlist(strsplit(time_p[3], "_"))[2], sep="-"),
                            ", ", model_n, ", ", ttl_emiss, ")" )

        most_similar_cnty_F1_fip <- most_similar_cnty_F1
        most_similar_cnty_F2_fip <- most_similar_cnty_F2
        most_similar_cnty_F3_fip <- most_similar_cnty_F3

        most_similar_cnty_F1 <- Min_fips_st_county$st_county[Min_fips_st_county$fips==most_similar_cnty_F1]
        most_similar_cnty_F1 <- paste(unlist(strsplit(most_similar_cnty_F1, "_"))[2], 
                                 unlist(strsplit(most_similar_cnty_F1, "_"))[1], sep= ", ")
        
        most_similar_cnty_F2 <- Min_fips_st_county$st_county[Min_fips_st_county$fips==most_similar_cnty_F2]
        most_similar_cnty_F2 <- paste(unlist(strsplit(most_similar_cnty_F2, "_"))[2], 
                                 unlist(strsplit(most_similar_cnty_F2, "_"))[1], sep= ", ")

        most_similar_cnty_F3 <- Min_fips_st_county$st_county[Min_fips_st_county$fips==most_similar_cnty_F3]
        most_similar_cnty_F3 <- paste(unlist(strsplit(most_similar_cnty_F3, "_"))[2], 
                                 unlist(strsplit(most_similar_cnty_F3, "_"))[1], sep= ", ")

        # Plot the donuts
        # paste0(c(most_similar_cnty_F1, most_similar_cnty_F2, most_similar_cnty_F3))
        assign(x = paste0("pie_", gsub("-", "_", model_n), "_F1"), 
                          value = {plot_the_pie(one_mod_pie_info_F1, titlem_F1, most_similar_cnty_F1)}
                          )
        
        assign(x = paste0("pie_", gsub("-", "_", model_n), "_F2"), 
                          value = {plot_the_pie(one_mod_pie_info_F2, titlem_F2, most_similar_cnty_F2)}
                          )
        
        assign(x = paste0("pie_", gsub("-", "_", model_n), "_F3"), 
                          value = {plot_the_pie(one_mod_pie_info_F3, titlem_F3, most_similar_cnty_F3)}
                          )
        
        # plot geographical maps:
        
        data(county.fips) # Load the county.fips dataset for plotting
        cnty <- map_data("county") # Load the county data from the maps package
        cnty2 <- cnty %>%
                 mutate(polyname = paste(region, subregion, sep=",")) %>%
                 left_join(county.fips, by="polyname")

        target_county_map_info <- cnty2 %>% filter(fips == target_fip)
        most_similar_cnty_F1_map_info <- cnty2 %>% filter(fips == most_similar_cnty_F1_fip)
        most_similar_cnty_F2_map_info <- cnty2 %>% filter(fips == most_similar_cnty_F2_fip)
        most_similar_cnty_F3_map_info <- cnty2 %>% filter(fips == most_similar_cnty_F3_fip)
        
        assign(x = paste0("map_", gsub("-", "_", model_n), "_F1"), 
               value = {plot_the_map(one_mod_map_info_F1, cnty2, titlem_F1, 
                                     target_county_map_info, most_similar_cnty_F1_map_info)})

        assign(x = paste0("map_", gsub("-", "_", model_n), "_F2"), 
               value = {plot_the_map(one_mod_map_info_F2, cnty2, titlem_F2,
                                     target_county_map_info, most_similar_cnty_F2_map_info)})

        assign(x = paste0("map_", gsub("-", "_", model_n), "_F3"), 
               value = {plot_the_map(one_mod_map_info_F3, cnty2, titlem_F3,
                                     target_county_map_info, most_similar_cnty_F3_map_info)})

        # rm(most_similar_cnty_F1_map_info, most_similar_cnty_F2_map_info, most_similar_cnty_F3_map_info)
        # rm(most_similar_cnty_F1, most_similar_cnty_F2, most_similar_cnty_F3)
        # rm(most_similar_cnty_F1_fip, most_similar_cnty_F2_fip, most_similar_cnty_F3_fip)
        # rm(titlem_F1, titlem_F2, titlem_F3)
        # rm(one_mod_pie_info_F1, one_mod_pie_info_F3, one_mod_pie_info_F3)
      }
      assign(x = paste0("plot_", target_fip) , 
             value={ggarrange(plotlist = list(map_bcc_csm1_1_m_F1, map_bcc_csm1_1_m_F2, map_bcc_csm1_1_m_F3,
                                              pie_bcc_csm1_1_m_F1, pie_bcc_csm1_1_m_F2, pie_bcc_csm1_1_m_F3,
                                              
                                              map_BNU_ESM_F1, map_BNU_ESM_F2, map_BNU_ESM_F3,
                                              pie_BNU_ESM_F1, pie_BNU_ESM_F2, pie_BNU_ESM_F3,

                                              map_CanESM2_F1, map_CanESM2_F2, map_CanESM2_F3,
                                              pie_CanESM2_F1, pie_CanESM2_F2, pie_CanESM2_F3,

                                              map_CNRM_CM5_F1, map_CNRM_CM5_F2, map_CNRM_CM5_F3,
                                              pie_CNRM_CM5_F1, pie_CNRM_CM5_F2, pie_CNRM_CM5_F3,

                                              map_GFDL_ESM2G_F1, map_GFDL_ESM2G_F2, map_GFDL_ESM2G_F3,
                                              pie_GFDL_ESM2G_F1, pie_GFDL_ESM2G_F2, pie_GFDL_ESM2G_F3,

                                              map_GFDL_ESM2M_F1, map_GFDL_ESM2M_F2, map_GFDL_ESM2M_F3,
                                              pie_GFDL_ESM2M_F1, pie_GFDL_ESM2M_F2, pie_GFDL_ESM2M_F3),
                              heights=c(3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3, 1), 
                              ncol = 3, nrow = 12, common.legend = TRUE, legend = "bottom")})
    }
    # master_path <- "/Users/hn/Desktop/"
    master_path <- paste0(data_dir, "/plots/")
    if (dir.exists(master_path) == F) { dir.create(path = master_path, recursive = T)}
    ggsave(filename = paste0("ID_Canyon_16027", plot_name_extension, ".png"), 
           plot = plot_16027, 
           path=master_path, device="png",
           dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

    ggsave(filename = paste0("OR_Gilliam_41021", plot_name_extension, ".png"), 
           plot = plot_41021, 
           path=master_path, device="png",
           dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

    ggsave(filename = paste0("OR_Hood_River_41027", plot_name_extension, ".png"), 
           plot = plot_41027,
           path=master_path, device="png",
           dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

    ggsave(filename = paste0("OR_Morrow_41049", plot_name_extension, ".png"), 
           plot = plot_41049,
           path=master_path, device="png",
           dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

    ggsave(filename = paste0("OR_Umatilla_41059", plot_name_extension, ".png"), 
           plot = plot_41059,
           path=master_path, device="png",
           dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

    ggsave(filename = paste0("WA_Adams_53001", plot_name_extension, ".png"), 
           plot = plot_53001,
           path=master_path, device="png",
           dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

    ggsave(filename = paste0("WA_Benton_53005", plot_name_extension, ".png"), 
           plot = plot_53005, 
           path=master_path, device="png",
           dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

    ggsave(filename = paste0("WA_Chelan_53007", plot_name_extension, ".png"), 
           plot = plot_53007, 
           path=master_path, device="png",
           dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

    ggsave(filename = paste0("WA_Columbia_53013", plot_name_extension, ".png"), 
           plot = plot_53013, 
           path=master_path, device="png",
           dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

    ggsave(filename = paste0("WA_Douglas_53017", plot_name_extension, ".png"), 
           plot = plot_53017, 
           path=master_path, device="png",
           dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

    ggsave(filename = paste0("WA_Franklin_53021", plot_name_extension, ".png"), 
           plot = plot_53021, 
           path=master_path, device="png",
           dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

    ggsave(filename = paste0("WA_Grant_53025", plot_name_extension, ".png"), 
           plot = plot_53025,
           path=master_path, device="png",
           dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

    ggsave(filename = paste0("WA_Kittitas_53037", plot_name_extension, ".png"), 
           plot = plot_53037,
           path=master_path, device="png",
           dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

    ggsave(filename = paste0("WA_Klickitat_53039", plot_name_extension, ".png"), 
           plot = plot_53039,
           path=master_path, device="png",
           dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

    ggsave(filename = paste0("WA_Okanogan_53047", plot_name_extension, ".png"), 
           plot = plot_53047,
           path=master_path, device="png",
           dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

    ggsave(filename = paste0("WA_Walla_Walla_53071", plot_name_extension, ".png"), 
           plot = plot_53071,
           path=master_path, device="png",
           dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

    ggsave(filename = paste0("WA_Yakima_53077", plot_name_extension, ".png"), 
           plot = plot_53077,
           path=master_path, device="png",
           dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

  }
}




