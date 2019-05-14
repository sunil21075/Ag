######################################################################
rm(list=ls())

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
####         Set up directories
####
######################################################################

data_sub_dirs <- c("no_no_85/", "no_w_85/", "w_no_85/", "w_w_85/", 
                   "no_no_45/", "no_w_45/", "w_no_45/", "w_w_45/")

data_dir <- paste0("/Users/hn/Desktop/Desktop/Kirti/check_point/analogs/", sigma_bd , "_sigma/",data_sub_dirs[1])
param_dir <- "/Users/hn/Documents/GitHub/Kirti/analogy/parameters/"
sigma_bd <- 2
######################################################################
####
####           global Files
####
######################################################################
local_cnty_fips <- "local_county_fips.csv"
usa_cnty_fips <- "all_us_1300_county_fips_locations.csv"
local_fip_cnty_name_map <- "17_counties_fips_unique.csv"

################## 
# 
# To extract names of counties associated with each unique fips
# 
all_us_fips <- data.table(read.csv(paste0(param_dir, "Min_fips_st_county_location.csv"), header=T, sep=",", as.is=T)) 
all_us_fips <- within(all_us_fips, remove(location))
all_us_fips <- unique(all_us_fips)
# 
##################
local_cnty_fips <- data.table(read.csv(paste0(param_dir, local_cnty_fips), header=T, sep=",", as.is=T))
usa_cnty_fips <- data.table(read.csv(paste0(param_dir, usa_cnty_fips), header=T, sep=",", as.is=T))
local_fip_cnty_name_map <- data.table(read.csv(paste0(param_dir, local_fip_cnty_name_map), 
                                               header=T, sep=",", as.is=T))

local_cnty_fips <- local_cnty_fips %>% filter(location %in% usa_cnty_fips$location)
local_fips <- unique(local_cnty_fips$fips)

model_names <- c("bcc-csm1-1-m", "BNU-ESM", "CanESM2", "CNRM-CM5", "GFDL-ESM2G", "GFDL-ESM2M")
time_p <- c("2026_2050", "2051_2075", "2076_2095")
emissions <- c("rcp45", "rcp85")

######################################################################
######################################################################
target_fip <- local_fips[1]
model_n <- model_names[1]
emission <- emissions[2]

target_fip= 53077

for (target_fip in local_fips){
  for(model_n in model_names){
    # read the data of the three time periods

    analog_file_name_F1 <- paste("analog", model_n, emission, time_p[1], sep="_")
    analog_dat_F1 <- data.table(readRDS(paste0(data_dir, analog_file_name_F1, ".rds")))

    analog_file_name_F2 <- paste("analog", model_n, emission, time_p[2], sep="_")
    analog_dat_F2 <- data.table(readRDS(paste0(data_dir, analog_file_name_F2, ".rds")))

    analog_file_name_F3 <- paste("analog", model_n, emission, time_p[3], sep="_")
    analog_dat_F3 <- data.table(readRDS(paste0(data_dir, analog_file_name_F3, ".rds")))

    # filter the location of interest:
    analog_dat_F1 <- analog_dat_F1 %>% filter(query_county == target_fip)
    analog_dat_F2 <- analog_dat_F2 %>% filter(query_county == target_fip)
    analog_dat_F3 <- analog_dat_F3 %>% filter(query_county == target_fip)
 
    # remove the stats of not_analog pieces
    analog_dat_F1 <- analog_dat_F1 %>% filter(analog_NNs_county != "no_analog")
    analog_dat_F2 <- analog_dat_F2 %>% filter(analog_NNs_county != "no_analog")
    analog_dat_F3 <- analog_dat_F3 %>% filter(analog_NNs_county != "no_analog")

    analog_dat_F1$analog_NNs_county <- as.integer(analog_dat_F1$analog_NNs_county)
    analog_dat_F1$query_county <- as.integer(analog_dat_F1$query_county)

    analog_dat_F2$analog_NNs_county <- as.integer(analog_dat_F2$analog_NNs_county)
    analog_dat_F2$query_county <- as.integer(analog_dat_F2$query_county)

    analog_dat_F3$analog_NNs_county <- as.integer(analog_dat_F3$analog_NNs_county)
    analog_dat_F3$query_county <- as.integer(analog_dat_F3$query_county)

    # produce data for geographical map
    one_mod_map_info_F1 <- produce_dt_for_map(analog_dat_F1)
    one_mod_map_info_F2 <- produce_dt_for_map(analog_dat_F2)
    one_mod_map_info_F3 <- produce_dt_for_map(analog_dat_F3)

    # produce data for donuts

    f_years_F1 = 1 + as.numeric(unlist(strsplit(time_p[1], "_")))[2] - 
                     as.numeric(unlist(strsplit(time_p[1], "_")))[1]

    f_years_F2 = 1 + as.numeric(unlist(strsplit(time_p[2], "_")))[2] - 
                     as.numeric(unlist(strsplit(time_p[2], "_")))[1]

    f_years_F3 = 1 + as.numeric(unlist(strsplit(time_p[3], "_")))[2] - 
                     as.numeric(unlist(strsplit(time_p[3], "_")))[1]

    one_mod_pie_info_F1 <- produce_dt_for_pie_Q4(analog_dt = analog_dat_F1, tgt_fip = target_fip, 
                                                 f_fips=local_cnty_fips, h_fips = usa_cnty_fips, 
                                                 f_years=f_years_F1, h_years=37)

    one_mod_pie_info_F2 <- produce_dt_for_pie_Q4(analog_dt=analog_dat_F2, tgt_fip = target_fip, 
                                                 f_fips=local_cnty_fips, h_fips = usa_cnty_fips, 
                                                 f_years=f_years_F2, h_years=37)

    one_mod_pie_info_F3 <- produce_dt_for_pie_Q4(analog_dt=analog_dat_F3, tgt_fip = target_fip, 
                                                 f_fips=local_cnty_fips, h_fips=usa_cnty_fips, 
                                                 f_years=f_years_F3, h_years=37)


    most_similar_cnty_F1 <- one_mod_pie_info_F1[[2]]
    one_mod_pie_info_F1 <- one_mod_pie_info_F1[[1]]

    most_similar_cnty_F2 <- one_mod_pie_info_F2[[2]]
    one_mod_pie_info_F2 <- one_mod_pie_info_F2[[1]]

    most_similar_cnty_F3 <- one_mod_pie_info_F3[[2]]
    one_mod_pie_info_F3 <- one_mod_pie_info_F3[[1]]

    # Extract name of county of interest for putting in the plots:
    target_cnty_name <- local_fip_cnty_name_map$st_county[local_fip_cnty_name_map$fips==target_fip]
    target_cnty_name <- paste(unlist(strsplit(target_cnty_name, "_"))[2], 
                        unlist(strsplit(target_cnty_name, "_"))[1], sep= ", ")

    titlem_F1 <- paste0(target_cnty_name, 
                        " (", 
                        paste(unlist(strsplit(time_p[1], "_"))[1], 
                              unlist(strsplit(time_p[1], "_"))[2], sep="-"),
                        ", ", model_n, ",", emission, ")" )

    titlem_F2 <- paste0(target_cnty_name, 
                        " (", 
                        paste(unlist(strsplit(time_p[2], "_"))[1], 
                              unlist(strsplit(time_p[2], "_"))[2], sep="-"),
                        ", ", model_n, ",", emission, ")" )

    titlem_F3 <- paste0(target_cnty_name, 
                        " (", 
                        paste(unlist(strsplit(time_p[3], "_"))[1], 
                              unlist(strsplit(time_p[3], "_"))[2], sep="-"),
                        ", ", model_n, ",", emission, ")" )

    most_similar_cnty_F1_fip <- most_similar_cnty_F1
    most_similar_cnty_F2_fip <- most_similar_cnty_F2
    most_similar_cnty_F3_fip <- most_similar_cnty_F3

    most_similar_cnty_F1 <- all_us_fips$st_county[all_us_fips$fips==most_similar_cnty_F1]
    most_similar_cnty_F1 <- paste(unlist(strsplit(most_similar_cnty_F1, "_"))[2], 
                             unlist(strsplit(most_similar_cnty_F1, "_"))[1], sep= ", ")
    
    most_similar_cnty_F2 <- all_us_fips$st_county[all_us_fips$fips==most_similar_cnty_F2]
    most_similar_cnty_F2 <- paste(unlist(strsplit(most_similar_cnty_F2, "_"))[2], 
                             unlist(strsplit(most_similar_cnty_F2, "_"))[1], sep= ", ")

    most_similar_cnty_F3 <- all_us_fips$st_county[all_us_fips$fips==most_similar_cnty_F3]
    most_similar_cnty_F3 <- paste(unlist(strsplit(most_similar_cnty_F3, "_"))[2], 
                             unlist(strsplit(most_similar_cnty_F3, "_"))[1], sep= ", ")

    # Plot the donuts
    assign(x = paste0("pie_", gsub("-", "_", model_n), "_F1"), 
                      value = {plot_the_pie(one_mod_pie_info_F1, titlem_F1, most_similar_cnty_F1)})
    
    assign(x = paste0("pie_", gsub("-", "_", model_n), "_F2"), 
                      value = {plot_the_pie(one_mod_pie_info_F2, titlem_F2, most_similar_cnty_F2)})
    
    assign(x = paste0("pie_", gsub("-", "_", model_n), "_F3"), 
                      value = {plot_the_pie(one_mod_pie_info_F3, titlem_F3, most_similar_cnty_F3)})
    
    # plot geographical maps:
    target_county_map_info <- cnty2 %>% filter(fips == target_fip)
    most_similar_cnty_F1_map_info <- cnty2 %>% filter(fips == most_similar_cnty_F1_fip)
    most_similar_cnty_F2_map_info <- cnty2 %>% filter(fips == most_similar_cnty_F2_fip)
    most_similar_cnty_F3_map_info <- cnty2 %>% filter(fips == most_similar_cnty_F3_fip)
    
    data(county.fips) # Load the county.fips dataset for plotting
    cnty <- map_data("county") # Load the county data from the maps package
    cnty2 <- cnty %>%
             mutate(polyname = paste(region, subregion, sep=",")) %>%
             left_join(county.fips, by="polyname")
    
    assign(x = paste0("map_", gsub("-", "_", model_n), "_F1"), 
           value = {plot_the_map(one_mod_map_info_F1, cnty2, titlem_F1, 
           	                     target_county_map_info, most_similar_cnty_F1_map_info)})

    assign(x = paste0("map_", gsub("-", "_", model_n), "_F2"), 
           value = {plot_the_map(one_mod_map_info_F2, cnty2, titlem_F2,
           	                     target_county_map_info, most_similar_cnty_F2_map_info)})

    assign(x = paste0("map_", gsub("-", "_", model_n), "_F3"), 
           value = {plot_the_map(one_mod_map_info_F3, cnty2, titlem_F3,
           	                     target_county_map_info, most_similar_cnty_F3_map_info)})
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
                          ncol = 3, nrow = 12, common.legend = TRUE)})

}

master_path <- paste0(data_dir, "/plots/")
if (dir.exists(master_path) == F) { dir.create(path = master_path, recursive = T)}


ggsave("plot_16027_ID_Canyon.png", plot_16027, 
       path=master_path, device="png",
       dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

ggsave("plot_41021_OR_Gilliam.png", plot_41021, 
       path=master_path, device="png",
       dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

ggsave("plot_41027_OR_Hood_River.png", plot_41027, 
       path=master_path, device="png",
       dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

 
ggsave("plot_41049_OR_Morrow.png", plot_41049, 
       path=master_path, device="png",
       dpi=300, width=40, height=100, unit="in", limitsize = FALSE)


ggsave("plot_41059_OR_Umatilla.png", plot_41059, 
       path=master_path, device="png",
       dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

ggsave("plot_53001_WA_Adams.png", plot_53001, 
       path=master_path, device="png",
       dpi=300, width=40, height=100, unit="in", limitsize = FALSE)


ggsave("plot_53005_WA_Benton.png", plot_53005, 
       path=master_path, device="png",
       dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

ggsave("plot_53007_WA_Chelan_.png", plot_53007, 
       path=master_path, device="png",
       dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

ggsave("plot_53013_WA_Columbia.png", plot_53013, 
       path=master_path, device="png",
       dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

ggsave("plot_53017_WA_Douglas.png", plot_53017, 
       path=master_path, device="png",
       dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

ggsave("plot_53021_WA_Franklin.png", plot_53021, 
       path=master_path, device="png",
       dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

ggsave("plot_53025_WA_Grant.png", plot_53025, 
       path=master_path, device="png",
       dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

ggsave("plot_53037_WA_Kittitas.png", plot_53037,
       path=master_path, device="png",
       dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

ggsave("plot_53039_WA_Klickitat.png", plot_53039, 
       path=master_path, device="png",
       dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

ggsave("plot_53047_WA_Okanogan.png", plot_53047, 
       path=master_path, device="png",
       dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

ggsave("plot_53071_WA_Walla_Walla.png", plot_53071, 
       path=master_path, device="png",
       dpi=300, width=40, height=100, unit="in", limitsize = FALSE)

ggsave("plot_53077_WA_Yakima.png", plot_53077, 
       path=master_path, device="png",
       dpi=300, width=40, height=100, unit="in", limitsize = FALSE)



