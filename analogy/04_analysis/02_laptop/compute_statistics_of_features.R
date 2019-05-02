######################################################################
rm(list=ls())
library(lubridate)
library(ggpubr)
library(purrr)
library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)
library(maps)

options(digit=9)
options(digits=9)

##########################################################################
####
####         functions here
####
##########################################################################


##########################################################################

ppp <- "/Users/hn/Documents/GitHub/Kirti/codling_moth/code/parameters/"

min_fips <- data.table(read.csv(paste0(ppp, "us_county_lat_long.csv"), header=T, as.is=T))
min_fips$location <- paste(min_fips$vicclat, min_fips$vicclon, sep="_")
min_fips <- within(min_fips, remove(vicid, state_fips, vic_km2, vicclat, vicclon))

us_list_girid <- data.table(read.table(paste0(ppp, "all_us_locations_list.txt")))
local_list_girid <- data.table(read.table(paste0(ppp, "local_list.txt")))

setnames(us_list_girid, old=c("V1"), new=c("location"))
setnames(local_list_girid, old=c("V1"), new=c("location"))

us_list_girid$location <- as.character(us_list_girid$location)
local_list_girid$location <- as.character(local_list_girid$location)

us_list_girid <- left_join(us_list_girid, min_fips)
local_list_girid <- left_join(local_list_girid, min_fips)

missing_locations_girids <- local_list_girid %>% filter(!( location %in% us_list_girid$location ))


#              location  fips state           county
# 1 43.53125_-116.59375 16027    ID    Canyon County
# 2 45.65625_-121.15625 53039    WA Klickitat County
# 3 45.71875_-120.34375 53039    WA Klickitat County
# 4 45.96875_-119.34375 53005    WA    Benton County
# 5 45.96875_-119.96875 53039    WA Klickitat County
# 6 46.03125_-119.84375 53005    WA    Benton County
# 7 46.03125_-119.90625 53039    WA Klickitat County
# 8 46.28125_-119.09375 53021    WA  Franklin County
# 9 46.59375_-119.78125 53005    WA    Benton County

##########################################################################
##########################################################################
####
####           global Files
####
##########################################################################

main_dir <- "/Users/hn/Desktop/Desktop/Kirti/check_point/analogs/features/"
param_dir <- "/Users/hn/Documents/GitHub/Kirti/analogy/parameters/"

local_cnty_fips <- "local_county_fips.csv"
usa_cnty_fips <- "all_us_1300_county_fips_locations.csv"
local_fip_cnty_name_map <- "17_counties_fips_unique.csv"

local_cnty_fips <- data.table(read.csv(paste0(param_dir, local_cnty_fips), header=T, sep=",", as.is=T))
usa_cnty_fips <- data.table(read.csv(paste0(param_dir, usa_cnty_fips), header=T, sep=",", as.is=T))
local_fip_cnty_name_map <- data.table(read.csv(paste0(param_dir, local_fip_cnty_name_map), 
                                               header=T, sep=",", as.is=T))

missing_analag_param <- local_cnty_fips %>% filter(!(location %in% usa_cnty_fips$location))

local_fips <- unique(local_cnty_fips$fips)

us_dir <- file.path(main_dir, "usa", "/")
local_dir <- file.path(main_dir, "local", "/")

usa_cnty_fips <- within(usa_cnty_fips, remove(st_county))
local_cnty_fips <- within(local_cnty_fips, remove(st_county))

us_feat <- data.table(readRDS(paste0(us_dir, "all_data_usa.rds")))
us_feat <- within(us_feat, remove(mean_escaped_Gen4, treatment))

bcc <- data.table(readRDS(paste0(local_dir, "feat_bcc-csm1-1-m_rcp85.rds")))
bnu <- data.table(readRDS(paste0(local_dir, "feat_BNU-ESM_rcp85.rds")))
can <- data.table(readRDS(paste0(local_dir, "feat_CanESM2_rcp85.rds")))
cnr <- data.table(readRDS(paste0(local_dir, "feat_CNRM-CM5_rcp85.rds")))
gfg <- data.table(readRDS(paste0(local_dir, "feat_GFDL-ESM2G_rcp85.rds")))
gfm <- data.table(readRDS(paste0(local_dir, "feat_GFDL-ESM2M_rcp85.rds")))

bcc <- within(bcc, remove(mean_escaped_Gen4, treatment))
bnu <- within(bnu, remove(mean_escaped_Gen4, treatment))
can <- within(can, remove(mean_escaped_Gen4, treatment))
cnr <- within(cnr, remove(mean_escaped_Gen4, treatment))
gfg <- within(gfg, remove(mean_escaped_Gen4, treatment))
gfm <- within(gfm, remove(mean_escaped_Gen4, treatment))

us_feat <- left_join(us_feat, usa_cnty_fips)

bcc <- left_join(bcc, min_fips)
bnu <- left_join(bnu, min_fips)
can <- left_join(can, min_fips)
cnr <- left_join(cnr, min_fips)
gfg <- left_join(gfg, min_fips)
gfm <- left_join(gfm, min_fips)

hist_chelan <- us_feat %>% filter(fips == 53007)
gfm_chelan <- gfm %>% filter(fips == 53007)

length(unique(hist_chelan$location))
length(unique(gfm_chelan$location))


gfm_locations <- unique(gfm$location)
us_locations <- unique(us_feat$location)


##################################################################################
min_fips_California <- min_fips %>% filter(state == "CA")
us_list_girid_california <- us_list_girid %>% filter(state == "CA")


##################################################################################

#                               summary statistics

##################################################################################

us_feat %>%
group_by(location, year) %>%
summarise_at(.funs = funs(mean(., na.rm=TRUE)), vars(CumDDinF))%>% 
data.table()


historical_summary <- us_feat %>%
                      group_by(fips) %>%
                      summarise_at(.funs = funs(mean(., na.rm=TRUE)), 
                                    vars(medianDoY, NumLarvaGens_Aug, 
                                         mean_escaped_Gen1, mean_escaped_Gen2, mean_escaped_Gen3,
                                         mean_precip, mean_gdd)) %>% 
                      data.table()


bcc_F3 <- bcc %>% filter(year >= 2076)
bnu_F3 <- bnu %>% filter(year >= 2076)
can_F3 <- can %>% filter(year >= 2076)
cnr_F3 <- cnr %>% filter(year >= 2076)
gfg_F3 <- gfg %>% filter(year >= 2076)
gfm_F3 <- gfm %>% filter(year >= 2076)

bcc_summary <- bcc_F3 %>%
               group_by(fips) %>%
               summarise_at(.funs = funs(mean(., na.rm=TRUE)), 
                            vars(medianDoY, NumLarvaGens_Aug, 
                                 mean_escaped_Gen1, mean_escaped_Gen2, mean_escaped_Gen3,
                                 mean_precip, mean_gdd)) %>% 
               data.table()

bnu_summary <- bnu_F3 %>%
               group_by(fips) %>%
               summarise_at(.funs = funs(mean(., na.rm=TRUE)), 
                            vars(medianDoY, NumLarvaGens_Aug, 
                                 mean_escaped_Gen1, mean_escaped_Gen2, mean_escaped_Gen3,
                                 mean_precip, mean_gdd)) %>% 
               data.table()


can_summary <- can_F3 %>%
               group_by(fips) %>%
               summarise_at(.funs = funs(mean(., na.rm=TRUE)), 
                            vars(medianDoY, NumLarvaGens_Aug, 
                                 mean_escaped_Gen1, mean_escaped_Gen2, mean_escaped_Gen3,
                                 mean_precip, mean_gdd)) %>% 
               data.table()

cnr_summary <- cnr_F3 %>%
               group_by(fips) %>%
               summarise_at(.funs = funs(mean(., na.rm=TRUE)), 
                            vars(medianDoY, NumLarvaGens_Aug, 
                                 mean_escaped_Gen1, mean_escaped_Gen2, mean_escaped_Gen3,
                                 mean_precip, mean_gdd)) %>% 
               data.table()

gfg_summary <- gfg_F3 %>%
               group_by(fips) %>%
               summarise_at(.funs = funs(mean(., na.rm=TRUE)), 
                            vars(medianDoY, NumLarvaGens_Aug, 
                                 mean_escaped_Gen1, mean_escaped_Gen2, mean_escaped_Gen3,
                                 mean_precip, mean_gdd)) %>% 
               data.table()


gfm_summary <- gfm_F3 %>%
               group_by(fips) %>%
               summarise_at(.funs = funs(mean(., na.rm=TRUE)), 
                            vars(medianDoY, NumLarvaGens_Aug, 
                                 mean_escaped_Gen1, mean_escaped_Gen2, mean_escaped_Gen3,
                                 mean_precip, mean_gdd)) %>% 
               data.table()




###############

bcc_summary_chelan <- bcc_summary %>% filter(fips== 53007)

# Chelan summary in bcc :

 # fips  medianDoY   NumLarvaGens_Aug  mean_escaped_Gen1   mean_escaped_Gen2  mean_escaped_Gen3   mean_precip   mean_gdd
 # 53007 98.4913043       2.71219194        47.3659847        64.1206714        11.3897521         358.700217  4120.23637

keycol <- c("fips")
setorderv(historical_summary, keycol)

historical_summary_california <- historical_summary[2:26, ]





