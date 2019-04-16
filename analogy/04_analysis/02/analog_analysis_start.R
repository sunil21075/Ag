library(lubridate)
library(purrr)
library(tidyverse)
library(data.table)
library(dplyr)

options(digit=9)
options(digits=9)


data_dir <- "/Users/hn/Desktop/Desktop/Kirti/check_point/analogs/w_gen_w_prec/500/"
param_dir <- "/Users/hn/Documents/GitHub/Kirti/analogy/parameters/"


file <- "all_close_analogs_unique_2026_2050_rcp45.rds"
local_county_fips <- "local_site_counties.csv"
usa_county_fips <- "all_us_1300_county_fips_unique.csv"

dt <- data.table(readRDS(paste0(data_dir, file)))
local_county_fips <- data.table(read.csv(paste0(param_dir, local_county_fips), header=T, sep=","))
usa_county_fips <- data.table(read.csv(paste0(param_dir, usa_county_fips), header=T, sep=","))

output <- find_unique_county_to_county_freq(dt, local_county_fips, usa_county_fips)

dt_aggregation <- output[[1]]
dt_agg_bcc_m <- output[[2]]
dt_agg_BNU  <- output[[3]]
dt_agg_CanESM2 <- output[[4]]
dt_agg_CNRM_CM5 <- output[[5]]
dt_agg_GFDLG <- output[[6]]
dt_agg_GFDLM <- output[[7]]


one_county_one_model <- dt_agg_bcc_m %>% filter(query_fips==53047)
model_name <- unique(one_county_one_model$ClimateScenario)


library(ggplot2)
library(maps)

data(county.fips) # Load the county.fips dataset

cnty <- map_data("county") # Load the county data from the maps package
cnty2 <- cnty %>%
         mutate(polyname = paste(region, subregion,sep=",")) %>%
         left_join(county.fips, by="polyname")

cnty2_one_county_one_model <- left_join(one_county_one_model, cnty2, by=c("analog_fips" = "fips"))

ggplot(cnty2_one_county_one_model, aes(long, lat, group = group)) + 
geom_polygon(aes(fill = freq), colour = rgb(1,1,1,0.2))  +
coord_quickmap()



















