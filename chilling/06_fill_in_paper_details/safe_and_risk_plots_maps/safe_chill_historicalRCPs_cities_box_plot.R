rm(list=ls())
library(data.table)
library(dplyr)
library(ggmap)
library(ggplot2)
options(digit=9)
options(digits=9)

source_path_core = "/Users/hn/Documents/00_GitHub/Ag/chilling/chill_core.R"
source_path_plot = "/Users/hn/Documents/00_GitHub/Ag/chilling/chill_plot_core.R"
source(source_path_plot)
source(source_path_core)

##########################################################################################
###                                                                                    ###
###                                    Driver                                          ###
###                                                                                    ###
##########################################################################################
plot_dir <- "/Users/hn/Documents/00_GitHub/Ag_papers/Chill_Paper/figures/safe_chill_boxplots/"
param_dir <- "/Users/hn/Documents/00_GitHub/Ag/chilling/parameters/"
limited_locations <- read.csv(paste0(param_dir, "limited_locations.csv"), header=T, as.is=T)
limited_locations$location <- paste0(limited_locations$lat, "_", limited_locations$long)

main_in <- "/Users/hn/Documents/01_research_data/Ag_check_point/chilling/01_data/02/"
files_name = c("sept_summary_comp.rds")

datas = data.table(readRDS(paste0(main_in, "sept_summary_comp.rds")))
datas <- datas %>% filter(time_period != "1950-2005") %>% data.table()
datas <- datas %>% filter(time_period != "2006-2025") %>% data.table()

###### Keep Locations of interest
datas <- datas %>% filter(location %in% limited_locations$location) %>% data.table()

information = produce_data_4_plots_w_noly_observed(datas)

#
# add city to the data
#
limited_locations <- within(limited_locations, remove(lat, long))

information[[1]] <- dplyr::left_join(x = information[[1]], y = limited_locations, by = "location")
ch_start = "Sept. 1"
#
# plot size
#
box_width = 10
box_height = 8

source_path_plot = "/Users/hn/Documents/00_GitHub/Ag/chilling/chill_plot_core.R"
source(source_path_plot)

# Eugene_safe_chill_sept_Apr_RCP_85

for (ct in unique(limited_locations$city)){
    for (em in c("RCP 4.5", "RCP 8.5")){
        curr_dt = information[[1]] %>% filter(city == ct & emission == em)
        output_name = paste0(gsub(" ", "_", ct), "_safe_chill_sept_Apr_", gsub('\\.',  "", gsub(" ", "_", em)), ".png")
        safe_apr <- safe_box_plot_per_city(data = curr_dt, due="Apr.", chill_start= ch_start)
        ggsave(output_name, safe_apr, path=plot_dir, width=5, height=5, unit="in", dpi=400)
    }
}



