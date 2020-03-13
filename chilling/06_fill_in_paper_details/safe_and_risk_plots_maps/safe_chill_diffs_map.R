rm(list=ls())
library(data.table)
library(dplyr)
library(ggmap)
library(ggplot2)
options(digit=9)
options(digits=9)

source_path_plot = "/Users/hn/Documents/00_GitHub/Ag/chilling/chill_plot_core.R"
source_path_core = "/Users/hn/Documents/00_GitHub/Ag/chilling/chill_core.R"
source(source_path_plot)
source(source_path_core)

##########################################################################################
###                                                                                    ###
###                                    Driver                                          ###
###                                                                                    ###
##########################################################################################
plot_dir <- "/Users/hn/Documents/00_GitHub/Ag_papers/Chill_Paper/figures/safe_chill_boxplots/"
param_dir <- "/Users/hn/Documents/00_GitHub/Ag/chilling/parameters/"
LocationGroups_NoMontana <- read.csv(paste0(param_dir, "LocationGroups_NoMontana.csv"), header=T, as.is=T)

main_in = "/Users/hn/Documents/01_research_data/Ag_check_point/chilling/01_data/02/"
files_name = c("sept_summary_comp.rds")

datas = data.table(readRDS(paste0(main_in, "sept_summary_comp.rds")))
datas <- datas %>% filter(time_period != "1950-2005") %>% data.table()
datas <- datas %>% filter(time_period != "2006-2025") %>% data.table()

###### Keep Locations of interest
datas <- datas %>% filter(location %in% LocationGroups_NoMontana$location) %>% data.table()

needed_cols = c("chill_season", "year", "sum_A1", "model", "emission", "location", "time_period")
datas = subset(datas, select=needed_cols)

datas$time_period[datas$time_period == "1979-2015"]  <- "Historical"
time_periods = c("Historical", "2026-2050", "2051-2075", "2076-2099")

datas$time_period = factor(datas$time_period, levels = time_periods, order=T)
datas$emission[datas$emission=="rcp85"] = "RCP 8.5"
datas$emission[datas$emission=="rcp45"] = "RCP 4.5"
datas$emission[datas$emission=="historical"] = "Historical"
#
# compute safe chill first
#
quan_per_loc_period_model_apr <- datas %>% 
                                 group_by(time_period, emission, model, location) %>%
                                 summarise(quan_10 = quantile(sum_A1, probs = 0.1)) %>%
                                 data.table()

# compute differences between each model and observed data
diffs <- quan_per_loc_period_model_apr %>%
         group_by(location) %>%
         mutate(SC_diff = quan_10 - quan_10[time_period == "Historical"])%>%
         data.table()

# remove the historical data itself for which diffs. are zeros
diffs <- diffs %>% filter(model != "observed")

diffs$hist_SC <- diffs$quan_10 + diffs$SC_diff

diffs$perc_diff <- (diffs$SC_diff * 100) / (diffs$hist_SC)

diffs_median <- diffs %>% 
                group_by(location, time_period, emission) %>%
                summarise(SC_diff_median = median(perc_diff)) %>%
                data.table()

diffs_median$SC_diff_median[diffs_median$SC_diff_median>150] <- diffs_median$SC_diff_median[diffs_median$SC_diff_median>150]* (-1)
plot_base <- paste0("/Users/hn/Documents/00_GitHub/Ag_papers/Chill_Paper/figures/SC_map_diff")
if (dir.exists(plot_base) == F) {dir.create(path = plot_base, recursive = T)}

core_path = "/Users/hn/Documents/00_GitHub/Ag/chilling/chill_core.R"
plot_core_path = "/Users/hn/Documents/00_GitHub/Ag/chilling/chill_plot_core.R"
source(core_path)
source(plot_core_path)

both_rcps_map <- diff_SC_map(data = diffs_median, color_col = "SC_diff_median")
ggsave(filename = paste0("SC_diff_perc_Sept_Apr_centered.png"), 
       plot=both_rcps_map, 
       width=7.5, height=5.7, units="in", 
       dpi=600, device="png", path=plot_base)


diffs_median_85 <- diffs_median %>% filter(emission=="RCP 8.5")

core_path = "/Users/hn/Documents/00_GitHub/Ag/chilling/chill_core.R"
plot_core_path = "/Users/hn/Documents/00_GitHub/Ag/chilling/chill_plot_core.R"
source(core_path)
source(plot_core_path)

rcp85_map <- diff_SC_map_one_emission(data = diffs_median_85, color_col = "SC_diff_median")
ggsave(filename = paste0("SC_diff_perc_Sept_Apr_centered_RCP85.png"), 
       plot=rcp85_map, 
       width=7.5, height=3.6, units="in", 
       dpi=600, device="png", path=plot_base)

