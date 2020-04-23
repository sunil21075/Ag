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

main_in <- "/Users/hn/Documents/01_research_data/chilling/01_data/02/"
files_name = c("sept_summary_comp.rds")

datas = data.table(readRDS(paste0(main_in, "sept_summary_comp.rds")))

datas <- datas %>% filter(time_period != "1950-2005") %>% data.table()
datas <- datas %>% filter(time_period != "2006-2025") %>% data.table()

######
###### Keep Locations of interest
######

datas <- datas %>% 
         filter(location %in% LocationGroups_NoMontana$location) %>% 
         data.table()


needed_cols = c("chill_season", "year", "sum_A1", "sum_J1", "sum_M1", "sum_F1",
                "model", "emission", "lat", "long", "time_period")

datas = subset(datas, select=needed_cols)


datas$time_period[datas$time_period == "1979-2015"] <- "Historical"
time_periods = c("Historical", "2026-2050", "2051-2075", "2076-2099")
datas$time_period = factor(datas$time_period, levels = time_periods, order=T)
datas$emission[datas$emission=="rcp85"] = "RCP 8.5"
datas$emission[datas$emission=="rcp45"] = "RCP 4.5"
datas$emission[datas$emission=="historical"] = "Historical"


datas_f <- datas %>% filter(time_period != "Historical")

datas_h_rcp85 <- datas %>% filter(time_period == "Historical")
datas_h_rcp45 <- datas %>% filter(time_period == "Historical")
datas_h_rcp85$emission = "RCP 8.5"
datas_h_rcp45$emission = "RCP 4.5"
datas = rbind(datas_f, datas_h_rcp45, datas_h_rcp85)


quan_per_loc_period_model_jan <- datas %>% 
                                 group_by(time_period, lat, long, emission, model) %>%
                                 summarise(quan_90 = quantile(sum_J1, probs = 0.1)) %>%
                                 data.table()

quan_per_loc_period_model_feb <- datas %>% 
                                 group_by(time_period, lat, long, emission, model) %>%
                                 summarise(quan_90 = quantile(sum_F1, probs = 0.1)) %>%
                                 data.table()

# quan_per_loc_period_model_mar <- datas %>% 
#                                  group_by(time_period, lat, long, emission, model) %>%
#                                  summarise(quan_90 = quantile(sum_M1, probs = 0.1)) %>%
#                                  data.table()

quan_per_loc_period_model_apr <- datas %>% 
                                 group_by(time_period, lat, long, emission, model) %>%
                                 summarise(quan_90 = quantile(sum_A1, probs = 0.1)) %>%
                                 data.table()


################
################   Median over models
################
median_quan_per_loc_period_model_jan1st <- quan_per_loc_period_model_jan %>%
                                        group_by(time_period, lat, long, emission) %>%
                                        summarise(median_over_model = median(quan_90)) %>%
                                        data.table()

median_quan_per_loc_period_model_feb1st <- quan_per_loc_period_model_feb %>%
                                        group_by(time_period, lat, long, emission) %>%
                                        summarise(median_over_model = median(quan_90)) %>%
                                        data.table()

# median_quan_per_loc_period_model_mar1st <- quan_per_loc_period_model_mar %>%
#                                       group_by(time_period, lat, long, emission) %>%
#                                       summarise(median_over_model = median(quan_90)) %>%
#                                       data.table()

median_quan_per_loc_period_model_apr1st <- quan_per_loc_period_model_apr %>%
                                        group_by(time_period, lat, long, emission) %>%
                                        summarise(median_over_model = median(quan_90)) %>%
                                        data.table()

#####################################################################################################
core_path = "/Users/hn/Documents/00_GitHub/Ag/chilling/chill_core.R"
plot_core_path = "/Users/hn/Documents/00_GitHub/Ag/chilling/chill_plot_core.R"
source(core_path)
source(plot_core_path)

the_SC_map <- function(dt, scenario_name, color_col, legend_label) {
  states <- map_data("state")
  states_cluster <- subset(states, region %in% c("oregon", "washington", "idaho"))

  minn <- min(dt[, get(color_col)])
  maxx <- max(dt[, get(color_col)])
  dt <- dt %>%
        filter(emission %in% c("Historical", scenario_name)) %>%
        data.table()
  dt$emission <- scenario_name
  
  dt %>% ggplot() +
         geom_polygon(data = states_cluster, aes(x = long, y = lat, group = group),
                     fill = "grey", color = "black") +
         # aes_string to allow naming of column in function 
         geom_point(aes_string(x = "long", y = "lat",
                               color = color_col), alpha = 0.4, size=0.1) +
         scale_color_viridis_c(option = "plasma", name = legend_label, direction = -1,
                               limits = c(minn, maxx),
                               breaks = pretty_breaks(n = 4)) +
         coord_fixed(xlim = c(-124.5, -111.4),  ylim = c(41, 50.5), ratio = 1.3) +
         facet_grid(~ emission ~ time_period) + # facet_wrap came with nrow = 1
         theme(axis.title.y = element_blank(),
               axis.title.x = element_blank(),
               axis.ticks.y = element_blank(), 
               axis.ticks.x = element_blank(),
                axis.text.x = element_blank(),
               axis.text.y = element_blank(),
               panel.grid.major = element_line(size = 0.1),
               legend.position="bottom", 
               strip.text = element_text(size=12, face="bold"),
               plot.margin = margin(t=-0.5, r=0.2, b=-0.5, l=0.2, unit = 'cm'),
               legend.title = element_blank()
               )
}


median_map_jan1st = the_SC_map(dt = median_quan_per_loc_period_model_jan1st, 
                               scenario_name= "RCP 8.5", 
                               color_col="median_over_model", 
                               legend_label="Jan.")

median_map_feb1st = the_SC_map(dt=median_quan_per_loc_period_model_feb1st, 
                               scenario_name= "RCP 8.5", 
                               color_col="median_over_model", 
                               legend_label="Feb.")

# median_map_mar1st = the_SC_map(dt=median_quan_per_loc_period_model_mar1st, scenario_name= "RCP 8.5", color_col="median_over_model", due="Mar.")

median_map_apr1st = the_SC_map(dt=median_quan_per_loc_period_model_apr1st, 
                               scenario_name= "RCP 8.5", 
                               color_col="median_over_model", 
                               legend_label="Mar.")

#####################################################################################################

plot_base <- paste0("/Users/hn/Documents/00_GitHub/Ag_papers/Chill_Paper/figures/SC_map/")
if (dir.exists(plot_base) == F) {dir.create(path = plot_base, recursive = T)}

#####################################################################################################

plt_width <- 10
plt_height <- 3.3
qual = 450

ggsave(filename = paste0("SC_sept1_till_Jan1_85_median_of_models.png"), 
       plot=median_map_jan1st, 
       width=plt_width, height=plt_height, units="in", 
       dpi=qual, device="png", 
       path=plot_base)

ggsave(filename = paste0("SC_sept1_till_Feb1_85_median_of_models.png"), 
       plot=median_map_feb1st, 
       width=plt_width, height=plt_height, units="in", 
       dpi=qual, device="png", 
       path=plot_base)


ggsave(filename = paste0("SC_sept1_till_Apr1_85_median_of_models.png"), 
       plot=median_map_apr1st, 
       width=plt_width, height=plt_height, units="in", 
       dpi=qual, device="png", 
       path=plot_base)





