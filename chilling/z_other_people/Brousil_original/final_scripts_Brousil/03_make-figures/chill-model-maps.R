# Script for creating chill accumulation & threshold maps.
# Intended to work with create-model-maps.sh script.

# 1. Load packages --------------------------------------------------------

library(ggmap)
library(ggpubr)
library(plyr)
library(lubridate)
library(purrr)
library(scales)
library(tidyverse)

# 2. Pull data from current directory -------------------------------------

# print to check
getwd()

the_dir <- dir()

# remove filenames that aren't data
the_dir <- the_dir[grep(pattern = "chill-data-summary",
                        x = the_dir)]

# Data for maps
the_dir_stats <- the_dir[grep(pattern = "chill-data-summary-stats",
                              x = the_dir)]
# Data for plots
the_dir_summary <- the_dir[-grep(pattern = "chill-data-summary-stats",
                                 x = the_dir)]

# Compile the summary stats files for mapping
stats_comp <- lapply(the_dir_stats, read.table, header = T)
stats_comp <- do.call(bind_rows, stats_comp)

str(stats_comp)
print(" ")
stats_comp %>% select(model, scenario) %>% unique()

stats_comp <- stats_comp %>%
  mutate(year = replace_na(year, "historical"),
         year = factor(x = year, levels = c("historical", "2040",
                                            "2060", "2080"),
                       ordered = T)) # fill in NA for years

str(stats_comp)

# Remove incomplete model runs
stats_comp <- stats_comp[-grep(x = stats_comp$model, pattern = "incomplete"), ]

str(stats_comp)

# Take a mean across models
stats_comp_ensemble <- stats_comp %>%
  filter(model != "observed") %>%
  group_by(lat, long, scenario, year) %>%
  summarize_if(.predicate = is.numeric, mean)

str(stats_comp_ensemble)

# Compile the data files for plotting
summary_comp <- lapply(the_dir_summary, read.table, header = T)
summary_comp <- do.call(bind_rows, summary_comp)

str(summary_comp)

# Remove incomplete model runs
summary_comp <- summary_comp[-grep(x = summary_comp$model, pattern = "incomplete"),]

str(summary_comp)

# 3. Mapping --------------------------------------------------------------

# Pull base layers
states <- map_data("state")
states_cluster <- subset(states, region %in% c("oregon", "washington", "idaho"))

# Base map for following figures
#base_map <- ggplot() +
#  geom_polygon(data = states_cluster, aes(x = long, y = lat, group = group),
#               fill = "grey", color = "black")


# 3a. Map functions -------------------------------------------------------

# Create a map of observed historical data. I'm not clear exactly how ggarrange()
# works with common.legend to harmonize legends between plots. To be safe, I
# make sure all plots have same limits up front and thus make a new observed
# hist map for each time ggarrange is called, using function below.

observed_hist_map <- function(min, max, month_col) {
  
  stats_comp %>%
    filter(model == "observed") %>%
    ggplot() +
    geom_polygon(data = states_cluster, aes(x = long, y = lat, group = group),
                 fill = "grey", color = "black") +
    geom_point(aes_string(x = "long", y = "lat",
                          color = month_col), alpha = 0.4) +
    scale_color_viridis_c(option = "plasma", name = "Median", direction = -1,
                          limits = c(min, max),
                          breaks = pretty_breaks(n = 4)) +
    coord_fixed(xlim = c(-124.5, -111.4),  ylim = c(41, 50.5), ratio = 1.3) +
    facet_wrap(~ year, nrow = 1) +
    ggtitle("Observed historical")
  
}

# Make a function to plot the future scenarios. Note that it requires the main
# data frame to be called stats_comp and the base map to be called
#states_cluster.

model_map <- function(model_name, scenario_name, month_col, min, max) {
  
  stats_comp %>%
    filter(model == model_name,
           scenario == scenario_name | scenario == "historical") %>%
    ggplot() +
    geom_polygon(data = states_cluster, aes(x = long, y = lat, group = group),
                 fill = "grey", color = "black") +
    # aes_string to allow naming of column in function 
    geom_point(aes_string(x = "long", y = "lat",
                          color = month_col), alpha = 0.4) +
    scale_color_viridis_c(option = "plasma", name = "Median", direction = -1,
                          limits = c(min, max),
                          breaks = pretty_breaks(n = 4)) +
    coord_fixed(xlim = c(-124.5, -111.4),  ylim = c(41, 50.5), ratio = 1.3) +
    facet_wrap(~ year, nrow = 1) +
    ggtitle(paste0(model_name))
  
}

# A function to make a map from the averaged dataset. Note that it uses the
# ensemble data frame.

ensemble_map <- function(scenario_name, month_col, min, max) {
  
  stats_comp_ensemble %>%
        filter(scenario == scenario_name | scenario == "historical") %>%
        ggplot() +
        geom_polygon(data = states_cluster, aes(x = long, y = lat, group = group),
                     fill = "grey", color = "black") +
        # aes_string to allow naming of column in function 
        geom_point(aes_string(x = "long", y = "lat",
                              color = month_col), alpha = 0.4) +
        scale_color_viridis_c(option = "plasma", name = "Median", direction = -1,
                              limits = c(min, max),
                              breaks = pretty_breaks(n = 4)) +
        coord_fixed(xlim = c(-124.5, -111.4),  ylim = c(41, 50.5), ratio = 1.3) +
        facet_wrap(~ year, nrow = 1) +
        ggtitle("Ensemble means")
  
}


# 3b. RCP45 monthly accum figs --------------------------------------------

# January, rcp45
# Going to make a common scale using min/max of all models in Jan @ rcp45 OR
# either modeled or observed historical
df_45 <- filter(stats_comp, scenario == "rcp45" | scenario == "historical")

str(df_45)
head(df_45)

accum_jan45_min <- min(df_45$median_J1)
accum_jan45_max <- max(df_45$median_J1)

# Create and automatically assign a map object for each model
for(h in unique(stats_comp$model)) {
  
  assign(x = paste(gsub(pattern = "-", replacement = "_", x = h), "map",
                   "jan45", sep="_"),
         value =
         {
           model_map(model = h, scenario_name = "rcp45", month_col = "median_J1",
                     min = accum_jan45_min, max = accum_jan45_max)
         }
  )
  
}

# Separately created maps for observed historical and ensemble mean
observed_map_jan45 <- observed_hist_map(min = accum_jan45_min, max = accum_jan45_max,
                                        month_col = "median_J1")

ensemble_map_jan45 <- ensemble_map(scenario_name = "rcp45", 
                                   month_col = "median_J1", min = accum_jan45_min,
                                   max = accum_jan45_max)

print("test 1")
ls()

# Need to add historical observed to this:
accum_jan45_figs <- ggarrange(plotlist = list(observed_map_jan45,
                                              ensemble_map_jan45,
                                              bcc_csm1_1_m_map_jan45,
                                              bcc_csm1_1_map_jan45,
                                              BNU_ESM_map_jan45,
                                              CanESM2_map_jan45,
                                              CCSM4_map_jan45, 
                                              CNRM_CM5_map_jan45,
                                              CSIRO_Mk3_6_0_map_jan45,
                                              GFDL_ESM2G_map_jan45,
                                              GFDL_ESM2M_map_jan45,
                                              HadGEM2_CC365_map_jan45,
                                              HadGEM2_ES365_map_jan45,
                                              inmcm4_map_jan45,
                                              IPSL_CM5A_LR_map_jan45, 
                                              IPSL_CM5A_MR_map_jan45,
                                              IPSL_CM5B_LR_map_jan45,
                                              MIROC_ESM_CHEM_map_jan45,
                                              MIROC5_map_jan45, 
                                              MRI_CGCM3_map_jan45,
                                              NorESM1_M_map_jan45),
                              ncol = 2, nrow = 11,
                              common.legend = TRUE)


print("test 2")
ls()

accum_jan45_figs<- annotate_figure(p = accum_jan45_figs,
                   top = text_grob(label = "Median accumulated chill units by Jan 1 under rcp 45",
                                   face = "bold", size = 18))

print("test 3")
ls()

ggsave(filename = "accum-jan45.png", plot = accum_jan45_figs, device = "png",
       width = 15, height = 40, units = "in")

rm(list = ls(pattern = "jan45"))



# February, rcp 45
accum_feb45_min <- min(df_45$median_F1)
accum_feb45_max <- max(df_45$median_F1)


for(h in unique(stats_comp$model)) {
  
  assign(x = paste(gsub(pattern = "-", replacement = "_", x = h), "map",
                   "feb45", sep="_"),
         value =
         {
           model_map(model = h, scenario_name = "rcp45", month_col = "median_F1",
                     min = accum_feb45_min, max = accum_feb45_max)
         }
  )
  
}


observed_map_feb45 <- observed_hist_map(min = accum_feb45_min, max = accum_feb45_max,
                                        month_col = "median_F1")

ensemble_map_feb45 <- ensemble_map(scenario_name = "rcp45", 
                                   month_col = "median_F1", min = accum_feb45_min,
                                   max = accum_feb45_max)

# Need to add historical observed to this:
accum_feb45_figs <- ggarrange(plotlist = list(observed_map_feb45,
                                              ensemble_map_feb45,
                                              bcc_csm1_1_m_map_feb45,
                                              bcc_csm1_1_map_feb45,
                                              BNU_ESM_map_feb45,
                                              CanESM2_map_feb45,
                                              CCSM4_map_feb45,
                                              CNRM_CM5_map_feb45,
                                              CSIRO_Mk3_6_0_map_feb45,
                                              GFDL_ESM2G_map_feb45,
                                              GFDL_ESM2M_map_feb45,
                                              HadGEM2_CC365_map_feb45,
                                              HadGEM2_ES365_map_feb45,
                                              inmcm4_map_feb45,
                                              IPSL_CM5A_LR_map_feb45, 
                                              IPSL_CM5A_MR_map_feb45,
                                              IPSL_CM5B_LR_map_feb45,
                                              MIROC_ESM_CHEM_map_feb45,
                                              MIROC5_map_feb45,
                                              MRI_CGCM3_map_feb45,
                                              NorESM1_M_map_feb45),
                              ncol = 2, nrow = 11,
                              common.legend = TRUE)

accum_feb45_figs<- annotate_figure(p = accum_feb45_figs,
                   top = text_grob(label = "Median accumulated chill units by Feb 1 under rcp 45",
                                   face = "bold", size = 18))

ggsave(filename = "accum-feb45.png", plot = accum_feb45_figs, device = "png",
       width = 15, height = 40, units = "in")

rm(list = ls(pattern = "feb45"))



# March, rcp 45
accum_mar45_min <- min(df_45$median_M1)
accum_mar45_max <- max(df_45$median_M1)


for(h in unique(stats_comp$model)) {
  
  assign(x = paste(gsub(pattern = "-", replacement = "_", x = h), "map",
                   "mar45", sep="_"),
         value =
         {
           model_map(model = h, scenario_name = "rcp45", month_col = "median_M1",
                     min = accum_mar45_min, max = accum_mar45_max)
         }
  )
  
}


observed_map_mar45 <- observed_hist_map(min = accum_mar45_min, max = accum_mar45_max,
                                        month_col = "median_M1")

ensemble_map_mar45 <- ensemble_map(scenario_name = "rcp45", 
                                   month_col = "median_M1", min = accum_mar45_min,
                                   max = accum_mar45_max)

# Need to add historical observed to this:
accum_mar45_figs <- ggarrange(plotlist = list(observed_map_mar45,
                                              ensemble_map_mar45,
                                              bcc_csm1_1_m_map_mar45,
                                              bcc_csm1_1_map_mar45,
                                              BNU_ESM_map_mar45,
                                              CanESM2_map_mar45,
                                              CCSM4_map_mar45,
                                              CNRM_CM5_map_mar45,
                                              CSIRO_Mk3_6_0_map_mar45,
                                              GFDL_ESM2G_map_mar45,
                                              GFDL_ESM2M_map_mar45,
                                              HadGEM2_CC365_map_mar45,
                                              HadGEM2_ES365_map_mar45,
                                              inmcm4_map_mar45,
                                              IPSL_CM5A_LR_map_mar45, 
                                              IPSL_CM5A_MR_map_mar45,
                                              IPSL_CM5B_LR_map_mar45,
                                              MIROC_ESM_CHEM_map_mar45,
                                              MIROC5_map_mar45,
                                              MRI_CGCM3_map_mar45,
                                              NorESM1_M_map_mar45),
                              ncol = 2, nrow = 11,
                              common.legend = TRUE)

accum_mar45_figs<- annotate_figure(p = accum_mar45_figs,
                   top = text_grob(label = "Median accumulated chill units by March 1 under rcp 45",
                                   face = "bold", size = 18))

ggsave(filename = "accum-mar45.png", plot = accum_mar45_figs, device = "png",
       width = 15, height = 40, units = "in")

rm(list = ls(pattern = "mar45"))


# April, rcp 45
accum_apr45_min <- min(df_45$median_A1)
accum_apr45_max <- max(df_45$median_A1)


for(h in unique(stats_comp$model)) {
  
  assign(x = paste(gsub(pattern = "-", replacement = "_", x = h), "map",
                   "apr45", sep="_"),
         value =
         {
           model_map(model = h, scenario_name = "rcp45", month_col = "median_A1",
                     min = accum_apr45_min, max = accum_apr45_max)
         }
  )
  
}


observed_map_apr45 <- observed_hist_map(min = accum_apr45_min, max = accum_apr45_max,
                                        month_col = "median_A1")

ensemble_map_apr45 <- ensemble_map(scenario_name = "rcp45", 
                                   month_col = "median_A1", min = accum_apr45_min,
                                   max = accum_apr45_max)

# Need to add historical observed to this:
accum_apr45_figs <- ggarrange(plotlist = list(observed_map_apr45,
                                              ensemble_map_apr45,
                                              bcc_csm1_1_m_map_apr45,
                                              bcc_csm1_1_map_apr45,
                                              BNU_ESM_map_apr45,
                                              CanESM2_map_apr45,
                                              CCSM4_map_apr45,
                                              CNRM_CM5_map_apr45,
                                              CSIRO_Mk3_6_0_map_apr45,
                                              GFDL_ESM2G_map_apr45,
                                              GFDL_ESM2M_map_apr45,
                                              HadGEM2_CC365_map_apr45,
                                              HadGEM2_ES365_map_apr45,
                                              inmcm4_map_apr45,
                                              IPSL_CM5A_LR_map_apr45, 
                                              IPSL_CM5A_MR_map_apr45,
                                              IPSL_CM5B_LR_map_apr45,
                                              MIROC_ESM_CHEM_map_apr45,
                                              MIROC5_map_apr45,
                                              MRI_CGCM3_map_apr45,
                                              NorESM1_M_map_apr45),
                              ncol = 2, nrow = 11,
                              common.legend = TRUE)

accum_apr45_figs<- annotate_figure(p = accum_apr45_figs,
                   top = text_grob(label = "Median accumulated chill units by April 1 under rcp 45",
                                   face = "bold", size = 18))

ggsave(filename = "accum-apr45.png", plot = accum_apr45_figs, device = "png",
       width = 15, height = 40, units = "in")

rm(list = ls(pattern = "apr45"))


# 3b. RCP85 monthly accum figs --------------------------------------------

# January, rcp85
# Going to make a common scale using min/max of all models in Jan @ rcp85 OR
# either modeled or observed historical
df_85 <- filter(stats_comp, scenario == "rcp85" | scenario == "historical")

accum_jan85_min <- min(df_85$median_J1)
accum_jan85_max <- max(df_85$median_J1)


for(h in unique(stats_comp$model)) {
  
  assign(x = paste(gsub(pattern = "-", replacement = "_", x = h), "map",
                   "jan85", sep="_"),
         value =
         {
           model_map(model = h, scenario_name = "rcp85", month_col = "median_J1",
                     min = accum_jan85_min, max = accum_jan85_max)
         }
  )
  
}



observed_map_jan85 <- observed_hist_map(min = accum_jan85_min, max = accum_jan85_max,
                                        month_col = "median_J1")

ensemble_map_jan85 <- ensemble_map(scenario_name = "rcp85", 
                                   month_col = "median_J1", min = accum_jan85_min,
                                   max = accum_jan85_max)

# Need to add historical observed to this:
accum_jan85_figs <- ggarrange(plotlist = list(observed_map_jan85,
                                              ensemble_map_jan85,
                                              bcc_csm1_1_m_map_jan85,
                                              bcc_csm1_1_map_jan85,
                                              BNU_ESM_map_jan85,
                                              CanESM2_map_jan85,
                                              CCSM4_map_jan85,
                                              CNRM_CM5_map_jan85,
                                              CSIRO_Mk3_6_0_map_jan85,
                                              GFDL_ESM2G_map_jan85,
                                              GFDL_ESM2M_map_jan85,
                                              HadGEM2_CC365_map_jan85,
                                              HadGEM2_ES365_map_jan85,
                                              inmcm4_map_jan85,
                                              IPSL_CM5A_LR_map_jan85, 
                                              IPSL_CM5A_MR_map_jan85,
                                              IPSL_CM5B_LR_map_jan85,
                                              MIROC_ESM_CHEM_map_jan85,
                                              MIROC5_map_jan85,
                                              MRI_CGCM3_map_jan85,
                                              NorESM1_M_map_jan85),
                              ncol = 2, nrow = 11,
                              common.legend = TRUE)

accum_jan85_figs<- annotate_figure(p = accum_jan85_figs,
                   top = text_grob(label = "Median accumulated chill units by Jan 1 under rcp 85",
                                   face = "bold", size = 18))

ggsave(filename = "accum-jan85.png", plot = accum_jan85_figs, device = "png",
       width = 15, height = 40, units = "in")

rm(list = ls(pattern = "jan85"))



# February, rcp 85
accum_feb85_min <- min(df_85$median_F1)
accum_feb85_max <- max(df_85$median_F1)


for(h in unique(stats_comp$model)) {
  
  assign(x = paste(gsub(pattern = "-", replacement = "_", x = h), "map",
                   "feb85", sep="_"),
         value =
         {
           model_map(model = h, scenario_name = "rcp85", month_col = "median_F1",
                     min = accum_feb85_min, max = accum_feb85_max)
         }
  )
  
}


observed_map_feb85 <- observed_hist_map(min = accum_feb85_min, max = accum_feb85_max,
                                        month_col = "median_F1")

ensemble_map_feb85 <- ensemble_map(scenario_name = "rcp85", 
                                   month_col = "median_F1", min = accum_feb85_min,
                                   max = accum_feb85_max)

# Need to add historical observed to this:
accum_feb85_figs <- ggarrange(plotlist = list(observed_map_feb85,
                                              ensemble_map_feb85,
                                              bcc_csm1_1_m_map_feb85,
                                              bcc_csm1_1_map_feb85,
                                              BNU_ESM_map_feb85,
                                              CanESM2_map_feb85,
                                              CCSM4_map_feb85,
                                              CNRM_CM5_map_feb85,
                                              CSIRO_Mk3_6_0_map_feb85,
                                              GFDL_ESM2G_map_feb85,
                                              GFDL_ESM2M_map_feb85,
                                              HadGEM2_CC365_map_feb85,
                                              HadGEM2_ES365_map_feb85,
                                              inmcm4_map_feb85,
                                              IPSL_CM5A_LR_map_feb85, 
                                              IPSL_CM5A_MR_map_feb85,
                                              IPSL_CM5B_LR_map_feb85,
                                              MIROC_ESM_CHEM_map_feb85,
                                              MIROC5_map_feb85,
                                              MRI_CGCM3_map_feb85,
                                              NorESM1_M_map_feb85),
                              ncol = 2, nrow = 11,
                              common.legend = TRUE)

accum_feb85_figs<- annotate_figure(p = accum_feb85_figs,
                   top = text_grob(label = "Median accumulated chill units by Feb 1 under rcp 85",
                                   face = "bold", size = 18))

ggsave(filename = "accum-feb85.png", plot = accum_feb85_figs, device = "png",
       width = 15, height = 40, units = "in")

rm(list = ls(pattern = "feb85"))



# March, rcp 85
accum_mar85_min <- min(df_85$median_M1)
accum_mar85_max <- max(df_85$median_M1)


for(h in unique(stats_comp$model)) {
  
  assign(x = paste(gsub(pattern = "-", replacement = "_", x = h), "map",
                   "mar85", sep="_"),
         value =
         {
           model_map(model = h, scenario_name = "rcp85", month_col = "median_M1",
                     min = accum_mar85_min, max = accum_mar85_max)
         }
  )
  
}


observed_map_mar85 <- observed_hist_map(min = accum_mar85_min, max = accum_mar85_max,
                                        month_col = "median_M1")

ensemble_map_mar85 <- ensemble_map(scenario_name = "rcp85", 
                                   month_col = "median_M1", min = accum_mar85_min,
                                   max = accum_mar85_max)

# Need to add historical observed to this:
accum_mar85_figs <- ggarrange(plotlist = list(observed_map_mar85,
                                              ensemble_map_mar85,
                                              bcc_csm1_1_m_map_mar85,
                                              bcc_csm1_1_map_mar85,
                                              BNU_ESM_map_mar85,
                                              CanESM2_map_mar85,
                                              CCSM4_map_mar85,
                                              CNRM_CM5_map_mar85,
                                              CSIRO_Mk3_6_0_map_mar85,
                                              GFDL_ESM2G_map_mar85,
                                              GFDL_ESM2M_map_mar85,
                                              HadGEM2_CC365_map_mar85,
                                              HadGEM2_ES365_map_mar85,
                                              inmcm4_map_mar85,
                                              IPSL_CM5A_LR_map_mar85, 
                                              IPSL_CM5A_MR_map_mar85,
                                              IPSL_CM5B_LR_map_mar85,
                                              MIROC_ESM_CHEM_map_mar85,
                                              MIROC5_map_mar85,
                                              MRI_CGCM3_map_mar85,
                                              NorESM1_M_map_mar85),
                              ncol = 2, nrow = 11,
                              common.legend = TRUE)

accum_mar85_figs<- annotate_figure(p = accum_mar85_figs,
                   top = text_grob(label = "Median accumulated chill units by March 1 under rcp 85",
                                   face = "bold", size = 18))

ggsave(filename = "accum-mar85.png", plot = accum_mar85_figs, device = "png",
       width = 15, height = 40, units = "in")

rm(list = ls(pattern = "mar85"))


# April, rcp 85
accum_apr85_min <- min(df_85$median_A1)
accum_apr85_max <- max(df_85$median_A1)


for(h in unique(stats_comp$model)) {
  
  assign(x = paste(gsub(pattern = "-", replacement = "_", x = h), "map",
                   "apr85", sep="_"),
         value =
         {
           model_map(model = h, scenario_name = "rcp85", month_col = "median_A1",
                     min = accum_apr85_min, max = accum_apr85_max)
         }
  )
  
}


observed_map_apr85 <- observed_hist_map(min = accum_apr85_min, max = accum_apr85_max,
                                        month_col = "median_A1")

ensemble_map_apr85 <- ensemble_map(scenario_name = "rcp85", 
                                   month_col = "median_A1", min = accum_apr85_min,
                                   max = accum_apr85_max)

# Need to add historical observed to this:
accum_apr85_figs <- ggarrange(plotlist = list(observed_map_apr85,
                                              ensemble_map_apr85,
                                              bcc_csm1_1_m_map_apr85,
                                              bcc_csm1_1_map_apr85,
                                              BNU_ESM_map_apr85,
                                              CanESM2_map_apr85,
                                              CCSM4_map_apr85,
                                              CNRM_CM5_map_apr85,
                                              CSIRO_Mk3_6_0_map_apr85,
                                              GFDL_ESM2G_map_apr85,
                                              GFDL_ESM2M_map_apr85,
                                              HadGEM2_CC365_map_apr85,
                                              HadGEM2_ES365_map_apr85,
                                              inmcm4_map_apr85,
                                              IPSL_CM5A_LR_map_apr85, 
                                              IPSL_CM5A_MR_map_apr85,
                                              IPSL_CM5B_LR_map_apr85,
                                              MIROC_ESM_CHEM_map_apr85,
                                              MIROC5_map_apr85,
                                              MRI_CGCM3_map_apr85,
                                              NorESM1_M_map_apr85),
                              ncol = 2, nrow = 11,
                              common.legend = TRUE)

accum_apr85_figs <- annotate_figure(p = accum_apr85_figs,
                   top = text_grob(label = "Median accumulated chill units by April 1 under rcp 85",
                                   face = "bold", size = 18))

ggsave(filename = "accum-apr85.png", plot = accum_apr85_figs, device = "png",
       width = 15, height = 40, units = "in")

rm(list = ls(pattern = "apr85"))



# 3c. RCP45 threshold figs ------------------------------------------------

# 50 unit threshold, rcp45
thresh50_45_min <- min(df_45$median_50)
thresh50_45_max <- max(df_45$median_50)


for(h in unique(stats_comp$model)) {
  
  assign(x = paste(gsub(pattern = "-", replacement = "_", x = h), "map",
                   "thresh50_45", sep="_"),
         value =
         {
           model_map(model = h, scenario_name = "rcp45", month_col = "median_50",
                     min = thresh50_45_min, max = thresh50_45_max)
         }
  )
  
}

# Separately created maps for observed historical and ensemble mean
observed_map_thresh50_45 <- observed_hist_map(min = thresh50_45_min,
                                        max = thresh50_45_max,
                                        month_col = "median_50")

ensemble_map_thresh50_45 <- ensemble_map(scenario_name = "rcp45", 
                                   month_col = "median_50",
                                   min = thresh50_45_min,
                                   max = thresh50_45_max)

# Need to add historical observed to this:
thresh50_45_figs <- ggarrange(plotlist = list(observed_map_thresh50_45,
                                              ensemble_map_thresh50_45,
                                              bcc_csm1_1_m_map_thresh50_45,
                                              bcc_csm1_1_map_thresh50_45,
                                              BNU_ESM_map_thresh50_45,
                                              CanESM2_map_thresh50_45,
                                              CCSM4_map_thresh50_45, 
                                              CNRM_CM5_map_thresh50_45,
                                              CSIRO_Mk3_6_0_map_thresh50_45,
                                              GFDL_ESM2G_map_thresh50_45,
                                              GFDL_ESM2M_map_thresh50_45,
                                              HadGEM2_CC365_map_thresh50_45,
                                              HadGEM2_ES365_map_thresh50_45,
                                              inmcm4_map_thresh50_45,
                                              IPSL_CM5A_LR_map_thresh50_45, 
                                              IPSL_CM5A_MR_map_thresh50_45,
                                              IPSL_CM5B_LR_map_thresh50_45,
                                              MIROC_ESM_CHEM_map_thresh50_45,
                                              MIROC5_map_thresh50_45, 
                                              MRI_CGCM3_map_thresh50_45,
                                              NorESM1_M_map_thresh50_45),
                              ncol = 2, nrow = 11,
                              common.legend = TRUE)

thresh50_45_figs <- annotate_figure(p = thresh50_45_figs,
                                    top = text_grob(label = "Median days to reach 50 chill unit threshold under rcp 45",
                                                    face = "bold", size = 18))

ggsave(filename = "thresh50_45.png", plot = thresh50_45_figs, device = "png",
       width = 15, height = 40, units = "in")

rm(list = ls(pattern = "thresh50_45"))

# 75 unit threshold, rcp45
thresh75_45_min <- min(df_45$median_75)
thresh75_45_max <- max(df_45$median_75)


for(h in unique(stats_comp$model)) {
  
  assign(x = paste(gsub(pattern = "-", replacement = "_", x = h), "map",
                   "thresh75_45", sep="_"),
         value =
         {
           model_map(model = h, scenario_name = "rcp45", month_col = "median_75",
                     min = thresh75_45_min, max = thresh75_45_max)
         }
  )
  
}

# Separately created maps for observed historical and ensemble mean
observed_map_thresh75_45 <- observed_hist_map(min = thresh75_45_min,
                                              max = thresh75_45_max,
                                              month_col = "median_75")

ensemble_map_thresh75_45 <- ensemble_map(scenario_name = "rcp45", 
                                         month_col = "median_75",
                                         min = thresh75_45_min,
                                         max = thresh75_45_max)

# Need to add historical observed to this:
thresh75_45_figs <- ggarrange(plotlist = list(observed_map_thresh75_45,
                                              ensemble_map_thresh75_45,
                                              bcc_csm1_1_m_map_thresh75_45,
                                              bcc_csm1_1_map_thresh75_45,
                                              BNU_ESM_map_thresh75_45,
                                              CanESM2_map_thresh75_45,
                                              CCSM4_map_thresh75_45, 
                                              CNRM_CM5_map_thresh75_45,
                                              CSIRO_Mk3_6_0_map_thresh75_45,
                                              GFDL_ESM2G_map_thresh75_45,
                                              GFDL_ESM2M_map_thresh75_45,
                                              HadGEM2_CC365_map_thresh75_45,
                                              HadGEM2_ES365_map_thresh75_45,
                                              inmcm4_map_thresh75_45,
                                              IPSL_CM5A_LR_map_thresh75_45, 
                                              IPSL_CM5A_MR_map_thresh75_45,
                                              IPSL_CM5B_LR_map_thresh75_45,
                                              MIROC_ESM_CHEM_map_thresh75_45,
                                              MIROC5_map_thresh75_45, 
                                              MRI_CGCM3_map_thresh75_45,
                                              NorESM1_M_map_thresh75_45),
                              ncol = 2, nrow = 11,
                              common.legend = TRUE)

thresh75_45_figs <- annotate_figure(p = thresh75_45_figs,
                                    top = text_grob(label = "Median days to reach 75 chill unit threshold under rcp 45",
                                                    face = "bold", size = 18))

ggsave(filename = "thresh75_45.png", plot = thresh75_45_figs, device = "png",
       width = 15, height = 40, units = "in")

rm(list = ls(pattern = "thresh75_45"))



# 3d. RCP85 threshold figs ------------------------------------------------

# 50 unit threshold, rcp85
thresh50_85_min <- min(df_85$median_50)
thresh50_85_max <- max(df_85$median_50)


for(h in unique(stats_comp$model)) {
  
  assign(x = paste(gsub(pattern = "-", replacement = "_", x = h), "map",
                   "thresh50_85", sep="_"),
         value =
         {
           model_map(model = h, scenario_name = "rcp85", month_col = "median_50",
                     min = thresh50_85_min, max = thresh50_85_max)
         }
  )
  
}

# Separately created maps for observed historical and ensemble mean
observed_map_thresh50_85 <- observed_hist_map(min = thresh50_85_min,
                                              max = thresh50_85_max,
                                              month_col = "median_50")

ensemble_map_thresh50_85 <- ensemble_map(scenario_name = "rcp85", 
                                         month_col = "median_50",
                                         min = thresh50_85_min,
                                         max = thresh50_85_max)

# Need to add historical observed to this:
thresh50_85_figs <- ggarrange(plotlist = list(observed_map_thresh50_85,
                                              ensemble_map_thresh50_85,
                                              bcc_csm1_1_m_map_thresh50_85,
                                              bcc_csm1_1_map_thresh50_85,
                                              BNU_ESM_map_thresh50_85,
                                              CanESM2_map_thresh50_85,
                                              CCSM4_map_thresh50_85, 
                                              CNRM_CM5_map_thresh50_85,
                                              CSIRO_Mk3_6_0_map_thresh50_85,
                                              GFDL_ESM2G_map_thresh50_85,
                                              GFDL_ESM2M_map_thresh50_85,
                                              HadGEM2_CC365_map_thresh50_85,
                                              HadGEM2_ES365_map_thresh50_85,
                                              inmcm4_map_thresh50_85,
                                              IPSL_CM5A_LR_map_thresh50_85, 
                                              IPSL_CM5A_MR_map_thresh50_85,
                                              IPSL_CM5B_LR_map_thresh50_85,
                                              MIROC_ESM_CHEM_map_thresh50_85,
                                              MIROC5_map_thresh50_85, 
                                              MRI_CGCM3_map_thresh50_85,
                                              NorESM1_M_map_thresh50_85),
                              ncol = 2, nrow = 11,
                              common.legend = TRUE)

thresh50_85_figs <- annotate_figure(p = thresh50_85_figs,
                                    top = text_grob(label = "Median days to reach 50 chill unit threshold under rcp 85",
                                                    face = "bold", size = 18))

ggsave(filename = "thresh50_85.png", plot = thresh50_85_figs, device = "png",
       width = 15, height = 40, units = "in")

rm(list = ls(pattern = "thresh50_85"))

# 75 unit threshold, rcp85
thresh75_85_min <- min(df_85$median_75)
thresh75_85_max <- max(df_85$median_75)


for(h in unique(stats_comp$model)) {
  
  assign(x = paste(gsub(pattern = "-", replacement = "_", x = h), "map",
                   "thresh75_85", sep="_"),
         value =
         {
           model_map(model = h, scenario_name = "rcp85", month_col = "median_75",
                     min = thresh75_85_min, max = thresh75_85_max)
         }
  )
  
}

# Separately created maps for observed historical and ensemble mean
observed_map_thresh75_85 <- observed_hist_map(min = thresh75_85_min,
                                              max = thresh75_85_max,
                                              month_col = "median_75")

ensemble_map_thresh75_85 <- ensemble_map(scenario_name = "rcp85", 
                                         month_col = "median_75",
                                         min = thresh75_85_min,
                                         max = thresh75_85_max)

# Need to add historical observed to this:
thresh75_85_figs <- ggarrange(plotlist = list(observed_map_thresh75_85,
                                              ensemble_map_thresh75_85,
                                              bcc_csm1_1_m_map_thresh75_85,
                                              bcc_csm1_1_map_thresh75_85,
                                              BNU_ESM_map_thresh75_85,
                                              CanESM2_map_thresh75_85,
                                              CCSM4_map_thresh75_85, 
                                              CNRM_CM5_map_thresh75_85,
                                              CSIRO_Mk3_6_0_map_thresh75_85,
                                              GFDL_ESM2G_map_thresh75_85,
                                              GFDL_ESM2M_map_thresh75_85,
                                              HadGEM2_CC365_map_thresh75_85,
                                              HadGEM2_ES365_map_thresh75_85,
                                              inmcm4_map_thresh75_85,
                                              IPSL_CM5A_LR_map_thresh75_85, 
                                              IPSL_CM5A_MR_map_thresh75_85,
                                              IPSL_CM5B_LR_map_thresh75_85,
                                              MIROC_ESM_CHEM_map_thresh75_85,
                                              MIROC5_map_thresh75_85, 
                                              MRI_CGCM3_map_thresh75_85,
                                              NorESM1_M_map_thresh75_85),
                              ncol = 2, nrow = 11,
                              common.legend = TRUE)

thresh75_85_figs <- annotate_figure(p = thresh75_85_figs,
                                    top = text_grob(label = "Median days to reach 75 chill unit threshold under rcp 85",
                                                    face = "bold", size = 18))

ggsave(filename = "thresh75_85.png", plot = thresh75_85_figs, device = "png",
       width = 15, height = 40, units = "in")

rm(list = ls(pattern = "thresh75_85"))

