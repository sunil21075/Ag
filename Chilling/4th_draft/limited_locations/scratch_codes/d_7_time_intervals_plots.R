# Hourly data for 7 time intervals

library(data.table)
library(ggplots)

data_dir = file.path("/Users/hn/Desktop/Desktop/Kirti/check_point/chilling/7_time_intervals/")

rcp45 <- data.table(readRDS(paste0(data_dir, "rcp45.rds")))
rcp85 <- data.table(readRDS(paste0(data_dir, "rcp85.rds")))
modeled_hist <- data.table(readRDS(paste0(data_dir, "modeled_hist.rds")))

# change some column names
# so they look like Matt's data
# so we can use his plot codes easily!
colnames(rcp45)[colnames(rcp45) == 'climateScenario'] <- 'model'
colnames(rcp85)[colnames(rcp85) == 'climateScenario'] <- 'model'
colnames(modeled_hist)[colnames(modeled_hist) == 'climateScenario'] <- 'model'

modeled_hist$scenario = "historical"
rcp45$scenario = "rcp45"
rcp85$scenario = "rcp85"



