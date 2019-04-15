
rm(list=ls())
library(tidyverse)
library(data.table)
library(dplyr)

options(digit=9)
options(digits=9)

##########################################################################################
###                                                                                    ###
###                             Define Functions here                                  ###
###                                                                                    ###
##########################################################################################
define_path <- function(model_name){
  if (model_name == "dynamic"){
      in_dir <- paste0(main_in_dir, model_specific_dir_name[1])
      } else if (model == "utah"){
          in_dir <- paste0(main_in_dir, model_specific_dir_name[2])
  }
}

clean_process <- function(dt){
  dt <- subset(dt, select=c(chill_season, sum_J1, 
                            sum_F1, sum_M1, sum_A1, lat, long,
                            scenario, model, year))
  
  dt <- dt %>% filter(year <= 2005 | year >= 2025)
  
  time_periods = c("Historical", "2025_2050", "2051_2075", "2076_2099")
  dt$time_period = 0L
  dt$time_period[dt$year <= 2005] <- time_periods[1]
  dt$time_period[dt$year >= 2025 & dt$year <= 2050] <- time_periods[2]
  dt$time_period[dt$year >  2050 & dt$year <= 2075] <- time_periods[3]
  dt$time_period[dt$year >  2075] <- time_periods[4]
  dt$time_period = factor(dt$time_period, levels=time_periods, order=T)

  dt$scenario[dt$scenario == "rcp45"] <- "RCP 4.5"
  dt$scenario[dt$scenario == "rcp85"] <- "RCP 8.5"
  dt$scenario[dt$scenario == "historical"] <- "Historical"

  jan_data <- subset(dt, select=c(sum_J1, lat, long, scenario, model, time_period, chill_season))
  feb_data <- subset(dt, select=c(sum_F1, lat, long, scenario, model, time_period, chill_season))
  mar_data <- subset(dt, select=c(sum_M1, lat, long, scenario, model, time_period, chill_season))
  apr_data <- subset(dt, select=c(sum_A1, lat, long, scenario, model, time_period, chill_season))
  return (list(jan_data, feb_data, mar_data, apr_data))
}

#############################################
###                                       ###
###                 Driver                ###
###                                       ###
#############################################
out_dir = "/Users/hn/Desktop/tables/"


# main_in_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/chilling/non_overlapping/"
# model_names = c("dynamic") # , "utah"
# model_specific_dir_name = paste0(model_names, "_model_stats/")
# file_name = "summary_comp.rds"
# mdata <- data.table(readRDS(paste0(main_in_dir, model_specific_dir_name, file_name)))
# setnames(mdata, old=c("Chill_season"), new=c("chill_season"))

main_in_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/chilling/non_overlapping/different_chill_start/"
begins <- c("mid_sept", "oct", "mid_oct", "nov", "mid_nov")
begin = begins[1]
mdata <- data.table(readRDS(paste0(main_in_dir, begin, ".rds")))



mdata <- mdata %>% filter(model != "observed")
information <- clean_process(mdata)

jan_data = information[[1]]
feb_data = information[[2]]
mar_data = information[[3]]
apr_data = information[[4]]
rm(information, mdata)

jan_result = count_years_threshs_met(dataT = jan_data, due="Jan")
feb_result = count_years_threshs_met(dataT = feb_data, due="Feb")
mar_result = count_years_threshs_met(dataT = mar_data, due="Mar")
apr_result = count_years_threshs_met(dataT = apr_data, due="Apr")

#####################
#####################              RCP 8.5
#####################
######****************************
################################## JAN
######****************************

quan_per <- jan_result %>% group_by(climate_type, time_period, scenario, thresh_range) %>% 
                           summarise(quan_25 = quantile(frac_passed, probs = 0.25)) %>% data.table()

######### COOLER 8.5

data <- quan_per %>% filter(scenario == "RCP 8.5", climate_type=="Cooler Area") %>% data.table()
data <- data[order(time_period, thresh_range), ]
dattest <- data %>% spread(time_period, quan_25)

write.table(dattest, file = paste0(out_dir, "jan_cool_85.csv"), row.names = FALSE, col.names = TRUE, sep = ",")
rm(data, dattest)

data <- quan_per %>% filter(scenario == "Historical", climate_type=="Cooler Area") %>% data.table()
data <- data[order(time_period, thresh_range), ]
dattest <- data %>% spread(time_period, quan_25)
write.table(dattest, file = paste0(out_dir, "jan_cool_hist.csv"), row.names = FALSE, col.names = TRUE, sep = ",")
rm(data, dattest)

######## WARMER

data <- quan_per %>% filter(scenario == "RCP 8.5", climate_type=="Warmer Area") %>% data.table()
data <- data[order(time_period, thresh_range), ]
dattest <- data %>% spread(time_period, quan_25)
write.table(dattest, file = paste0(out_dir, "jan_warm_85.csv"), row.names = FALSE, col.names = TRUE, sep = ",")
rm(data, dattest)

data <- quan_per %>% filter(scenario == "Historical", climate_type=="Warmer Area") %>% data.table()
data <- data[order(time_period, thresh_range), ]
dattest <- data %>% spread(time_period, quan_25)
write.table(dattest, file = paste0(out_dir, "jan_warm_hist.csv"), row.names = FALSE, col.names = TRUE, sep = ",")
rm(data, dattest, quan_per)

######****************************
################################## FEB
######****************************
quan_per_feb <- feb_result %>% group_by(climate_type, time_period, scenario, thresh_range) %>% 
                               summarise(quan_25 = quantile(frac_passed, probs = 0.25)) %>% data.table()

######## COOLER

data <- quan_per_feb %>% filter(scenario == "RCP 8.5", climate_type=="Cooler Area") %>% data.table()
data <- data[order(time_period, thresh_range), ]
dattest <- data %>% spread(time_period, quan_25)
write.table(dattest, file = paste0(out_dir, "feb_cool_85.csv"), row.names = FALSE, col.names = TRUE, sep = ",")
rm(data, dattest)

data <- quan_per_feb %>% filter(scenario == "Historical", climate_type=="Cooler Area") %>% data.table()
data <- data[order(time_period, thresh_range), ]
dattest <- data %>% spread(time_period, quan_25)
write.table(dattest, file = paste0(out_dir, "feb_cool_hist.csv"), row.names = FALSE, col.names = TRUE, sep = ",")
rm(data, dattest)

######## WARMER

data <- quan_per_feb %>% filter(scenario == "RCP 8.5", climate_type=="Warmer Area") %>% data.table()
data <- data[order(time_period, thresh_range), ]
dattest <- data %>% spread(time_period, quan_25)
write.table(dattest, file = paste0(out_dir, "feb_warm_85.csv"), row.names = FALSE, col.names = TRUE, sep = ",")
rm(data, dattest)

data <- quan_per_feb %>% filter(scenario == "Historical", climate_type=="Warmer Area") %>% data.table()
data <- data[order(time_period, thresh_range), ]
dattest <- data %>% spread(time_period, quan_25)
write.table(dattest, file = paste0(out_dir, "feb_warm_hist.csv"), row.names = FALSE, col.names = TRUE, sep = ",")
rm(data, dattest, quan_per_feb)

######****************************
################################## MARCH
######****************************

quan_per <- mar_result %>% group_by(climate_type, time_period, scenario, thresh_range) %>% 
                           summarise(quan_25 = quantile(frac_passed, probs = 0.25)) %>% data.table()

######### COOLER
data <- quan_per %>% filter(scenario == "RCP 8.5", climate_type=="Cooler Area") %>% data.table()
data <- data[order(time_period, thresh_range), ]
dattest <- data %>% spread(time_period,quan_25)
write.table(dattest, file = paste0(out_dir, "march_cool_85.csv"), row.names = FALSE, col.names = TRUE, sep = ",")
rm(data, dattest)

data <- quan_per %>% filter(scenario == "Historical", climate_type=="Cooler Area") %>% data.table()
data <- data[order(time_period, thresh_range), ]
dattest <- data %>% spread(time_period, quan_25)
write.table(dattest, file = paste0(out_dir, "march_cool_hist.csv"), row.names = FALSE, col.names = TRUE, sep = ",")
rm(data, dattest)

######## WARM

data <- quan_per %>% filter(scenario == "RCP 8.5", climate_type=="Warmer Area") %>% data.table()
data <- data[order(time_period, thresh_range), ]
dattest <- data %>% spread(time_period, quan_25)
write.table(dattest, file = paste0(out_dir, "march_warm_85.csv"), row.names = FALSE, col.names = TRUE, sep = ",")
rm(data, dattest)

data <- quan_per %>% filter(scenario == "Historical", climate_type=="Warmer Area") %>% data.table()
data <- data[order(time_period, thresh_range), ]
dattest <- data %>% spread(time_period, quan_25)
write.table(dattest, file = paste0(out_dir, "march_warm_hist.csv"), row.names = FALSE, col.names = TRUE, sep = ",")
rm(data, dattest, quan_per)

######****************************
################################## April
######****************************

quan_per <- apr_result %>% group_by(climate_type, time_period, scenario, thresh_range) %>% 
                           summarise(quan_25 = quantile(frac_passed, probs = 0.25)) %>% data.table()

######### COOLER
data <- quan_per %>% filter(scenario == "RCP 8.5", climate_type=="Cooler Area") %>% data.table()
data <- data[order(time_period, thresh_range), ]
dattest <- data %>% spread(time_period,quan_25)
write.table(dattest, file = paste0(out_dir, "april_cool_85.csv"), row.names = FALSE, col.names = TRUE, sep = ",")
rm(data, dattest)

data <- quan_per %>% filter(scenario == "Historical", climate_type=="Cooler Area") %>% data.table()
data <- data[order(time_period, thresh_range), ]
dattest <- data %>% spread(time_period, quan_25)
write.table(dattest, file = paste0(out_dir, "april_cool_hist.csv"), row.names = FALSE, col.names = TRUE, sep = ",")
rm(data, dattest)

######## WARM

data <- quan_per %>% filter(scenario == "RCP 8.5", climate_type=="Warmer Area") %>% data.table()
data <- data[order(time_period, thresh_range), ]
dattest <- data %>% spread(time_period, quan_25)
write.table(dattest, file = paste0(out_dir, "april_warm_85.csv"), row.names = FALSE, col.names = TRUE, sep = ",")
rm(data, dattest)

data <- quan_per %>% filter(scenario == "Historical", climate_type=="Warmer Area") %>% data.table()
data <- data[order(time_period, thresh_range), ]
dattest <- data %>% spread(time_period, quan_25)
write.table(dattest, file = paste0(out_dir, "april_warm_hist.csv"), row.names = FALSE, col.names = TRUE, sep = ",")
rm(data, dattest, quan_per)

#####################
#####################              RCP 4.5
#####################
######****************************
################################## JAN
######****************************

quan_per <- jan_result %>% group_by(climate_type, time_period, scenario, thresh_range) %>% 
                           summarise(quan_25 = quantile(frac_passed, probs = 0.25)) %>% data.table()

######### COOLER

data <- quan_per %>% filter(scenario == "RCP 4.5", climate_type == "Cooler Area") %>% data.table()
data <- data[order(time_period, thresh_range), ]
dattest <- data %>% spread(time_period, quan_25)
write.table(dattest, file = paste0(out_dir, "jan_cool_45.csv"), row.names = FALSE, col.names = TRUE, sep = ",")
rm(data, dattest)

######## WARM

data <- quan_per %>% filter(scenario == "RCP 4.5", climate_type=="Warmer Area") %>% data.table()
data <- data[order(time_period, thresh_range), ]
dattest <- data %>% spread(time_period, quan_25)
write.table(dattest, file = paste0(out_dir, "jan_warm_45.csv"), row.names = FALSE, col.names = TRUE, sep = ",")
rm(data, dattest)

######****************************
################################## FEB
######****************************
quan_per_feb <- feb_result %>% group_by(climate_type, time_period, scenario, thresh_range) %>% 
                               summarise(quan_25 = quantile(frac_passed, probs = 0.25)) %>% data.table()

######## COOLER

data <- quan_per_feb %>% filter(scenario == "RCP 4.5", climate_type=="Cooler Area") %>% data.table()
data <- data[order(time_period, thresh_range), ]
dattest <- data %>% spread(time_period, quan_25)
write.table(dattest, file = paste0(out_dir, "feb_cool_45.csv"), row.names = FALSE, col.names = TRUE, sep = ",")
rm(data, dattest)

######## WARM

data <- quan_per_feb %>% filter(scenario == "RCP 4.5", climate_type=="Warmer Area") %>% data.table()
data <- data[order(time_period, thresh_range), ]
dattest <- data %>% spread(time_period, quan_25)
write.table(dattest, file = paste0(out_dir, "feb_warm_45.csv"), row.names = FALSE, col.names = TRUE, sep = ",")
rm(data, dattest)

######****************************
################################## MARCH
######****************************

quan_per <- mar_result %>% group_by(climate_type, time_period, scenario, thresh_range) %>% 
                           summarise(quan_25 = quantile(frac_passed, probs = 0.25)) %>% data.table()

######### COOLER

data <- quan_per %>% filter(scenario == "RCP 4.5", climate_type=="Cooler Area") %>% data.table()
data <- data[order(time_period, thresh_range), ]
dattest <- data %>% spread(time_period, quan_25)
write.table(dattest, file = paste0(out_dir, "march_cool_45.csv"), row.names = FALSE, col.names = TRUE, sep = ",")
rm(data, dattest)

######## WARM

data <- quan_per %>% filter(scenario == "RCP 4.5", climate_type=="Warmer Area") %>% data.table()
data <- data[order(time_period, thresh_range), ]
dattest <- data %>% spread(time_period, quan_25)
write.table(dattest, file = paste0(out_dir, "march_warm_45.csv"), row.names = FALSE, col.names = TRUE, sep = ",")
rm(data, dattest)

######****************************
################################## April
######****************************

quan_per <- apr_result %>% group_by(climate_type, time_period, scenario, thresh_range) %>% 
                           summarise(quan_25 = quantile(frac_passed, probs = 0.25)) %>% data.table()

######### COOLER

data <- quan_per %>% filter(scenario == "RCP 4.5", climate_type=="Cooler Area") %>% data.table()
data <- data[order(time_period, thresh_range), ]
dattest <- data %>% spread(time_period, quan_25)
write.table(dattest, file = paste0(out_dir, "april_cool_45.csv"), row.names = FALSE, col.names = TRUE, sep = ",")
rm(data, dattest)

######## WARM

data <- quan_per %>% filter(scenario == "RCP 4.5", climate_type=="Warmer Area") %>% data.table()
data <- data[order(time_period, thresh_range), ]
dattest <- data %>% spread(time_period, quan_25)
write.table(dattest, file = paste0(out_dir, "april_warm_45.csv"), row.names = FALSE, col.names = TRUE, sep = ",")
rm(data, dattest)






