
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
<<<<<<< HEAD
  dt <- subset(dt, select=c(chill_season, sum_J1, 
                            sum_F1, sum_M1, sum_A1, lat, long,
=======
  dt <- subset(dt, select=c(chill_season,
                            sum_J1, sum_F1, sum_M1, sum_A1, 
                            lat, long, climate_type,
>>>>>>> a255bd425a6f23bebc1f80714626251bfb7c2646
                            scenario, model, year))
  
  dt <- dt %>% filter(year <= 2005 | year >= 2025)
  
  time_periods = c("Historical", "2025_2050", "2051_2075", "2076_2099")
  dt$time_period = 0L
  dt$time_period[dt$year <= 2005] <- time_periods[1]
<<<<<<< HEAD
  dt$time_period[dt$year >= 2025 & dt$year <= 2050] <- time_periods[2]
  dt$time_period[dt$year >  2050 & dt$year <= 2075] <- time_periods[3]
=======
  dt$time_period[dt$year >= 2025 & dt$year<=2050] <- time_periods[2]
  dt$time_period[dt$year >  2050 & dt$year<=2075] <- time_periods[3]
>>>>>>> a255bd425a6f23bebc1f80714626251bfb7c2646
  dt$time_period[dt$year >  2075] <- time_periods[4]
  dt$time_period = factor(dt$time_period, levels=time_periods, order=T)

  dt$scenario[dt$scenario == "rcp45"] <- "RCP 4.5"
  dt$scenario[dt$scenario == "rcp85"] <- "RCP 8.5"
  dt$scenario[dt$scenario == "historical"] <- "Historical"

<<<<<<< HEAD
  jan_data <- subset(dt, select=c(sum_J1, lat, long, scenario, model, time_period, chill_season))
  feb_data <- subset(dt, select=c(sum_F1, lat, long, scenario, model, time_period, chill_season))
  mar_data <- subset(dt, select=c(sum_M1, lat, long, scenario, model, time_period, chill_season))
  apr_data <- subset(dt, select=c(sum_A1, lat, long, scenario, model, time_period, chill_season))
  return (list(jan_data, feb_data, mar_data, apr_data))
}

=======
  jan_data <- subset(dt, select=c(sum_J1, lat, long, climate_type, scenario, model, time_period, chill_season))
  feb_data <- subset(dt, select=c(sum_F1, lat, long, climate_type, scenario, model, time_period, chill_season))
  mar_data <- subset(dt, select=c(sum_M1, lat, long, climate_type, scenario, model, time_period, chill_season))
  apr_data <- subset(dt, select=c(sum_A1, lat, long, climate_type, scenario, model, time_period, chill_season))
  return (list(jan_data, feb_data, mar_data, apr_data))
}

count_years_threshs_met <- function(dataT, due){
  h_year_count <- length(unique(dataT[dataT$time_period =="Historical",]$chill_season))
  f1_year_count <- length(unique(dataT[dataT$time_period== "2025_2050",]$chill_season))
  f2_year_count <- length(unique(dataT[dataT$time_period== "2051_2075",]$chill_season))
  f3_year_count <- length(unique(dataT[dataT$time_period== "2076_2099",]$chill_season))
  if (due == "Jan"){
    col_name = "sum_J1"
    } else if (due == "Feb"){
        col_name = "sum_F1"
    } else if(due =="Mar"){
        col_name = "sum_M1"
    } else if (due =="Apr"){
        col_name = "sum_A1"
  }
  
  bks = c(-200, seq(20, 75, 5), 300)

  result <- dataT %>%
            mutate(thresh_range = cut(get(col_name), breaks = bks )) %>%
            group_by(lat, long, climate_type, time_period, 
                     thresh_range, model, scenario) %>%
            summarize(no_years = n_distinct(chill_season)) %>% 
            data.table()
  
  time_periods = c("Historical", "2025_2050", "2051_2075", "2076_2099")
  result$time_period = factor(result$time_period, 
                              levels=time_periods,
                              order=T)
  
  result$thresh_range <- factor(result$thresh_range, order=T)
  result$thresh_range <- fct_rev(result$thresh_range)
  result <- result[order(thresh_range), ]


  result <- result %>% 
            group_by(lat, long, climate_type, time_period, model, scenario) %>% 
            mutate(n_years_passed = cumsum(no_years)) %>% 
            data.table()

  # the following can be done more efficiently!
  result_hist <- result %>% filter(time_period == "Historical") %>% data.table()
  result_50 <- result %>% filter(time_period == "2025_2050") %>% data.table()
  result_75 <- result %>% filter(time_period == "2051_2075") %>% data.table()
  result_99 <- result %>% filter(time_period == "2076_2099") %>% data.table()
  
  result_hist$frac_passed = result_hist$n_years_passed / h_year_count
  result_50$frac_passed = result_50$n_years_passed / f1_year_count
  result_75$frac_passed = result_75$n_years_passed / f2_year_count
  result_99$frac_passed = result_99$n_years_passed / f3_year_count

  result <- rbind(result_hist, result_50, result_75, result_99)
  return(result)
}

>>>>>>> a255bd425a6f23bebc1f80714626251bfb7c2646
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






