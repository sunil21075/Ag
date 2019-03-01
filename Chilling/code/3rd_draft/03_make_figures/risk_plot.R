library(data.table)
library(dplyr)

options(digit=9)
options(digits=9)

#############################################
###                                       ###
###       Define Functions here           ###
###                                       ###
#############################################
define_path <- function(model_name){
    if (model_name == "dynamic"){
        in_dir <- paste0(main_in_dir, model_specific_dir_name[1])
        } else if (model == "utah"){
            in_dir <- paste0(main_in_dir, model_specific_dir_name[2])
    }
}

clean_process <- function(data){

    data <- subset(data, select=c(Chill_season, sum_J1, 
    	                          sum_F1, sum_M1, lat, long, climate_type,
    	                          scenario, model, year))
    
    data <- data %>% filter(year <= 2005 | year >= 2025)

    data$time_period = 0L
    data$time_period[data$year<=2005] <- "Historical"
    data$time_period[data$year>=2025 & data$year<=2050] <- "2025_2050"
    data$time_period[data$year> 2050 & data$year<=2075] <- "2051_2075"
    data$time_period[data$year> 2075] <- "2076_2099"

    data$scenario[data$scenario == "rcp45"] <- "RCP 4.5"
    data$scenario[data$scenario == "rcp85"] <- "RCP 8.5"
    data$scenario[data$scenario == "historical"] <- "Historical"

    data <- within(data, remove(year, Chill_season, sum_M1, sum_A1, sum))
    return (data)
}

detect_non_met_years <- function(dataT, threshs){
    ## This function detects the number of years
    ## for which a given location, in a given 
    ## model, e.g. BNU, in a given time_period, e.g. historical,
    ## has not met a given threshold.

    # result <- data %>% 
    #           group_by(lat, long, time_period, scenario, model, climate_type) %>%
    #           summarise_all(funs(sum(. == 0))) %>% data.table()


    result <- data %>%
			  mutate(time_range = cut(SUM_J1, breaks = seq(20, 75, 5))) %>%
			  group_by(city, temp_range, .drop = FALSE) %>%
			  summarize(years = n_distinct(year))

    return(result)
}
#############################################

main_in_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/chilling/non_overlapping/"

model_specific_dir_name = c("dynamic_model_stats/", "utah_model_stats/")
model_names = c("dynamic", "utah")

file_name = "summary_comp.rds"

