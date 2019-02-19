
rm(list=ls())

library(data.table)
library(ggpubr)
library(plyr)
library(tidyverse)
library(ggplot2)
########################################################################################
#######
####### Add time period 2040, 2060, 2080 function with and without overlap
#######
#######
########################################################################################

add_time_period <- function(data){
    all_data = data.table()
    data_hist = filter(data, year<=2005)
    data_2040 = filter(data, year>2025 & year<=2055)
    data_2060 = filter(data, year>2045 & year<=2075)
    data_2080 = filter(data, year>2065 & year<=2085)

    data_hist$ClimateGroup = "Historical"
    data_2040$ClimateGroup = "2040's"
    data_2060$ClimateGroup = "2060's"
    data_2080$ClimateGroup = "2080's"

    all_data = rbind(data_hist, data_2040, data_2060, data_2080)
    
    time_periods = c("Historical", "2040's", "2060's", "2080's")

    all_data$ClimateGroup <- factor(all_data$ClimateGroup)
    all_data$ClimateGroup <- ordered(all_data$ClimateGroup, levels = time_periods)
    all_data <- within(all_data, remove(year))
    return(all_data)
}

clean_data <- function(data){
    data = within(data, remove(sum, sum_J1, sum_F1, sum_M1, sum_A1, 
                               lat, long, Chill_season))
    data = filter(data, model != "observed" )

    data = within(data, remove(model))
    return(data)
}

########################################################################################

# data directories
utah_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/chilling/utah_model_stats/"
dynamic_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/chilling/dynamic_model_stats/"

utah <- data.table(readRDS(paste0(utah_dir, "summary_comp.rds")))
dynamic <- data.table(readRDS(paste0(dynamic_dir, "summary_comp.rds")))

# remove extra columns
utah = clean_data(utah)
dynamic = clean_data(dynamic)

utah <- add_time_period(utah)
dynamic <- add_time_period(dynamic)

#######
#######     Compute Medians
#######

utah_medians <- utah %>% group_by(climate_type, scenario, ClimateGroup) %>%
                         summarise_at(.funs = funs(med = median), vars(thresh_20:thresh_75))

dynamic_medians <- dynamic %>% group_by(climate_type, scenario, ClimateGroup) %>%
                       summarise_at(.funs = funs(med = median), vars(thresh_20:thresh_75))

#######
#######     Compute InterQuantile Ranges
#######

utah_IQR <- utah %>% group_by(climate_type, scenario, ClimateGroup) %>%
                     summarise_at(.funs = funs(IntQntRng = IQR), vars(thresh_20:thresh_75))

dynamic_IQR <- dynamic %>% group_by(climate_type, scenario, ClimateGroup) %>%
                           summarise_at(.funs = funs(IntQntRng = IQR), vars(thresh_20:thresh_75))


#######
#######     merge medians and IQRs
#######
utah_merged = merge(utah_medians, utah_IQR, by=c("climate_type", "scenario", "ClimateGroup"))
dynamic_merged = merge(dynamic_medians, dynamic_IQR, by=c("climate_type", "scenario", "ClimateGroup"))

write_dir = "/Users/hn/Documents/GitHub/Kirti/Chilling/code/"

write.table(utah_medians, file = paste0(write_dir, "utah_medians.csv"), 
            row.names = FALSE, 
            col.names = TRUE, 
            sep = ",")

write.table(dynamic_medians, file = paste0(write_dir, "dynamic_medians.csv"),
            row.names = FALSE, 
            col.names = TRUE, 
            sep = ",")

write.table(utah_IQR, file = paste0(write_dir, "utah_IQR.csv"), 
            row.names = FALSE, 
            col.names = TRUE, 
            sep = ",")

write.table(dynamic_IQR, file = paste0(write_dir, "dynamic_IQR.csv"),
            row.names = FALSE, 
            col.names = TRUE, 
            sep = ",")

write.table(utah_merged, file = paste0(write_dir, "utah_medians_IQR.csv"),
            row.names = FALSE, 
            col.names = TRUE, 
            sep = ",")

write.table(dynamic_merged, file = paste0(write_dir, "dynamic_medians_IQR.csv"),
            row.names = FALSE, 
            col.names = TRUE, 
            sep = ",")

##########################################################################
###########
###########                         Plot Area
###########
##########################################################################
utah_medians <- rename(utah_medians, c(thresh_20_med="20", thresh_25_med="25",
                                       thresh_30_med="30", thresh_35_med="35",
                                       thresh_40_med="40", thresh_45_med="45",
                                       thresh_50_med="50", thresh_55_med="55",
                                       thresh_60_med="60", thresh_65_med="65",
                                       thresh_70_med="70", thresh_75_med="75"
                                        ))

dynamic_medians <- rename(dynamic_medians, c(thresh_20_med="20", thresh_25_med="25",
                                             thresh_30_med="30", thresh_35_med="35",
                                             thresh_40_med="40", thresh_45_med="45",
                                             thresh_50_med="50", thresh_55_med="55",
                                             thresh_60_med="60", thresh_65_med="65",
                                             thresh_70_med="70", thresh_75_med="75"
                                             ))

utah_medians_melt = melt(utah_medians, id=c("climate_type", "scenario", "ClimateGroup"))
dynamic_medians_melt = melt(dynamic_medians, id=c("climate_type", "scenario", "ClimateGroup"))

# Convert the column variable to integers
utah_medians_melt[,] <- lapply(utah_medians_melt, factor)
utah_medians_melt[,] <- lapply(utah_medians_melt, function(x) type.convert(as.character(x), as.is = TRUE))

dynamic_medians_melt[,] <- lapply(dynamic_medians_melt, factor)
dynamic_medians_melt[,] <- lapply(dynamic_medians_melt, function(x) type.convert(as.character(x), as.is = TRUE))


utah_medians_plot = ggplot(utah_medians_melt, aes(x=variable, y=value), 
                           fill=factor(ClimateGroup)) + 
                    geom_point() + 
                    stat_smooth(method = "lm", se=FALSE, color="black") + 
                    labs(x = "thresholds", y = "median", fill = "Climate Group") +
                    facet_grid(. ~ scenario ~ climate_type, scales = "free")




