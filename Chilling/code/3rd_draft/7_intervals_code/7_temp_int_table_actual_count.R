rm(list=ls())
library(data.table)
library(dplyr)

data_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/chilling/7_temp_intervals_data/"

iof = list(c(-Inf, -2), 
           c(-2, 4), 
           c(4, 6), 
           c(6, 8), 
           c(8, 13), 
           c(13, 16), 
           c(16, Inf))
iof_char = c("(-Inf, -2]",
             "(-2, 4]",
             "(4, 6]",
             "(6, 8]",
             "(8, 13]",
             "(13, 16]",
             "(16, Inf)")
iof_breaks = c(-Inf, -2, 4, 6, 8, 13, 16, Inf)
name_pref = "sept_thru_"
dead_line = c("dec", "jan")
name_post = "_modeled.rds"

the_table = data.frame(matrix(ncol = 8, nrow = 0))

for (month in dead_line){
    ##########################################
    #                                        #
    #         initialize the AUC table       #
    #         to populate                    #
    #                                        #
    ##########################################
    df_help <- data.frame(matrix(ncol = 8, nrow = 6))
    colnames(df_help) <- c("location", iof_char)

    # The table to be filled with no. hours in each interval for each location
    # The time interval goes from sept. to Dec. or Jan. It is shortened
    # because variable names are getting long. It is NOT just for month of
    # Dec. or Jan.
    # The data for 2076-2099 are from RCP 8.5

    df_help[, "location"] = c(paste0(month, "_rich_hist"), 
                              paste0(month, "_rich_76_99"), 
                              paste0(month, "_rich_diff"), 
                              paste0(month, "_omak_hist"), 
                              paste0(month, "_omak_76_99"), 
                              paste0(month, "_omak_diff")
                              )
    ##########################################
    #                                        #
    #         read the data off the disk     #
    #                                        #
    ##########################################

    data = data.table(readRDS(paste0(data_dir, name_pref, month, name_post)))
    data <- data %>% filter(Year <= 2005 | Year > 2075)
    data <- data %>% filter(scenario== "historical" | scenario== "rcp85")

    data$CountyGroup = 0L
    data$CountyGroup[data$location == "48.40625_-119.53125"] = "omak"
    data$CountyGroup[data$location == "46.28125_-119.34375"] = "rich"

    data = within(data, remove(model, location, Month))

    data$ClimateGroup[data$Year <= 2005] <- "1950-2005"
    data$ClimateGroup[data$Year > 2075] <- "2076-2099"

    # order the climate groups
    data$ClimateGroup <- factor(data$ClimateGroup, levels = c("1950-2005", "2076-2099"))

    ##########################################
    #                                        #
    #         separate scenarios             #
    #                                        #
    ##########################################
    data_hist = data %>% filter(scenario %in% c("historical"))
    data_85   = data %>% filter(scenario %in% c("rcp85"))
    # rm(data)
   
    ##########################################
    #                                        #
    #          pick up proper years          #
    #                                        #
    ##########################################
    data_hist <- data_hist %>% 
                 filter(Year > 1950 & Year <= 2005,
                        Chill_season != "chill_1949-1950" &
                        Chill_season != "chill_2005-2006")

    ########################################### 85
    data_85_2076_2099 <- data_85  %>% 
                         filter(Year > 2075 & Year <= 2099,
                                Chill_season != "chill_2075-2076" &
                                Chill_season != "chill_2099-2100")
    # rm(data_85)

    ##########################################
    #                                        #
    #          separate warm/cool.           #
    #                                        #
    ########################################## H
    data_hist_w = data_hist %>% filter(CountyGroup == "rich")
    data_hist_c = data_hist %>% filter(CountyGroup == "omak")
    # rm(data_hist)
    ########################################## 85
    data_85_2076_2099_w = data_85_2076_2099 %>% filter(CountyGroup == "rich")
    data_85_2076_2099_c = data_85_2076_2099 %>% filter(CountyGroup == "omak")
    # rm(data_85_2076_2099)
    
    ##########################################
    #                                        #
    #          populate the table            #
    #                                        #
    ##########################################    
    ################
    #   Richland   #
    ################
    v = data_hist_w %>% 
        mutate(temp_cat = cut(Temp, breaks = iof_breaks)) %>% 
        group_by(temp_cat) %>% 
        summarise(no_hours = n()) %>% data.table()
    df_help[1, 2:8] <- v$no_hours

    v = data_85_2076_2099_w %>% 
        mutate(temp_cat = cut(Temp, breaks = iof_breaks)) %>% 
        group_by(temp_cat) %>% 
        summarise(no_hours = n()) %>% data.table()
    df_help[2, 2:8] <- v$no_hours

    df_help[3, 2:8] = df_help[2, 2:8] - df_help[1, 2:8]
    ################
    #     Omak     #
    ################
    v = data_hist_c %>% 
        mutate(temp_cat = cut(Temp, breaks = iof_breaks)) %>% 
        group_by(temp_cat) %>% 
        summarise(no_hours = n()) %>% data.table()
    df_help[4, 2:8] <- v$no_hours

    v = data_85_2076_2099_c %>% 
        mutate(temp_cat = cut(Temp, breaks = iof_breaks)) %>% 
        group_by(temp_cat) %>% 
        summarise(no_hours = n()) %>% data.table()
    df_help[5, 2:8] <- v$no_hours

    df_help[6, 2:8] = df_help[5, 2:8] - df_help[4, 2:8]
    
    the_table = rbind(the_table, df_help)
     
}

actual_times = the_table

# drop the column name. It messes up the row-sum computation for example.
numeric_actual_time = actual_times[, 2:8]
sum_rows = rowSums(numeric_actual_time)

numeric_percentages_time = numeric_actual_time / abs(sum_rows)

####### We want the difference between actual percentages,
# The divition above messes up the differences, it does not map the actual
# difference to actual percentage-difference. So, we have to do the following correction.
numeric_percentages_time[3,] = numeric_percentages_time[2,] - numeric_percentages_time[1,]
numeric_percentages_time[6,] = numeric_percentages_time[5,] - numeric_percentages_time[4,]
numeric_percentages_time[9,] = numeric_percentages_time[8,] - numeric_percentages_time[7,]
numeric_percentages_time[12,]= numeric_percentages_time[11,]- numeric_percentages_time[10,]

numeric_percentages_time$Four_to_13 = numeric_percentages_time[, 4] + 
                                      numeric_percentages_time[, 5] + 
                                      numeric_percentages_time[, 6]

numeric_percentages_time = numeric_percentages_time*100
# put back the name column in there
perc_table <- cbind(the_table[, 1], numeric_percentages_time)

write.table(x = perc_table, row.names=F, col.names = T, sep=",",
            file = paste0("/Users/hn/Desktop/", "perc_table.csv"))

write.table(x =actual_times, row.names=F, col.names = T, sep=",",
            file = paste0("/Users/hn/Desktop/", "actual_table.csv"))


