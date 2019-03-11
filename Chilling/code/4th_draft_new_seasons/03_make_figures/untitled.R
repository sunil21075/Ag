
######################################################################
#                                                                    #
#           The colorful box plots where colors indicates            #
#           level of risk. We do not need RCP 4.5 and                #
#           There we go:                                             #
#           (and we want to average over all models and locations)   #
#                                                                    #
######################################################################
jan_result <- jan_result %>% filter(scenario=="Historical" | scenario=="RCP 8.5")

data <- jan_result
data <- data %>% filter(scenario=="Historical" | scenario=="RCP 8.5")
data <- within(data, remove(lat, long, no_years, n_years_passed))

data_w <- data %>% filter(climate_type == "Warmer Area")
data_c <- data %>% filter(climate_type == "Cooler Area")

data_w <- within(data_w, remove(climate_type))
data_c <- within(data_c, remove(climate_type))
        
result <- data_w %>% 
          group_by(time_period, thresh_range, model, scenario) %>% 
          aggregate(average_met = mean(frac_passed)) %>% 
          data.table()

feb_result <- feb_result %>% filter(scenario=="Historical" | scenario=="RCP 8.5")
mar_result <- mar_result %>% filter(scenario=="Historical" | scenario=="RCP 8.5")









