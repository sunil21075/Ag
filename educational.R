####################################################################################
########################################## Educational - For reference
# file %>%
# filter(Year > 2025 & Year <= 2055,
# Chill_season != "chill_2025-2026" &
# Chill_season != "chill_2055-2056") %>% 
# group_by(Chill_season) %>%
##########################################

# assign(x = paste0(month, "_density_plot_", "rcp45"),
#        value = {plot_dens(data=data_45, month_name=month)})

##########################################
# df[4, 5] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)

A_filtered <- A %>% filter_all(any_vars(is.na(.)))
A <- summary_comp %>% filter_all(any_vars(is.na(.)))
A <- summary_compy %>% filter_all(any_vars(is.na(.)))
A <- summary_compy %>% filter(any_vars(is.na(.)))
A <- A %>% filter_all(any_vars(is.na(.)))

######## The same
A = A[, .(mean_gdd = mean(CumDDinF)), 
         by = c("location", "year")]

B <- B %>%
     group_by(location, year) %>%
     summarise_at(.funs = funs(mean(., na.rm=TRUE)), vars(CumDDinF))%>% 
     data.table()
######## 
# Chnage name of a columns
colnames(data)[colnames(data)=="old_name"] <- "new_name"
setnames(data, old=c("old_name","another_old_name"), new=c("new_name", "another_new_name"))

# order a data by a/multiple column. Adding a negative would make the ordering reverse
A <- A[order(location), ]

result <- dataT %>%
            mutate(thresh_range = cut(get(col_name), breaks = bks )) %>%
            group_by(lat, long, climate_type, time_period, 
                     thresh_range, model, scenario) %>%
            summarize(no_years = n_distinct(Chill_season)) %>% 
            data.table()


quan_per_feb <- feb_result %>% 
                group_by(climate_type, time_period, scenario, thresh_range) %>% 
                summarise(quan_25 = quantile(frac_passed, probs = 0.25)) %>% 
                data.table()



