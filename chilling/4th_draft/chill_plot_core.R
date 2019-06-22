
safe_box_plot <- function(data, due, chill_start){
  color_ord = c("grey47" , "dodgerblue", "olivedrab4", "red") # 
  categ_lab = c("Historical", "2025-2050", "2051-2075", "2076-2099")
  box_width = 0.25
  
  df <- data.frame(data)
  df <- (df %>% group_by(time_period, scenario, warm_cold))
  medians <- (df %>% summarise(med = median(quan_90)))
  medians_vec <- medians$med
  
  the_theme = theme(plot.margin = unit(c(t=.2, r=.2, b=.2, l=0.2), "cm"),
                    panel.border = element_rect(fill=NA, size=.3),
                    panel.grid.major = element_line(size = 0.05),
                    panel.grid.minor = element_blank(),
                    panel.spacing = unit(.25, "cm"),
                    legend.position = "bottom", 
                    legend.key.size = unit(1.6, "line"),
                    legend.spacing.x = unit(.2, 'cm'),
                    panel.spacing.y = unit(.5, 'cm'),
                    legend.text = element_text(size=16),
                    legend.margin = margin(t=0, r=0, b=0, l=0, unit = 'cm'),
                    legend.title = element_blank(),
                    plot.title = element_text(size = 20, face = "bold"),
                    plot.subtitle = element_text(face = "bold"),
                    strip.text.x = element_text(size=18, face="bold"),
                    strip.text.y = element_text(size=18, face="bold"),
                    axis.ticks = element_line(size=.1, color="black"),
                    axis.text.y = element_text(size=14, face="bold", color="black"),
                    axis.title.y = element_text(size=22, face="bold", margin = margin(t=0, r=10, b=0, l=0)),
                    axis.text.x = element_blank(),
                    axis.title.x = element_blank()
                    )
  
  safe_b <- ggplot(data = data, aes(x=time_period, y=quan_90, fill=time_period)) +
            geom_boxplot(outlier.size=-.25, notch=F, width=box_width, lwd=.1) +
            labs(x="", y="safe chill") +
            facet_grid(~ scenario ~ warm_cold ) + 
            the_theme + 
            scale_fill_manual(values = color_ord,
                              name = "Time\nPeriod", 
                              labels = categ_lab) + 
            scale_color_manual(values = color_ord,
                               name = "Time\nPeriod", 
                               limits = color_ord,
                               labels = categ_lab) + 
            scale_x_discrete(breaks = c("Historical", "2025_2050", "2051_2075", "2076_2099"),
                             labels = categ_lab)  +
            geom_text(data = medians, 
                      aes(label = sprintf("%1.0f", medians$med), y=medians$med), 
                          size=4.2, 
                          position =  position_dodge(.09),
                          vjust = 0.1, hjust=1.45) + 
            ggtitle(lab=paste0("Safe chill accumulation by ", due, " 1st"),
                    subtitle = paste0("chill season started on ", chill_start)) 
  
  return(safe_b)
}

ensemble_map <- function(data, color_col, due) {
  states <- map_data("state")
  states_cluster <- subset(states, region %in% c("oregon", "washington", "idaho"))

  if (color_col=="mean_over_model"){
     low_lim = min(data$mean_over_model)
     up_lim = max(data$mean_over_model)
  } else if (color_col=="median_over_model"){
     low_lim = min(data$median_over_model)
     up_lim = max(data$median_over_model)
  }
  
  data %>% ggplot() +
           geom_polygon(data = states_cluster, aes(x=long, y=lat, group = group),
                        fill = "grey", color = "black", size=.3) +
            # aes_string to allow naming of column in function 
            geom_point(aes_string(x = "long", y = "lat",
                       color = color_col), alpha = 0.4, size=.4) +
           coord_fixed(xlim = c(-124.5, -111.4),  ylim = c(41, 50.5), ratio = 1.3) +
           facet_grid(~ scenario ~ time_period) +
           ggtitle(paste0("Ensemble ", unlist(strsplit(color_col, "_"))[1] , " by ", due, " 1st")) + 
           theme_bw() + 
           theme(legend.position = "bottom",
                 legend.title = element_blank(),
                 legend.key.size = unit(1.4, "line"),
                 plot.margin = margin(t=0, r=0.2, b=0, l=0.2, unit = 'cm')
                 # axis.text.x = element_text(size=3, face="plain", color="black"),
                 # axis.text.y = element_text(size=3, face="plain", color="black"),
                 # legend.margin = margin(t=0, r=0, b=-0.1, l=0, unit = 'cm')
                ) +
           scale_color_gradient2(midpoint=(low_lim + up_lim)/2, low="red", mid="white", high="blue", 
                                 space ="Lab")
}

produce_data_4_plots <- function(data){
  needed_cols = c("chill_season", "sum_J1", "sum_F1", "sum_M1", "sum_A1", "year", "model", 
                  "scenario", "lat", "long", "warm_cold")

  ################### CLEAN DATA
  data = subset(data, select=needed_cols)
  # pick up future data
  # data = data %>% filter(scenario != "historical")
  data <- organize_non_over_time_period_two_hist(data)

  ################### GENERATE STATS
  #######################################################################
  ##                                                                   ##
  ##   Find the 90th percentile of the chill units                     ##
  ##   Grouped by location, model, time_period and rcp                 ##
  ##   This could be used for box plots, later compute the mean.       ##
  ##   for maps                                                        ##
  ##                                                                   ##
  #######################################################################
  quan_per_loc_period_model_jan <- data %>% 
                                   group_by(time_period, lat, long, scenario, model, warm_cold) %>%
                                   summarise(quan_90 = quantile(sum_J1, probs = 0.1)) %>%
                                   data.table()
  
  quan_per_loc_period_model_feb <- data %>% 
                                   group_by(time_period, lat, long, scenario, model, warm_cold) %>%
                                   summarise(quan_90 = quantile(sum_F1, probs = 0.1)) %>%
                                   data.table()

  quan_per_loc_period_model_mar <- data %>% 
                                   group_by(time_period, lat, long, scenario, model, warm_cold) %>%
                                   summarise(quan_90 = quantile(sum_M1, probs = 0.1)) %>%
                                   data.table()

  quan_per_loc_period_model_apr <- data %>% 
                                   group_by(time_period, lat, long, scenario, model, warm_cold) %>%
                                   summarise(quan_90 = quantile(sum_A1, probs = 0.1)) %>%
                                   data.table()

  # it seems there is a library, perhaps tidyverse, that messes up
  # the above line, so the two variables above are 1-by-1. 
  # just close and re-open R Studio
  ########################################################################
  #######                                                          #######
  #######                     Means                              #######
  #######                                                          #######
  ########################################################################
  
  mean_quan_per_loc_period_model_jan <- quan_per_loc_period_model_jan %>%
                                        group_by(time_period, lat, long, scenario) %>%
                                        summarise(mean_over_model = mean(quan_90)) %>%
                                        data.table()

  mean_quan_per_loc_period_model_feb <- quan_per_loc_period_model_feb %>%
                                        group_by(time_period, lat, long, scenario) %>%
                                        summarise(mean_over_model = mean(quan_90)) %>%
                                        data.table()
  
  mean_quan_per_loc_period_model_mar <- quan_per_loc_period_model_mar %>%
                                        group_by(time_period, lat, long, scenario) %>%
                                        summarise(mean_over_model = mean(quan_90)) %>%
                                        data.table()

  mean_quan_per_loc_period_model_apr <- quan_per_loc_period_model_apr %>%
                                        group_by(time_period, lat, long, scenario) %>%
                                        summarise(mean_over_model = mean(quan_90)) %>%
                                        data.table()
  ########################################################################
  #######                                                          #######
  #######                     Medians                              #######
  #######                                                          #######
  ########################################################################
  median_quan_per_loc_period_model_jan <- quan_per_loc_period_model_jan %>%
                                          group_by(time_period, lat, long, scenario) %>%
                                          summarise(median_over_model = median(quan_90)) %>%
                                          data.table()

  median_quan_per_loc_period_model_feb <- quan_per_loc_period_model_feb %>%
                                          group_by(time_period, lat, long, scenario) %>%
                                          summarise(median_over_model = median(quan_90)) %>%
                                          data.table()
  
  median_quan_per_loc_period_model_mar <- quan_per_loc_period_model_mar %>%
                                          group_by(time_period, lat, long, scenario) %>%
                                          summarise(median_over_model = median(quan_90)) %>%
                                          data.table()
  
  median_quan_per_loc_period_model_apr <- quan_per_loc_period_model_apr %>%
                                          group_by(time_period, lat, long, scenario) %>%
                                          summarise(median_over_model = median(quan_90)) %>%
                                          data.table()
  
  # quan_per_loc_period_model_jan$time_period = factor(quan_per_loc_period_model_jan$time_period, order=T)
  # mean_quan_per_loc_period_model_jan$time_period = factor(mean_quan_per_loc_period_model_jan$time_period, order=T)
  # median_quan_per_loc_period_model_jan$time_period = factor(median_quan_per_loc_period_model_jan$time_period, order=T)

  # quan_per_loc_period_model_feb$time_period_feb = factor(quan_per_loc_period_model_feb$time_period, order=T)
  # mean_quan_per_loc_period_model_feb$time_period = factor(mean_quan_per_loc_period_model_feb$time_period, order=T)
  # median_quan_per_loc_period_model_feb$time_period= factor(median_quan_per_loc_period_model_feb$time_period, order=T)

  return(list(quan_per_loc_period_model_jan,
              mean_quan_per_loc_period_model_jan,
              median_quan_per_loc_period_model_jan,
              quan_per_loc_period_model_feb,
              mean_quan_per_loc_period_model_feb,
              median_quan_per_loc_period_model_feb,
              quan_per_loc_period_model_mar,
              mean_quan_per_loc_period_model_mar,
              median_quan_per_loc_period_model_mar,
              quan_per_loc_period_model_apr,
              mean_quan_per_loc_period_model_apr,
              median_quan_per_loc_period_model_apr
              )
        )
}

produce_data_4_safe_chill_box_plots_new_seasons <- function(data){

  ################### CLEAN DATA
  data <- organize_non_over_time_period_two_hist(data)

  ################### GENERATE STATS
  #######################################################################
  ##                                                                   ##
  ##   Find the 90th percentile of the chill units                     ##
  ##   Grouped by location, model, time_period and rcp                 ##
  ##   This could be used for box plots, later compute the mean.       ##
  ##   for maps                                                        ##
  ##                                                                   ##
  #######################################################################
  quan_per_loc_period_model_jan <- data %>% 
                                   group_by(time_period, lat, long, scenario, model, start) %>%
                                   summarise(quan_90 = quantile(sum_J1, probs = 0.1)) %>%
                                   data.table()
  
  quan_per_loc_period_model_feb <- data %>% 
                                   group_by(time_period, lat, long, scenario, model, start) %>%
                                   summarise(quan_90 = quantile(sum_F1, probs = 0.1)) %>%
                                   data.table()

  quan_per_loc_period_model_mar <- data %>% 
                                   group_by(time_period, lat, long, scenario, model, start) %>%
                                   summarise(quan_90 = quantile(sum_M1, probs = 0.1)) %>%
                                   data.table()

  quan_per_loc_period_model_apr <- data %>% 
                                   group_by(time_period, lat, long, scenario, model, start) %>%
                                   summarise(quan_90 = quantile(sum_A1, probs = 0.1)) %>%
                                   data.table()

  # it seems there is a library, perhaps tidyverse, that messes up
  # the above line, so the two variables above are 1-by-1. 
  # just close and re-open R Studio
  ########################################################################
  #######                                                          #######
  #######                     Means                              #######
  #######                                                          #######
  ########################################################################
  
  mean_quan_per_loc_period_model_jan <- quan_per_loc_period_model_jan %>%
                                        group_by(time_period, lat, long, scenario, start) %>%
                                        summarise(mean_over_model = mean(quan_90)) %>%
                                        data.table()

  mean_quan_per_loc_period_model_feb <- quan_per_loc_period_model_feb %>%
                                        group_by(time_period, lat, long, scenario, start) %>%
                                        summarise(mean_over_model = mean(quan_90)) %>%
                                        data.table()
  
  mean_quan_per_loc_period_model_mar <- quan_per_loc_period_model_mar %>%
                                        group_by(time_period, lat, long, scenario, start) %>%
                                        summarise(mean_over_model = mean(quan_90)) %>%
                                        data.table()

  mean_quan_per_loc_period_model_apr <- quan_per_loc_period_model_apr %>%
                                        group_by(time_period, lat, long, scenario, start) %>%
                                        summarise(mean_over_model = mean(quan_90)) %>%
                                        data.table()
  ########################################################################
  #######                                                          #######
  #######                     Medians                              #######
  #######                                                          #######
  ########################################################################
  median_quan_per_loc_period_model_jan <- quan_per_loc_period_model_jan %>%
                                          group_by(time_period, lat, long, scenario, start) %>%
                                          summarise(median_over_model = median(quan_90)) %>%
                                          data.table()

  median_quan_per_loc_period_model_feb <- quan_per_loc_period_model_feb %>%
                                          group_by(time_period, lat, long, scenario, start) %>%
                                          summarise(median_over_model = median(quan_90)) %>%
                                          data.table()
  
  median_quan_per_loc_period_model_mar <- quan_per_loc_period_model_mar %>%
                                          group_by(time_period, lat, long, scenario, start) %>%
                                          summarise(median_over_model = median(quan_90)) %>%
                                          data.table()
  
  median_quan_per_loc_period_model_apr <- quan_per_loc_period_model_apr %>%
                                          group_by(time_period, lat, long, scenario, start) %>%
                                          summarise(median_over_model = median(quan_90)) %>%
                                          data.table()

  return(list(quan_per_loc_period_model_jan,
              mean_quan_per_loc_period_model_jan,
              median_quan_per_loc_period_model_jan,
              quan_per_loc_period_model_feb,
              mean_quan_per_loc_period_model_feb,
              median_quan_per_loc_period_model_feb,
              quan_per_loc_period_model_mar,
              mean_quan_per_loc_period_model_mar,
              median_quan_per_loc_period_model_mar,
              quan_per_loc_period_model_apr,
              mean_quan_per_loc_period_model_apr,
              median_quan_per_loc_period_model_apr
              )
        )
}

organize_non_over_time_period_two_hist <- function(data){
  data = data %>% filter(year<=2005 | year>=2025)
  time_periods = c("Historical","2025_2050", "2051_2075", "2076_2099")
  
  data$time_period = 0L
  data$time_period[data$year <= 2005] = time_periods[1]
  data$time_period[data$year >= 2025 & data$year <= 2050] = time_periods[2]
  data$time_period[data$year >= 2051 & data$year <= 2075] = time_periods[3]
  data$time_period[data$year >= 2076] = time_periods[4]
  
  data$time_period = factor(data$time_period, levels = time_periods, order=T)
  data_f <- data %>% filter(time_period != "Historical")
  
  data_h_rcp85 <- data %>% filter(time_period == "Historical")
  data_h_rcp45 <- data %>% filter(time_period == "Historical")
  
  data_h_rcp85$scenario = "RCP 8.5"
  data_h_rcp45$scenario = "RCP 4.5"
  
  data_f$scenario[data_f$scenario=="rcp85"] = "RCP 8.5"
  data_f$scenario[data_f$scenario=="rcp45"] = "RCP 4.5"

  data = rbind(data_f, data_h_rcp45, data_h_rcp85)
  rm(data_h_rcp45, data_h_rcp85, data_f)
  return(data)
}






