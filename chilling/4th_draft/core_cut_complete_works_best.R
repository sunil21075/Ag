count_years_threshs_met_all_locations <- function(dataT, due){
  h_year_count <- length(unique(dataT[dataT$time_period =="Historical",]$chill_season))
  f1_year_count <- length(unique(dataT[dataT$time_period== "2025_2050",]$chill_season))
  f2_year_count <- length(unique(dataT[dataT$time_period== "2051_2075",]$chill_season))
  f3_year_count <- length(unique(dataT[dataT$time_period== "2076_2099",]$chill_season))
  if (due == "Jan"){
    col_name = "sum_J1"
    } else if(due == "Feb"){
      col_name = "sum_F1"
    } else if(due =="Mar"){
      col_name = "sum_M1"
    } else if(due =="Apr"){
      col_name = "sum_A1"
  }

  bks = c(-300, seq(20, 75, 5), 300)

  dataT$location = paste0(dataT$lat, "_", dataT$long)
  dataT <- within(dataT, remove("lat", "long"))

  dataT_hist <- dataT %>% filter(scenario == "Historical")
  dataT_45 <- dataT %>% filter(scenario == "RCP 4.5")
  dataT_85 <- dataT %>% filter(scenario == "RCP 8.5")

  dataT_hist <- droplevels(dataT_hist)
  dataT_45 <- droplevels(dataT_45)
  dataT_85 <- droplevels(dataT_85)

  result_85 <- dataT_85 %>%
               mutate(thresh_range = cut(get(col_name), breaks = bks)) %>%
               tidyr::complete(time_period, thresh_range, model, location) %>%
               group_by(time_period, thresh_range, model, location) %>%
               summarize(no_years = n_distinct(chill_season, na.rm = TRUE)) %>% 
               data.table()

  result_45 <- dataT_45 %>%
               mutate(thresh_range = cut(get(col_name), breaks = bks)) %>%
               tidyr::complete(time_period, thresh_range, model, location) %>%
               group_by(time_period, thresh_range, model, location) %>%
               summarize(no_years = n_distinct(chill_season, na.rm = TRUE)) %>% 
               data.table()

  result_H <- dataT_hist %>%
              mutate(thresh_range = cut(get(col_name), breaks = bks)) %>%
              tidyr::complete(time_period, thresh_range, model, location) %>%
              group_by(time_period, thresh_range, model, location) %>%
              summarize(no_years = n_distinct(chill_season, na.rm = TRUE)) %>% 
              data.table()

  # we do this, so historical appears in both plots
  result_85 <- rbind(result_85, result_H)
  result_45 <- rbind(result_45, result_H)

  result_85$scenario <- "RCP 8.5"
  result_45$scenario <- "RCP 4.5"

  result <- rbind(result_45, result_85)

  time_periods = c("Historical", "2025_2050", "2051_2075", "2076_2099")
  result$time_period = factor(result$time_period, levels=time_periods, order=T)
  
  result$thresh_range <- factor(result$thresh_range, order=T)
  result$thresh_range <- fct_rev(result$thresh_range)
  result <- result[order(thresh_range), ]

  result <- result %>% 
            group_by(time_period, model, scenario, location) %>% 
            mutate(n_years_passed = cumsum(no_years)) %>% 
            data.table()
  
  result_hist <- result %>% filter(time_period == "Historical") %>% data.table()
  result_50 <- result %>% filter(time_period == "2025_2050") %>% data.table()
  result_75 <- result %>% filter(time_period == "2051_2075") %>% data.table()
  result_99 <- result %>% filter(time_period == "2076_2099") %>% data.table()
  
  result_hist$frac_passed = result_hist$n_years_passed / h_year_count
  result_50$frac_passed = result_50$n_years_passed / f1_year_count
  result_75$frac_passed = result_75$n_years_passed / f2_year_count
  result_99$frac_passed = result_99$n_years_passed / f3_year_count

  result <- rbind(result_hist, result_50, result_75, result_99)
  result <- na.omit(result)
  return(result)
}

count_years_threshs_met_limit_location <- function(dataT, due){
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
    } else if(due =="Apr"){
      col_name = "sum_A1"
  }

  bks = c(-300, seq(20, 75, 5), 300)

  # df_help[1, 2:8] = table(cut(data_hist_rich$Temp, breaks = iof_breaks))
  dataT_hist <- dataT %>% filter(scenario == "Historical")
  dataT_45 <- dataT %>% filter(scenario == "RCP 4.5")
  dataT_85 <- dataT %>% filter(scenario == "RCP 8.5")

  dataT_hist <- droplevels(dataT_hist)
  dataT_45 <- droplevels(dataT_45)
  dataT_85 <- droplevels(dataT_85)

  result_85 <- dataT_85 %>%
               mutate(thresh_range = cut(get(col_name), breaks = bks)) %>%
               complete(time_period, thresh_range, model, city) %>%
               group_by(time_period, thresh_range, model, city) %>%
               summarize(no_years = n_distinct(chill_season, na.rm = TRUE)) %>% 
               data.table()

  result_45 <- dataT_45 %>%
               mutate(thresh_range = cut(get(col_name), breaks = bks)) %>%
               complete(time_period, thresh_range, model, city) %>%
               group_by(time_period, thresh_range, model, city) %>%
               summarize(no_years = n_distinct(chill_season, na.rm = TRUE)) %>% 
               data.table()

  result_H <- dataT_hist %>%
              mutate(thresh_range = cut(get(col_name), breaks = bks)) %>%
              complete(time_period, thresh_range, model, city) %>%
              group_by(time_period, thresh_range, model, city) %>%
              summarize(no_years = n_distinct(chill_season, na.rm = TRUE)) %>% 
              data.table()

  # we do this, so historical appears in both plots
  result_85 <- rbind(result_85, result_H)
  result_45 <- rbind(result_45, result_H)

  result_85$scenario <- "RCP 8.5"
  result_45$scenario <- "RCP 4.5"

  result <- rbind(result_45, result_85)

  time_periods = c("Historical", "2025_2050", "2051_2075", "2076_2099")
  result$time_period = factor(result$time_period, levels=time_periods, order=T)
  
  result$thresh_range <- factor(result$thresh_range, order=T)
  result$thresh_range <- fct_rev(result$thresh_range)
  result <- result[order(thresh_range), ]

  result <- result %>% 
            group_by(time_period, model, scenario, city) %>% 
            mutate(n_years_passed = cumsum(no_years)) %>% 
            data.table()
  
  result_hist <- result %>% filter(time_period == "Historical") %>% data.table()
  result_50 <- result %>% filter(time_period == "2025_2050") %>% data.table()
  result_75 <- result %>% filter(time_period == "2051_2075") %>% data.table()
  result_99 <- result %>% filter(time_period == "2076_2099") %>% data.table()
  
  result_hist$frac_passed = result_hist$n_years_passed / h_year_count
  result_50$frac_passed = result_50$n_years_passed / f1_year_count
  result_75$frac_passed = result_75$n_years_passed / f2_year_count
  result_99$frac_passed = result_99$n_years_passed / f3_year_count

  result <- rbind(result_hist, result_50, result_75, result_99)
  result <- na.omit(result)
  return(result)
}