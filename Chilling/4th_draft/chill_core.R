options(digits=9)
options(digits=9)


remove_montana_add_warm_cold <- function(data_dt, LocationGroups_NoMontana){
  if (!("location" %in% colnames(data_dt))){
    data_dt$location <- paste0(data_dt$lat, "_", data_dt$long)
  }
  data_dt <- data_dt %>% filter(location %in% LocationGroups_NoMontana$location)
  data_dt <- left_join(x=data_dt, y=LocationGroups_NoMontana)
  data_dt <- within(data_dt, remove(location))
  return(data_dt)
}
###################################################################
#          **********                            **********
#          **********        WARNING !!!!        **********
#          **********                            **********
##
## DO NOT load any libraries here.
## And do not load any libraries on the drivers!
## Unless you are aware of conflicts between packages.
## I spent hours to figrue out what the hell is going on!

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

########################################
######
######    Folder 02_ stuff
######
########################################

grab_coord <- function(A){
    out_put <- A %>%
               transmute(lat = as.numeric(substr(x = .id, start = 19, stop = 26)),
               long = as.numeric(substr(x = .id, start = 28, stop = 37)),
               median_20 = median_20,
               median_25 = median_25,
               median_30 = median_30,
               median_35 = median_35,
               median_40 = median_40,
               median_45 = median_45,
               median_50 = median_50,
               median_55 = median_55,
               median_60 = median_60,
               median_65 = median_65,
               median_70 = median_70,
               median_75 = median_75,
               median_J1 = median_J1,
               median_F1 = median_F1,
               median_M1 = median_M1,
               median_A1 = median_A1)
    return (out_put)
}

get_medians <- function(a_list){
  medians_data <- ldply(.data = a_list,
                        .fun = function(x) medians(thresh_20 = x[, "thresh_20"],
                                                   thresh_25 = x[, "thresh_25"],
                                                   thresh_30 = x[, "thresh_30"],
                                                   thresh_35 = x[, "thresh_35"],
                                                   thresh_40 = x[, "thresh_40"],
                                                   thresh_45 = x[, "thresh_45"],
                                                   thresh_50 = x[, "thresh_50"],
                                                   thresh_55 = x[, "thresh_55"],
                                                   thresh_60 = x[, "thresh_60"],
                                                   thresh_65 = x[, "thresh_65"],
                                                   thresh_70 = x[, "thresh_70"],
                                                   thresh_75 = x[, "thresh_75"],
                                                   sum_J1 = x[, "sum_J1"],
                                                   sum_F1 = x[, "sum_F1"],
                                                   sum_M1 = x[, "sum_M1"],
                                                   sum_A1 = x[, "sum_A1"]))
  return (medians_data)
}

# A function that pulls the median values we want
medians <- function(thresh_20, thresh_25, thresh_30,
                    thresh_35, thresh_40, thresh_45,
                    thresh_50, thresh_55, thresh_60,
                    thresh_65, thresh_70, thresh_75, 
                    sum_J1, sum_F1, sum_M1, sum_A1) {
  c(median_20 = median(thresh_20),
    median_25 = median(thresh_25),
    median_30 = median(thresh_30),
    median_35 = median(thresh_35),
    median_40 = median(thresh_40),
    median_45 = median(thresh_45),
    median_50 = median(thresh_50),
    median_55 = median(thresh_55),
    median_60 = median(thresh_60),
    median_65 = median(thresh_65),
    median_70 = median(thresh_70),
    median_75 = median(thresh_75),
    median_J1 = median(sum_J1),
    median_F1 = median(sum_F1),
    median_M1 = median(sum_M1),
    median_A1 = median(sum_A1)
    )
}
##################################################
#####                                        #####
#####    this is the overlapping one         #####
#####    the non-overlap is below            #####
#####                                        #####
##################################################
process_data <- function(file, time_period) { # this is for overlapping data, which we never will use!
  if (time_period=="2040"){
    processed_data <- file %>%
                      filter(year >= 2025 & year <= 2055,
                             chill_season != "chill_2025-2026" &
                             chill_season != "chill_2055-2056")
  } else if (time_period=="2060"){
    processed_data <- file %>%
                      filter(year > 2045 & year <= 2075,
                             chill_season != "chill_2045-2046" &
                             chill_season != "chill_2075-2076")
  } else if (time_period=="2080") {
     processed_data <- file %>%
                       filter(year > 2065 & year <= 2095,
                              chill_season != "chill_2065-2066" &
                              chill_season != "chill_2095-2096")
  }
  processed_data <- threshold_func(processed_data, data_type="modeled")
  return(processed_data)
}

##################################################
#####                                        #####
#####    this is the non-overlapping one     #####
#####                                        #####
##################################################
process_data_non_overlap <- function(file, time_period) {
  if (time_period == "2025_2050"){
    processed_data <- file %>%
                      filter(year >= 2025 & year <= 2050,
                             chill_season != "chill_2025-2026" &
                             chill_season != "chill_2050-2051")
   } else if (time_period == "2051_2075"){
    processed_data <- file %>%
                      filter(year > 2050 & year <= 2075,
                             chill_season != "chill_2050-2051" &
                             chill_season != "chill_2075-2076")
   } else if (time_period == "2076_2100") {
     processed_data <- file %>%
                       filter(year > 2075 & year <= 2099,
                              chill_season != "chill_2075-2076" &
                              chill_season != "chill_2099-2100")
   
   } else if (time_period == "2005_2024") {
    processed_data <- file %>%
                      filter(year >= 2005 & year <= 2024,
                             chill_season != "chill_2005-2006" &
                             chill_season != "chill_2024-2025")
  }
  processed_data <- threshold_func(processed_data, data_type="modeled")
  return(processed_data)
}

threshold_func <- function(file, data_type){
  if (data_type=="modeled"){
    data <- file %>%
           # Only want complete seasons of data
           filter(chill_season != "chill_1949-1950" & chill_season != "chill_2005-2006")
   } else {
    data <- file %>%
            # Only want complete seasons of data
            filter(chill_season != "chill_1978-1979" & chill_season != "chill_2015-2016")
  } 
  data <- data %>% 
          # Within a season
          group_by(chill_season) %>%
          # Mutate output is the row index of the first 
          # time where it meets threshold
          # within the group. (Index is the same as counting 
          # the start date as day = 1)
          mutate(thresh_20 = detect_index(.x = cume_portions,
                                          .f = chill_thresh,
                                          threshold = 20),
                 thresh_25 = detect_index(.x = cume_portions,
                                          .f = chill_thresh,
                                          threshold = 25),
                 thresh_30 = detect_index(.x = cume_portions,
                                          .f = chill_thresh,
                                          threshold = 30),
                 thresh_35 = detect_index(.x = cume_portions,
                                          .f = chill_thresh,
                                          threshold = 35),
                 thresh_40 = detect_index(.x = cume_portions,
                                          .f = chill_thresh,
                                          threshold = 40),
                 thresh_45 = detect_index(.x = cume_portions,
                                          .f = chill_thresh,
                                          threshold = 45),
                 thresh_50 = detect_index(.x = cume_portions,
                                          .f = chill_thresh,
                                          threshold = 50),
                 thresh_55 = detect_index(.x = cume_portions,
                                          .f = chill_thresh,
                                          threshold = 55),
                 thresh_60 = detect_index(.x = cume_portions,
                                          .f = chill_thresh,
                                          threshold = 60),
                 thresh_65 = detect_index(.x = cume_portions,
                                          .f = chill_thresh,
                                          threshold = 65),
                 thresh_70 = detect_index(.x = cume_portions,
                                          .f = chill_thresh,
                                          threshold = 70),
                 thresh_75 = detect_index(.x = cume_portions,
                                          .f = chill_thresh,
                                          threshold = 75),
                 # Below we set the row where, ex. month = 1 (jan) and day = 1 to be
                 # equal to the value of cume_portions. This creates one non-NA row
                 # for the whole column. Then in summarise we remove all NAs, which
                 # collapses the column to a single value -- perfect for a one-line
                 # summary per season.
                 sum_J1 = case_when(month == 1 & day == 1 ~ cume_portions),
                 sum_F1 = case_when(month == 2 & day == 1 ~ cume_portions),
                 sum_M1 = case_when(month == 3 & day == 1 ~ cume_portions),
                 sum_A1 = case_when(month == 4 & day == 1 ~ cume_portions)) %>% 
          summarise(sum = sum(daily_portions),
                    thresh_20 = unique(thresh_20), # retain the thresholds
                    thresh_25 = unique(thresh_25),
                    thresh_30 = unique(thresh_30),
                    thresh_35 = unique(thresh_35),
                    thresh_40 = unique(thresh_40),
                    thresh_45 = unique(thresh_45),
                    thresh_50 = unique(thresh_50),
                    thresh_55 = unique(thresh_55),
                    thresh_60 = unique(thresh_60),
                    thresh_65 = unique(thresh_65),
                    thresh_70 = unique(thresh_70),
                    thresh_75 = unique(thresh_75),
                    sum_J1 = na.omit(sum_J1),
                    sum_F1 = na.omit(sum_F1),
                    sum_M1 = na.omit(sum_M1),
                    sum_A1 = na.omit(sum_A1)) %>%
          data.frame() # to allow for ldply() later
  return(data)
}

# A function to check against a threshold
chill_thresh <- function(x, threshold) {x >= threshold}

###################################################################

put_chill_season <- function(met_hourly_dt, chill_start = "sept"){

  if (chill_start == "sept"){
    #########################
    #
    # Chill season start at Sep.
    #
    #########################
    met_hourly_dt <- met_hourly_dt %>%
                     mutate(chill_season = case_when(
                     # If Jan:Aug then part of chill season of prev year - current year
                     month %in% c(1:8) ~ paste0("chill_", (year - 1), "-", year),
                     # If Sept:Dec then part of chill season of current year - next year
                     month %in% c(9:12) ~ paste0("chill_", year, "-", (year + 1))
                     ))
    } else if (chill_start == "mid_sept"){
      #########################
      #
      # Chill season start at Mid Sep.
      #
      #########################
      met_hourly_dt <- met_hourly_dt %>%
                       mutate(chill_season = case_when(
                       # If Jan:Sept_15th then part of chill season of prev year - current year                
                       month %in% c(1:8) ~ paste0("chill_", (year - 1), "-", year),
                       ((month %in% c(9)) & (day <= 15)) ~ paste0("chill_", (year - 1), "-", year),
                       
                       # If Sept_16th:Dec then part of chill season of current year - next year
                       ((month %in% c(9)) & (day >= 16)) ~ paste0("chill_", year, "-", (year + 1)),
                        (month %in% c(10:12)) ~ paste0("chill_", year, "-", (year + 1))
                       ))
      } else if (chill_start == "oct"){
      #########################
      #
      # Chill season start at Oct
      #
      #########################
      met_hourly_dt <- met_hourly_dt %>%
                        mutate(chill_season = case_when(
                        # If Jan:Sept then part of chill season of prev year - current year
                        month %in% c(1:9) ~ paste0("chill_", (year - 1), "-", year),
                        # If Oct:Dec then part of chill season of current year - next year
                        month %in% c(10:12) ~ paste0("chill_", year, "-", (year + 1))
                        ))
      } else if (chill_start == "mid_oct"){
      #########################
      #
      # Chill season start at Mid Oct
      #
      #########################
      met_hourly_dt <- met_hourly_dt %>%
                       mutate(chill_season = case_when(
                       # If Jan:oct_15th then part of chill season of prev year - current year                
                       month %in% c(1:9) ~ paste0("chill_", (year - 1), "-", year),
                       ((month %in% c(10)) & (day <= 15)) ~ paste0("chill_", (year - 1), "-", year),
                       # If oct_16th:Dec then part of chill season of current year - next year
                       ((month %in% c(10)) & (day >= 16)) ~ paste0("chill_", year, "-", (year + 1)),
                       month %in% c(11:12) ~ paste0("chill_", year, "-", (year + 1))
                       ))
    } else if (chill_start == "nov"){
      #########################
      #
      # Chill season start at Nov
      #
      #########################
      met_hourly_dt <- met_hourly_dt %>%
                        mutate(chill_season = case_when(
                        # If Jan:Nov then part of chill season of prev year - current year
                        month %in% c(1:10) ~ paste0("chill_", (year - 1), "-", year),
                        # If Nov:Dec then part of chill season of current year - next year
                        month %in% c(11:12) ~ paste0("chill_", year, "-", (year + 1))
                        ))
    } else if (chill_start == "mid_nov"){
      #########################
      #
      # Chill season start at Mid Nov
      #
      #########################
      met_hourly_dt <- met_hourly_dt %>%
                       mutate(chill_season = case_when(
                       # If Jan:Nov_15th then part of chill season of prev year - current year                
                       month %in% c(1:10) ~ paste0("chill_", (year - 1), "-", year),
                       ((month %in% c(11)) & (day <= 15)) ~ paste0("chill_", (year - 1), "-", year),
                       # If Nov_16th:Dec then part of chill season of current year - next year
                       ((month %in% c(11)) & (day >= 16)) ~ paste0("chill_", year, "-", (year + 1)),
                       month %in% c(12) ~ paste0("chill_", year, "-", (year + 1))
                       ))
  }
  return(met_hourly_dt)
}
# 1. Prep binary conversion functions ------------------------
# Define function for reading binary 8-col files

read_binary <- function(file_path, hist, no_vars){
  ######    The modeled historical is in /data/hydro/jennylabcommon2/metdata/maca_v2_vic_binary/
  ######    modeled historical is equivalent to having 4 variables, and years 1950-2005
  ######
  ######    The observed historical is in 
  ######    /data/hydro/jennylabcommon2/metdata/historical/UI_historical/VIC_Binary_CONUS_to_2016
  ######    observed historical is equivalent to having 8 variables, and years 1979-2016
  ######
  if (hist) {
    if (no_vars==4){
      start_year <- 1950
      end_year <- 2005
      } else {
        start_year <- 1979
        end_year <- 2015
      }
  } else{
    start_year <- 2006
    end_year <- 2099
  }
  ymd_file <- create_ymdvalues(start_year, end_year)
  data <- read_binary_addmdy(file_path, ymd_file, no_vars)
  return(data)
}

read_binary_addmdy <- function(filename, ymd, no_vars){
  if (no_vars==4){
    return(read_binary_addmdy_4var(filename, ymd))
  } else {return(read_binary_addmdy_8var(filename, ymd))}
}

read_binary_addmdy_8var <- function(filename, ymd){
    Nofvariables <- 8 # number of variables or column in the forcing data file
    Nrecords <- nrow(ymd)
    ind <- seq(1, Nrecords * Nofvariables, Nofvariables)
    fileCon  <-  file(filename, "rb")
    temp <- readBin(fileCon, integer(), size = 2, n = Nrecords * Nofvariables,
                    endian = "little")
    dataM <- matrix(0, Nrecords, 8)
    k <- 1
    dataM[1:Nrecords, 1] <- temp[ind] / 40.00         # precip data
    dataM[1:Nrecords, 2] <- temp[ind + 1] / 100.00    # Max temperature data
    dataM[1:Nrecords, 3] <- temp[ind + 2] / 100.00    # Min temperature data
    dataM[1:Nrecords, 4] <- temp[ind + 3] / 100.00    # Wind speed data
    dataM[1:Nrecords, 5] <- temp[ind + 4] / 10000.00  # SPH
    dataM[1:Nrecords, 6] <- temp[ind + 5] / 40.00     # SRAD
    dataM[1:Nrecords, 7] <- temp[ind + 6] / 100.00    # Rmax
    dataM[1:Nrecords, 8] <- temp[ind + 7] / 100.00    # RMin
    AllData <- cbind(ymd, dataM)
    # calculate daily GDD  ...what? There doesn't appear to be any GDD work?
    colnames(AllData) <- c("year", "month", "day", "precip", "tmax", "tmin",
                           "windspeed", "SPH", "SRAD", "Rmax", "Rmin")
    close(fileCon)
    return(AllData)
}

read_binary_addmdy_4var <- function(filename, ymd) {
    Nofvariables <- 4 # number of variables or column in the forcing data file
    Nrecords <- nrow(ymd)
    ind <- seq(1, Nrecords * Nofvariables, Nofvariables)
    fileCon <-  file(filename, "rb")
    temp <- readBin(fileCon, integer(), size = 2, n = Nrecords * Nofvariables,
                    endian="little")
    dataM <- matrix(0, Nrecords, 4)
    k <- 1
    dataM[1:Nrecords, 1] <- temp[ind] / 40.00       # precip data
    dataM[1:Nrecords, 2] <- temp[ind + 1] / 100.00  # Max temperature data
    dataM[1:Nrecords, 3] <- temp[ind + 2] / 100.00  # Min temperature data
    dataM[1:Nrecords, 4] <- temp[ind + 3] / 100.00  # Wind speed data

    AllData <- cbind(ymd, dataM)
    # calculate daily GDD  ...what? There doesn't appear to be any GDD work?
    colnames(AllData) <- c("year", "month", "day", "precip", "tmax", "tmin",
                           "windspeed")
    close(fileCon)
    return(AllData)
}

create_ymdvalues <- function(data_start_year, data_end_year){
    Years <- seq(data_start_year, data_end_year)
    nYears <- length(Years)
    daycount_in_year <- 0
    moncount_in_year <- 0
    yearrep_in_year <- 0

    for (i in 1:nYears){
        ly <- leap_year(Years[i])
        if (ly == TRUE){
            days_in_mon <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
        }
      else{
        days_in_mon <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
      }

      for (j in 1:12){
        daycount_in_year <- c(daycount_in_year, seq(1, days_in_mon[j]))
        moncount_in_year <- c(moncount_in_year, rep(j, days_in_mon[j]))
        yearrep_in_year <- c(yearrep_in_year, rep(Years[i], days_in_mon[j]))
      }
    }

    daycount_in_year <- daycount_in_year[-1] #delete the leading 0
    moncount_in_year <- moncount_in_year[-1]
    yearrep_in_year <- yearrep_in_year[-1]
    ymd <- cbind(yearrep_in_year, moncount_in_year, daycount_in_year)
    colnames(ymd) <- c("year", "month", "day")
    return(ymd)
}