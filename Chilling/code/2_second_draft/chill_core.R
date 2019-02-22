###################################################################
#          **********                            **********
#          **********        WARNING !!!!        **********
#          **********                            **********
##
## DO NOT load any libraries here.
## And do not load any libraries on the drivers!
## Unless you are aware of conflicts between packages.
## I spent hours to figrue out what the hell is going on!
###################################################################
# 1. Prep binary conversion functions ------------------------
# Define function for reading binary 8-col files
options(digits=9)
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
########################################
######
######    Folder 02_ stuff
######
########################################

# A function to check against a threshold
chill_thresh <- function(x, threshold) {x >= threshold}

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

threshold_func <- function(file, data_type){
  if (data_type=="modeled"){
    data <- file %>%
    # Only want complete seasons of data
    filter(Chill_season != "chill_1949-1950" &
           Chill_season != "chill_2005-2006")
  } else {
    data <- file %>%
            # Only want complete seasons of data
            filter(Chill_season != "chill_1978-1979" &
                   Chill_season != "chill_2015-2016")
  }
  
  data <- data %>% 
          # Within a season
          group_by(Chill_season) %>%
          # Mutate output is the row index of the first 
          # time where it meets threshold
          # within the group. (Index is the same as counting 
          # the start date as day = 1)
          mutate(thresh_20 = detect_index(.x = Cume_portions,
                                          .f = chill_thresh,
                                          threshold = 20),
                 thresh_25 = detect_index(.x = Cume_portions,
                                          .f = chill_thresh,
                                          threshold = 25),
                 thresh_30 = detect_index(.x = Cume_portions,
                                          .f = chill_thresh,
                                          threshold = 30),
                 thresh_35 = detect_index(.x = Cume_portions,
                                          .f = chill_thresh,
                                          threshold = 35),
                 thresh_40 = detect_index(.x = Cume_portions,
                                          .f = chill_thresh,
                                          threshold = 40),
                 thresh_45 = detect_index(.x = Cume_portions,
                                          .f = chill_thresh,
                                          threshold = 45),
                 thresh_50 = detect_index(.x = Cume_portions,
                                          .f = chill_thresh,
                                          threshold = 50),
                 thresh_55 = detect_index(.x = Cume_portions,
                                          .f = chill_thresh,
                                          threshold = 55),
                 thresh_60 = detect_index(.x = Cume_portions,
                                          .f = chill_thresh,
                                          threshold = 60),
                 thresh_65 = detect_index(.x = Cume_portions,
                                          .f = chill_thresh,
                                          threshold = 65),
                 thresh_70 = detect_index(.x = Cume_portions,
                                          .f = chill_thresh,
                                          threshold = 70),
                 thresh_75 = detect_index(.x = Cume_portions,
                                          .f = chill_thresh,
                                          threshold = 75),
                 # Below we set the row where, ex. month = 1 (jan) and day = 1 to be
                 # equal to the value of Cume_portions. This creates one non-NA row
                 # for the whole column. Then in summarise we remove all NAs, which
                 # collapses the column to a single value -- perfect for a one-line
                 # summary per season.
                 sum_J1 = case_when(Month == 1 & Day == 1 ~ Cume_portions),
                 sum_F1 = case_when(Month == 2 & Day == 1 ~ Cume_portions),
                 sum_M1 = case_when(Month == 3 & Day == 1 ~ Cume_portions),
                 sum_A1 = case_when(Month == 4 & Day == 1 ~ Cume_portions)) %>% 
          summarise(sum = sum(Daily_portions),
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

##################################################
#####                                        #####
#####    this is the overlapping one         #####
#####    the non-overlap is below            #####
#####                                        #####
##################################################
process_data <- function(file, time_period) {
  if (time_period=="2040"){
    processed_data <- file %>%
                      filter(Year > 2025 & Year <= 2055,
                             Chill_season != "chill_2025-2026" &
                             Chill_season != "chill_2055-2056")
  } else if (time_period=="2060"){
    processed_data <- file %>%
                      filter(Year > 2045 & Year <= 2075,
                             Chill_season != "chill_2045-2046" &
                             Chill_season != "chill_2075-2076")
  } else if (time_period=="2080") {
     processed_data <- file %>%
                       filter(Year > 2065 & Year <= 2095,
                              Chill_season != "chill_2065-2066" &
                              Chill_season != "chill_2095-2096")
  }
  processed_data <- processed_data %>% 
                    group_by(Chill_season) %>%
                    mutate(thresh_20 = detect_index(.x = Cume_portions,
                                                    .f = chill_thresh,
                                                    threshold = 20),
                           thresh_25 = detect_index(.x = Cume_portions,
                                                    .f = chill_thresh,
                                                    threshold = 25),
                           thresh_30 = detect_index(.x = Cume_portions,
                                                    .f = chill_thresh,
                                                    threshold = 30),
                           thresh_35 = detect_index(.x = Cume_portions,
                                                    .f = chill_thresh,
                                                    threshold = 35),
                           thresh_40 = detect_index(.x = Cume_portions,
                                                    .f = chill_thresh,
                                                    threshold = 40),
                           thresh_45 = detect_index(.x = Cume_portions,
                                                    .f = chill_thresh,
                                                    threshold = 45),
                           thresh_50 = detect_index(.x = Cume_portions,
                                                    .f = chill_thresh,
                                                    threshold = 50),
                           thresh_55 = detect_index(.x = Cume_portions,
                                                    .f = chill_thresh,
                                                    threshold = 55),
                           thresh_60 = detect_index(.x = Cume_portions,
                                                    .f = chill_thresh,
                                                    threshold = 60),
                           thresh_65 = detect_index(.x = Cume_portions,
                                                    .f = chill_thresh,
                                                    threshold = 65),
                           thresh_70 = detect_index(.x = Cume_portions,
                                                    .f = chill_thresh,
                                                    threshold = 70),
                           thresh_75 = detect_index(.x = Cume_portions,
                                                    .f = chill_thresh,
                                                    threshold = 75),
                           sum_J1 = case_when(Month == 1 & Day == 1 ~ Cume_portions),
                           sum_F1 = case_when(Month == 2 & Day == 1 ~ Cume_portions),
                           sum_M1 = case_when(Month == 3 & Day == 1 ~ Cume_portions),
                           sum_A1 = case_when(Month == 4 & Day == 1 ~ Cume_portions)) %>% 
                    summarise(sum = sum(Daily_portions),
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
  return(processed_data)
}


process_data_non_overlap <- function(file, time_period) {
  if (time_period == "2025_2050"){
    processed_data <- file %>%
                      filter(Year > 2025 & Year <= 2050,
                             Chill_season != "chill_2025-2026" &
                             Chill_season != "chill_2050-2051")
  } else if (time_period == "2051_2075"){
    processed_data <- file %>%
                      filter(Year > 2050 & Year <= 2075,
                             Chill_season != "chill_2050-2051" &
                             Chill_season != "chill_2075-2076")
  } else if (time_period == "2076_2100") {
     processed_data <- file %>%
                       filter(Year > 2075 & Year <= 2099,
                              Chill_season != "chill_2075-2076" &
                              Chill_season != "chill_2099-2100")
  }
  processed_data <- processed_data %>% 
                    group_by(Chill_season) %>%
                    mutate(thresh_20 = detect_index(.x = Cume_portions,
                                                    .f = chill_thresh,
                                                    threshold = 20),
                           thresh_25 = detect_index(.x = Cume_portions,
                                                    .f = chill_thresh,
                                                    threshold = 25),
                           thresh_30 = detect_index(.x = Cume_portions,
                                                    .f = chill_thresh,
                                                    threshold = 30),
                           thresh_35 = detect_index(.x = Cume_portions,
                                                    .f = chill_thresh,
                                                    threshold = 35),
                           thresh_40 = detect_index(.x = Cume_portions,
                                                    .f = chill_thresh,
                                                    threshold = 40),
                           thresh_45 = detect_index(.x = Cume_portions,
                                                    .f = chill_thresh,
                                                    threshold = 45),
                           thresh_50 = detect_index(.x = Cume_portions,
                                                    .f = chill_thresh,
                                                    threshold = 50),
                           thresh_55 = detect_index(.x = Cume_portions,
                                                    .f = chill_thresh,
                                                    threshold = 55),
                           thresh_60 = detect_index(.x = Cume_portions,
                                                    .f = chill_thresh,
                                                    threshold = 60),
                           thresh_65 = detect_index(.x = Cume_portions,
                                                    .f = chill_thresh,
                                                    threshold = 65),
                           thresh_70 = detect_index(.x = Cume_portions,
                                                    .f = chill_thresh,
                                                    threshold = 70),
                           thresh_75 = detect_index(.x = Cume_portions,
                                                    .f = chill_thresh,
                                                    threshold = 75),
                           sum_J1 =  case_when(Month == 1 & Day == 1 ~ Cume_portions),
                           sum_F1 = case_when(Month == 2 & Day == 1 ~ Cume_portions),
                           sum_M1 = case_when(Month == 3 & Day == 1 ~ Cume_portions),
                           sum_A1 = case_when(Month == 4 & Day == 1 ~ Cume_portions)) %>% 
                    summarise(sum = sum(Daily_portions),
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
  return(processed_data)
}