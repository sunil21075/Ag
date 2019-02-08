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

read_binary <- function(file_name, file_path, hist, no_vars){
	if (hist) {
		start_year <- 1950
		end_year <- 2005
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
    dataM[1:Nrecords, 1] <- temp[ind] / 40.00  #precip data
    dataM[1:Nrecords, 2] <- temp[ind + 1] / 100.00  #Max temperature data
    dataM[1:Nrecords, 3] <- temp[ind + 2] / 100.00  #Min temperature data
    dataM[1:Nrecords, 4] <- temp[ind + 3] / 100.00  #Wind speed data
    dataM[1:Nrecords, 5] <- temp[ind + 4] / 10000.00  #SPH
    dataM[1:Nrecords, 6] <- temp[ind + 5] / 40.00  #SRAD
    dataM[1:Nrecords, 7] <- temp[ind + 6] / 100.00  #Rmax
    dataM[1:Nrecords, 8] <- temp[ind + 7] / 100.00  #RMin
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
    dataM[1:Nrecords, 1] <- temp[ind] / 40.00  #precip data
    dataM[1:Nrecords, 2] <- temp[ind + 1] / 100.00  #Max temperature data
    dataM[1:Nrecords, 3] <- temp[ind + 2] / 100.00  #Min temperature data
    dataM[1:Nrecords, 4] <- temp[ind + 3] / 100.00  #Wind speed data

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
medians <- function(thresh_50, thresh_75, sum_J1, sum_F1, sum_M1, sum_A1) {
    c(median_50 = median(thresh_50),
      median_75 = median(thresh_75),
      median_J1 = median(sum_J1),
      median_F1 = median(sum_F1),
      median_M1 = median(sum_M1),
      median_A1 = median(sum_A1))
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
          mutate(thresh_50 = detect_index(.x = Cume_portions,
                                          .f = chill_thresh,
                                          threshold = 50), # threshold = 50 chill portions
                 thresh_75 = detect_index(.x = Cume_portions,
                                          .f = chill_thresh,
                                          threshold = 75),
                 # Below we set the row where, ex. month = 1 (jan) and day = 1 to be
                 # equal to the value of Cume_portions. This creates one non-NA row
                 # for the whole column. Then in summarise we remove all NAs, which
                 # collapses the column to a single value -- perfect for a one-line
                 # summary per season.
                 sum_J1 =  case_when( # January 1 chill sum
                   Month == 1 & Day == 1 ~ Cume_portions),
                 sum_F1 = case_when( # February 1 chill sum, etc
                   Month == 2 & Day == 1 ~ Cume_portions),
                 sum_M1 = case_when(
                   Month == 3 & Day == 1 ~ Cume_portions),
                 sum_A1 = case_when(
                   Month == 4 & Day == 1 ~ Cume_portions)) %>% 
          summarise(sum = sum(Daily_portions),
                    thresh_50 = unique(thresh_50), # retain the thresholds
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
               median_50 = median_50,
               median_75 = median_75,
               median_J1 = median_J1,
               median_F1 = median_F1,
               median_M1 = median_M1,
               median_A1 = median_A1)
    return (out_put)
}

get_medians <- function(a_list){
  medians_data <- ldply(.data = a_list,
                        .fun = function(x) medians(thresh_50 = x[, "thresh_50"],
                                                    thresh_75 = x[, "thresh_75"],
                                                    sum_J1 = x[, "sum_J1"],
                                                    sum_F1 = x[, "sum_F1"],
                                                    sum_M1 = x[, "sum_M1"],
                                                    sum_A1 = x[, "sum_A1"]))
  return (medians_data)
}

process_2040 <- function(file){ 
    processed_data <- file %>%
            filter(Year > 2025 & Year <= 2055,
                   Chill_season != "chill_2025-2026" &
                   Chill_season != "chill_2055-2056") %>% 
            group_by(Chill_season) %>%
            # Mutate output is the row index of the 
            # first time where it meets threshold
            # within the group. 
            # (Index is the same as counting the start date as day = 1)
            mutate(thresh_50 = detect_index(.x = Cume_portions,
                                            .f = chill_thresh,
                                            threshold = 50),
                                            thresh_75 = detect_index(.x = Cume_portions,
                                                                     .f = chill_thresh,
                                                                      threshold = 75),
           # Below we set the row where, ex. month = 1 (jan) and day = 1 to be
           # equal to the value of Cume_portions. This creates one non-NA row
           # for the whole column. Then in summarise we remove all NAs, which
           # collapses the column to a single value -- perfect for a one-line
           # summary per season.
           sum_J1 =  case_when(
             Month == 1 & Day == 1 ~ Cume_portions),
           sum_F1 = case_when(
             Month == 2 & Day == 1 ~ Cume_portions),
           sum_M1 = case_when(
             Month == 3 & Day == 1 ~ Cume_portions),
           sum_A1 = case_when(
             Month == 4 & Day == 1 ~ Cume_portions)) %>% 
    summarise(sum = sum(Daily_portions),
              thresh_50 = unique(thresh_50), # retain the thresholds
              thresh_75 = unique(thresh_75),
              sum_J1 = na.omit(sum_J1),
              sum_F1 = na.omit(sum_F1),
              sum_M1 = na.omit(sum_M1),
              sum_A1 = na.omit(sum_A1)) %>%
    data.frame() # to allow for ldply() later
    return (processed_data)
}

process_2060 <- function(file){
    processed_data <- file %>%
                      filter(Year > 2045 & Year <= 2075,
                             Chill_season != "chill_2045-2046" &
                             Chill_season != "chill_2075-2076") %>% 
                      group_by(Chill_season) %>%
                      mutate(thresh_50 = detect_index(.x = Cume_portions,
                                                      .f = chill_thresh,
                                                      threshold = 50),
                             thresh_75 = detect_index(.x = Cume_portions,
                                                      .f = chill_thresh,
                                                      threshold = 75),
                             sum_J1 =  case_when(
                               Month == 1 & Day == 1 ~ Cume_portions),
                             sum_F1 = case_when(
                               Month == 2 & Day == 1 ~ Cume_portions),
                             sum_M1 = case_when(
                               Month == 3 & Day == 1 ~ Cume_portions),
                             sum_A1 = case_when(
                               Month == 4 & Day == 1 ~ Cume_portions)) %>% 
                      summarise(sum = sum(Daily_portions),
                                thresh_50 = unique(thresh_50), # retain the thresholds
                                thresh_75 = unique(thresh_75),
                                sum_J1 = na.omit(sum_J1),
                                sum_F1 = na.omit(sum_F1),
                                sum_M1 = na.omit(sum_M1),
                                sum_A1 = na.omit(sum_A1)) %>%
                      data.frame() # to allow for ldply() later
    return (processed_data)
}

process_2080 <- function(file) {
    processed_data <- file %>%
                      filter(Year > 2065 & Year <= 2095,
                             Chill_season != "chill_2065-2066" &
                               Chill_season != "chill_2095-2096") %>% 
                      group_by(Chill_season) %>%
                      mutate(thresh_50 = detect_index(.x = Cume_portions,
                                                      .f = chill_thresh,
                                                      threshold = 50),
                             thresh_75 = detect_index(.x = Cume_portions,
                                                      .f = chill_thresh,
                                                      threshold = 75),
                             sum_J1 =  case_when(
                               Month == 1 & Day == 1 ~ Cume_portions),
                             sum_F1 = case_when(
                               Month == 2 & Day == 1 ~ Cume_portions),
                             sum_M1 = case_when(
                               Month == 3 & Day == 1 ~ Cume_portions),
                             sum_A1 = case_when(
                               Month == 4 & Day == 1 ~ Cume_portions)) %>% 
                      summarise(sum = sum(Daily_portions),
                                thresh_50 = unique(thresh_50), # retain the thresholds
                                thresh_75 = unique(thresh_75),
                                sum_J1 = na.omit(sum_J1),
                                sum_F1 = na.omit(sum_F1),
                                sum_M1 = na.omit(sum_M1),
                                sum_A1 = na.omit(sum_A1)) %>%
                      data.frame() # to allow for ldply() later
    return(processed_data)
}





