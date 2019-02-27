# Script intended to take meteorological data from 
# ../jennylabcommon2/metdata/historical/UI_historical/VIC_Binary_CONUS_to_2016
# and run chill model on it. Note that as of 2018-11-19 I split this separate 
# from the script that runs the metdata/maca_v2_vic_binary data because the 
# historical data start and end years are different between modeled historical 
# (maca_) and UI_historical

# Runs with run-chill-model_historical-observed.sh

# Author: Matt Brousil with read_binary_ funs from Matt Pruett (written by
# Kirti?)

# Overview:
# 1. Functions for converting binary data
# 2. Prep file list for processing and output location
# 3. Read binary and run through chill model

# Necessary packages:
library(chillR)
library(tidyverse)
library(lubridate)

# 1. Prep binary conversion function --------------------------------------


# Define function for reading binary 8-col files

read_binary_8 <- function(file_name, file_path, hist){

  # MRB:It doesn't appear that file_name is used...?

  if (hist) {
    start_year <- 1979
    end_year <- 2015
  }

  if (!hist) {
    start_year <- 2006
    end_year <- 2099
  }

  ## create year month day values ##
  create_ymdvalues <- function(data_start_year, data_end_year) {
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

  ## read binary data and add dates ##
  readbinarydata_addmdy <- function(filename, ymd) {
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

  # create date ranges #
  ymd_file <- create_ymdvalues(start_year, end_year)

  # read data #

  readbinarydata_addmdy(file_path, ymd_file)

}


# 2. Pre-processing prep --------------------------------------------------


# 2a. Only use files in geographic locations we're interested in
focal_files <- read.delim(file = "/home/mbrousil/files/listoffilesforMatt.txt",
                          header = F)
focal_files <- as.vector(focal_files$V1)


# 2b. Note if working with a directory of historical data

hist <- TRUE


# 2c. If needed, make fastscratch folder matching the name of the 
#     current directory

# Define main output path
main_out <- file.path("/fastscratch",
                      "mbrousil",
                      "historical",
                      "UI_historical",
                      "VIC_Binary_CONUS_to_2016")


if (dir.exists(main_out) == F) {

  dir.create(path = main_out, recursive = T)

}


# 2d. Prep list of files for processing

# get files in current folder
dir_con <- dir()

# remove filenames that aren't data
dir_con <- dir_con[grep(pattern = "data_",
                        x = dir_con)]

# choose only files that we're interested in
dir_con <- dir_con[which(dir_con %in% focal_files)]

print(dir_con)



# 3. Process the data -----------------------------------------------------


# Time the processing of this batch of files
start_time <- Sys.time()

for(file in dir_con){


  # 3a. read in binary meteorological data file from specified path
  met_data <- read_binary_8(file_path = file,
                            hist = hist)

  # I make the assumption that lat always has same number of decimal points
  lat <- as.numeric(substr(x = file, start = 6, stop = 13))

  # data frame required
  met_data <- as.data.frame(met_data)


  # 3b. Clean it up

  # rename needed columns
  met_data <- met_data %>%
    rename(Year = year,
           Month = month,
           Day = day,
           Tmax = tmax,
           Tmin = tmin) %>%
    select(-c(precip, windspeed, SPH, SRAD, Rmax, Rmin)) %>%
    data.frame()


  # 3c. Get hourly interpolation

  # generate hourly data
  met_hourly <- stack_hourly_temps(weather = met_data,
                                   latitude = lat)

  # save only the necessary list item
  met_hourly <- met_hourly[[1]]


  # 3d. Run the chill accumulation model and sum up by day
  
  # we want this on a seasonal basis specific to chill
  met_hourly <- met_hourly %>%
    mutate(Chill_season = case_when(
      # If Jan:Aug then part of chill season of prev year - current year
      Month %in% c(1:8) ~ paste0("chill_", (Year - 1), "-", Year),
      # If Sept:Dec then part of chill season of current year - next year
      Month %in% c(9:12) ~ paste0("chill_", Year, "-", (Year + 1))
    ))
  
  # sum within a day using NON-cumulative chill portions
  met_daily <- met_hourly %>%
    group_by(Chill_season) %>% # should maintain correct day, time order
    mutate(chill = Dynamic_Model(HourTemp = Temp, summ = F)) %>%
    group_by(Chill_season, Year, Month, Day) %>%
    summarise(Daily_portions = sum(chill))
  
  met_daily <- met_daily %>%
    group_by(Chill_season) %>%
    mutate(Cume_portions = cumsum(Daily_portions))
  

  # 3e. Save output
  write.table(x = met_daily,
              file = file.path(main_out,
                               paste0("chill_output_",
                                      file,
                                      ".txt")),
              row.names = F)


  # Remove objects not needed in future iterations
  rm(met_data, met_hourly, met_daily)


}

# How long did it take?
end_time <- Sys.time()

print( end_time - start_time)

