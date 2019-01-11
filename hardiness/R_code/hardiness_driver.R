rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)

param_dir = "/Users/hn/Documents/GitHub/Kirti/hardiness/R_code/parameters/"
input_data_dir = "/Users/hn/Documents/GitHub/Kirti/hardiness/R_code/input_data/"

# read parameters
input_params  = data.table(read.csv(paste0(param_dir, "input_parameters", ".csv")))
variety_params= data.table(read.csv(paste0(param_dir,"variety_parameters", ".csv")))

# read input temps
input_temps = data.table(read.csv(paste0(input_data_dir, "input_temps", ".csv")))
Prosser_temps= data.table(read.csv(paste0(input_data_dir, "Prosser_temps", ".csv")))