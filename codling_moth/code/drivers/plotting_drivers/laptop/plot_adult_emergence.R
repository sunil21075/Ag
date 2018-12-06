rm(list=ls())
library(chron)
library(data.table)
library(reshape2)
library(dplyr)
library(foreach)
library(iterators)
library(ggplot2)
################################################################################################

input_dir = "/Users/hn/Desktop/Kirti/check_point/my_aeolus_2015/all_local/"
plot_path = "/Users/hn/Desktop/Kirti/check_point/my_aeolus_2015/all_local/"
color_ord = c("grey70", "dodgerblue", "olivedrab4", "red")
box_width = 0.25 

file_name = "combined_CM_rcp45.rds"
plot_output_name = "adult_emergence_rcp45"
plot_adult_emergence(input_dir=input_dir, file_name, box_width=.12, plot_path, plot_output_name)


file_name = "combined_CM_rcp85.rds"
plot_output_name = "adult_emergence_rcp85"
plot_adult_emergence(input_dir=input_dir, file_name, box_width=.12, plot_path, plot_output_name)
