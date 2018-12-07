rm(list=ls())
library(chron)
library(data.table)
library(reshape2)
library(dplyr)
library(foreach)
library(iterators)
library(ggplot2)

########################################################################################
input_dir = "/Users/hn/Desktop/Kirti/check_point/my_aeolus_2015/all_local/"
plot_path = "/Users/hn/Desktop/Kirti/check_point/my_aeolus_2015/all_local/"
color_ord = c("grey70", "dodgerblue", "olivedrab4", "red")

stages = c("Larva", "Adult")
dead_lines = c("Aug", "Nov")
versions = c("rcp45", "rcp85")

file_pref = "generations_" 
file_mid = "_combined_CMPOP_"
file_end = ".rds"

plot_with = 6.5
plot_height = 2.5

for (dead_line in dead_lines){
    for (version in versions){
        file_name = paste0(file_pref, dead_line, file_mid)
        file_name = paste0(file_name, version, file_end)
        for (stage in stages){
            plot_No_generations(input_dir,
                                file_name,
                                stage,
                                dead_line = dead_line,
                                box_width=.45,
                                plot_with=plot_with, 
                                plot_height= plot_height,
                                plot_path,
                                version=version,
                                color_ord)
        }
    }
}