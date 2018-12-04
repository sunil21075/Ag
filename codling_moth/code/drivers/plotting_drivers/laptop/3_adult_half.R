rm(list=ls())
library(chron)
library(data.table)
library(ggplot2)
library(reshape2)
library(dplyr)
library(foreach)
library(iterators)
library(cowplot)


input_dir <- "/Users/hn/Desktop/Kirti/check_point/my_aeolus_2015/all_local/"
plot_path <- "/Users/hn/Documents/GitHub/Kirti/Codling_moth_Code/big_box_plots/3_evolve/"
versions <- c("rcp45", "rcp85")
stages <- c("adult", "larva")
for (stage in stages){
  for (version in versions){
    output_name = paste0(stage, "_flight_", version, "_half.png")
    input_name = paste0("combined_CM_", version, ".rds")
    plot_flight_DoY_half(input_dir=input_dir, input_name=input_name, 
    	                 stage=stage, output_dir=plot_path, 
    	                 output_name=output_name,
    	                 plot_with=7, plot_height=4)
  }
}
