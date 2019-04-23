rm(list=ls())
library(data.table)
library(dplyr)
library(ggmap)
library(ggplot2)
options(digit=9)
options(digits=9)

source_path = "chill_plot_core.R"
source(source_path)

##########################################################################################
###                                                                                    ###
###                                    Driver                                          ###
###                                                                                    ###
##########################################################################################

time_types = c("non_overlapping") # , "overlapping"
model_types = c("dynamic_model_stats") # , "utah_model_stats"
main_in = "/Users/hn/Desktop/Desktop/Kirti/check_point/chilling"
file_name = "summary_comp.rds"

time_type = time_types[1]
model_type = model_types[1]

for (time_type in time_types){
  for (model_type in model_types){
    in_dir = file.path(main_in, time_type, model_type, file_name)
    out_dir = file.path(main_in, time_type, model_type, "/")
    
    datas = data.table(readRDS(in_dir))
    datas <- datas %>% filter(model != "observed")

    information = produce_data_4_plots(datas)

    safe_jan <- safe_box_plot(information[[1]], due="Jan.")
    safe_feb <- safe_box_plot(information[[4]], due="Feb.")
    safe_mar <- safe_box_plot(information[[7]], due="Mar.")
    
    # out_dir
    output_name = paste0(time_type, "_", unlist(strsplit(model_type, "_"))[1], "_Jan.png")
    ggsave(output_name, safe_jan, path="/Users/hn/Desktop/", width=4, height=4, unit="in", dpi=400)

    output_name = paste0(time_type, "_", unlist(strsplit(model_type, "_"))[1], "_Feb.png")
    ggsave(output_name, safe_feb, path="/Users/hn/Desktop/", width=4, height=4, unit="in", dpi=400)

    output_name = paste0(time_type, "_", unlist(strsplit(model_type, "_"))[1], "_Mar.png")
    ggsave(output_name, safe_mar, path="/Users/hn/Desktop/", width=4, height=4, unit="in", dpi=400)
    
    # means over models
    # mean_map_jan = ensemble_map(data=information[[2]], color_col="mean_over_model", due="Jan.")
    # mean_map_feb = ensemble_map(data=information[[5]], color_col="mean_over_model", due="Feb.")

    # output_name = paste0(time_type, "_", unlist(strsplit(model_type, "_"))[1], "_map_jan.png") 
    # ggsave(output_name, mean_map_jan, path=out_dir, width=7, height=4.5, unit="in", dpi=400)

    # output_name = paste0(time_type, "_", unlist(strsplit(model_type, "_"))[1], "_map_feb.png") 
    # ggsave(output_name, mean_map_feb, path=out_dir, width=7, height=4.5, unit="in", dpi=400)

    # medians over models
    median_map_jan = ensemble_map(data=information[[3]], color_col="median_over_model", due="Jan.")
    median_map_feb = ensemble_map(data=information[[6]], color_col="median_over_model", due="Feb.")
    median_map_mar = ensemble_map(data=information[[6]], color_col="median_over_model", due="Mar.")

    output_name = paste0(time_type, "_", unlist(strsplit(model_type, "_"))[1], "_map_jan.png") 
    ggsave(output_name, median_map_jan, path="/Users/hn/Desktop/", width=7, height=4.5, unit="in", dpi=400)

    output_name = paste0(time_type, "_", unlist(strsplit(model_type, "_"))[1], "_map_feb.png") 
    ggsave(output_name, median_map_feb, path="/Users/hn/Desktop/", width=7, height=4.5, unit="in", dpi=400)

    output_name = paste0(time_type, "_", unlist(strsplit(model_type, "_"))[1], "_map_mar.png") 
    ggsave(output_name, median_map_mar, path="/Users/hn/Desktop/", width=7, height=4.5, unit="in", dpi=400)   
  }
}

