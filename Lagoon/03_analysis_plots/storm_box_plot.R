rm(list=ls())
library(lubridate)
library(ggpubr)
library(purrr)
library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)

source_path_1 = "/Users/hn/Documents/GitHub/Kirti/Lagoon/core_lagoon.R"
source_path_2 = "/Users/hn/Documents/GitHub/Kirti/Lagoon/core_plot_lagoon.R"
source(source_path_1)
source(source_path_2)

options(digit=9)
options(digits=9)

in_dir <- "/Users/hn/Desktop/Desktop/Kirti/check_point/lagoon/storm/"
plot_dir <- paste0(in_dir, "plots/")
           
all_storms <- readRDS(paste0(in_dir, "all_storms.rds"))

# A <- all_storms
# all_storms <- within(all_storms, remove(cluster)) %>% 
#               data.table()

all_storms <- all_storms %>%
              filter(return_period != "1950-2005")%>%
              data.table()
              
box_p <- storm_box_plot(all_storms)

ggsave(filename = paste0("storm_box.png"), 
       plot = box_p, 
       width = 8, height = 4, units = "in", 
       dpi=600, device = "png",
       path=plot_dir)


