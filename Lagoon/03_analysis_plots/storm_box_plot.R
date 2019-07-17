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

all_storms <- readRDS(paste0(in_dir, "all_modeled_storms.rds"))
all_storms <- within(all_storms, remove(cluster)) %>% data.table()


box_p <- storm_box_plot(all_storms)
ggsave(filename = paste0("box_p.png"), 
       plot = box_p, 
       width = 4, height = 2, units = "in", 
       dpi=600, device = "png",
       path="/Users/hn/Desktop/Desktop/Kirti/check_point/lagoon/")


