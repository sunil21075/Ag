rm(list=ls())
library(ggmap)
library(ggpubr)
library(plyr)
library(lubridate)
library(purrr)
library(scales)
library(tidyverse)
library(data.table)
options(digits=9)
options(digit=9)
################################################################################
###########                                                          ###########
###########                                                          ###########
###########                                                          ###########
################################################################################

in_dir <- "/Users/hn/Documents/GitHub/Kirti/Growing_tree_lat_long_CSV_files/"
plot_dir <- "/Users/hn/Desktop/"

#####
##### The locations of all farms in the US given by Kiriti's email xlsx file
#####
all_west <- read.table(paste0(in_dir, "lat_long_Grid.csv"), header=TRUE, sep=",")
grids <- read.table(paste0(in_dir, "TreeFruitGrids.csv"), header=TRUE, sep=",")
setnames(all_west, old=c("vicidgeo"), new=c("vicid"))

west_1_percent <- merge(all_west, grids) %>% filter(SumTreeFruit>= 0.01)
west_1_percent <- within(west_1_percent, remove(vicid, SumTreeFruit))

################################################################################
####################
####################             Local Locations the 295 locations
####################
################################################################################

param_dir <- "/Users/hn/Documents/GitHub/Kirti/codling_moth/code/parameters/"
local_295 <- read.table(paste0(param_dir, "LocationGroups.csv"), header=TRUE, sep=",")
local_295 <- within(local_295, remove(locationGroup))
setnames(local_295, old=c("latitude", "longitude"), new=c("lat", "long"))

################################################################################
###########                                                          ###########
###########                                                          ###########
###########                                                          ###########
################################################################################

file_list <- paste0("data_", west_1_percent$lat, "_", west_1_percent$long)

# 5 files are in the 295 files that are not in the all west
# extract them here
D2 <- paste0("data_", local_295$lat, "_", local_295$long)
D2 = subset(D2, !(D2 %in% file_list))

all_west_locs <- c(D2, file_list)
main_out <- "/Users/hn/Documents/GitHub/Kirti/Chilling/parameters/"
write.table(x = all_west_locs,
            file = file.path(main_out, "file_list.txt"),
            row.names = F, col.names = F)

test <- read.delim(file = paste0(main_out, "file_list.txt"), header = F)
test <- as.vector(test$V1)

################################################################################
####################                 USA Locations
################################################################################
all_us_locations <- read.delim(file = paste0(param_dir, "all_us_locations_list.txt"), header = F)
all_us_locations <- as.vector(all_us_locations$V1)
x <- sapply(all_us_locations, function(x) strsplit(x, "_")[[1]], USE.NAMES=FALSE)
us_lats = x[1, ]
us_long = x[2, ]
all_us_locations <- data.table(us_lats, us_long)
all_us_locations$lat = as.numeric(all_us_locations$lat)
all_us_locations$long = as.numeric(all_us_locations$long)

#############################################################################

#                   Map PLOTS

#############################################################################
states <- map_data("state")

all_us_map <- all_us_locations %>%  
              ggplot() +
              geom_polygon(data = states, 
                           aes(x=long, y=lat, group = group),
                               fill = "grey", color = "red", size=0.1) +
              geom_point(aes_string(x = "long", y = "lat"), alpha = 0.8, size=0.1)

ggsave(filename = "all_us_map.png", plot = all_us_map, device = "png",
       width = 20, height = 12, units = "in", dpi=400, path=plot_dir)

states <- map_data("state")
states_cluster <- subset(states, region %in% c("oregon", "washington", "idaho"))

local_295_map <- local_locs %>%  
                 ggplot() +
                 geom_polygon(data = states_cluster, 
                              aes(x=long, y=lat, group = group),
                                  fill = "grey", color = "red", size=0.1) +
                 geom_point(aes_string(x = "long", y = "lat"), alpha = 0.8, size=0.1)

ggsave(filename = "local_295_map.png", plot = local_295_map, device = "png",
       width = 5, height = 3, units = "in", dpi=400, path=plot_dir)

# merged_locs_0_percent <- merge(all_west, grids) %>% filter(SumTreeFruit > 0)
merged_locs_1_percent <- merge(all_west, grids) %>% filter(SumTreeFruit>= 0.01)
merged_locs_2_percent <- merge(all_west, grids) %>% filter(SumTreeFruit>= 0.02)

states <- map_data("state")
states_cluster <- subset(states, region %in% c("oregon", "washington", "idaho"))

loc_0_percent_map <- merged_locs_0_percent %>%  
                 ggplot() +
                 geom_polygon(data = states_cluster, 
                              aes(x=long, y=lat, group = group),
                                  fill = "grey", color = "red", size=0.1) +
                 geom_point(aes_string(x = "long", y = "lat"), alpha = 0.8, size=0.1)

ggsave(filename = "loc_0_percent.png", plot = loc_0_percent_map, device = "png",
       width = 5, height = 3, units = "in", dpi=400, path=plot_dir)


loc_1_percent_map <- merged_locs_1_percent %>%  
                 ggplot() +
                 geom_polygon(data = states_cluster, 
                              aes(x=long, y=lat, group = group),
                                  fill = "grey", color = "red", size=0.1) +
                 geom_point(aes_string(x = "long", y = "lat"), alpha = 0.8, size=0.1)

ggsave(filename = "loc_1_percent.png", plot = loc_1_percent_map, device = "png",
       width = 5, height = 3, units = "in", dpi=400, path=plot_dir)


loc_2_percent_map <- merged_locs_2_percent %>%  
                 ggplot() +
                 geom_polygon(data = states_cluster, 
                              aes(x=long, y=lat, group = group),
                                  fill = "grey", color = "red", size=0.1) +
                 geom_point(aes_string(x = "long", y = "lat"), alpha = 0.8, size=0.1)

ggsave(filename = "loc_2_percent.png", plot = loc_2_percent_map, device = "png",
       width = 5, height = 3, units = "in", dpi=400, path=plot_dir)


merged_locs_3_percent_map <- merge(all_west, grids) %>% filter(SumTreeFruit>= 0.03)
loc_3_percent <- merged_locs_3_percent %>%  
                 ggplot() +
                 geom_polygon(data = states_cluster, 
                              aes(x=long, y=lat, group = group),
                                  fill = "grey", color = "red", size=0.1) +
                 geom_point(aes_string(x = "long", y = "lat"), alpha = 0.8, size=0.1)

ggsave(filename = "loc_3_percent.png", plot = merged_locs_3_percent_map, device = "png",
       width = 5, height = 3, units = "in", dpi=400, path=plot_dir)


