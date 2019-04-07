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

in_dir <- "/Users/hn/Documents/GitHub/Kirti/Growing_tree_lat_long_CSV_files/"
plot_dir <- "/Users/hn/Desktop/"

#####
##### The locations of all farms in the US given by Kiriti's email xlsx file
#####
all_west <- read.table(paste0(in_dir, "lat_long_Grid.csv"), header=TRUE, sep=",")
grids <- read.table(paste0(in_dir, "TreeFruitGrids.csv"), header=TRUE, sep=",")
setnames(all_west, old=c("vicidgeo"), new = c("vicid"))

west_1_percent <- merge(all_west, grids) %>% filter(SumTreeFruit >= 0.01)
west_1_percent <- within(west_1_percent, remove(vicid, SumTreeFruit))

west_0_percent <- merge(all_west, grids) %>% filter(SumTreeFruit > 0)
west_0_percent <- within(west_0_percent, remove(vicid, SumTreeFruit))

################################################################################
###########                                                          ###########
###########                                                          ###########
###########                                                          ###########
################################################################################

file_list_1_percent <- paste0("data_", west_1_percent$lat, "_", west_1_percent$long)
file_list_0_percent <- paste0("data_", west_0_percent$lat, "_", west_0_percent$long)

# there are 5 files are in the 295 files that are not in the all west
# extract them here
D2 <- paste0("data_", local_295$lat, "_", local_295$long)
D2 <- subset(D2, !(D2 %in% file_list_1_percent))
all_west_locs_1 <- c(D2, file_list_1_percent)


D2 <- paste0("data_", local_295$lat, "_", local_295$long)
D2 <- subset(D2, !(D2 %in% file_list_0_percent))
all_west_locs_0 <- c(D2, file_list_0_percent)

main_out <- "/Users/hn/Documents/GitHub/Kirti/Chilling/parameters/"
write.table(x = all_west_locs_1,
            file = file.path(main_out, "file_list_1_percent.txt"),
            row.names = F, col.names = F)

write.table(x = all_west_locs_0,
            file = file.path(main_out, "file_list_0_percent.txt"),
            row.names = F, col.names = F)

# test <- read.delim(file = paste0(main_out, "file_list_1_percent.txt"), header = F)
# test <- as.vector(test$V1)

################################################################################

####################      Form location groups, Warmer, cooler, oregon

################################################################################

y <- sapply(all_west_locs, function(x) strsplit(x, "_")[[1]], USE.NAMES=FALSE)
west_lats = y[2, ]
west_long = y[3, ]
all_west_lat_long <- data.table(west_lats, west_long)
setnames(all_west_lat_long, old=c("west_lats", "west_long"), new=c("latitude", "longitude"))

pm_dir <- "/Users/hn/Documents/GitHub/Kirti/codling_moth/code/parameters/"
WA_LocationGroups <- read.csv(paste0(pm_dir, "LocationGroups.csv"))

all_west_lat_long <- join(x=all_west_lat_long, y=WA_LocationGroups, type = "left", match = "all")
all_west_lat_long[is.na(all_west_lat_long)] <- 3
new_pm_dir <- "/Users/hn/Documents/GitHub/Kirti/Chilling/parameters/"
write.table(x = all_west_lat_long,
            file = file.path(new_pm_dir, "LocationGroups.csv"),
            row.names = F, col.names = T, sep=",")

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

local_295_map <- local_295 %>%  
                 ggplot() +
                 geom_polygon(data = states_cluster, 
                              aes(x=long, y=lat, group = group),
                                  fill = "grey", color = "red", size=0.1) +
                 geom_point(aes_string(x = "long", y = "lat"), alpha = 0.8, size=0.1)

ggsave(filename = "local_295_map.png", plot = local_295_map, device = "png",
       width = 5, height = 3, units = "in", dpi=400, path=plot_dir)

merged_locs_0_percent <- merge(all_west, grids) %>% filter(SumTreeFruit > 0)
merged_locs_1_percent <- merge(all_west, grids) %>% filter(SumTreeFruit>= 0.01)

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
                                  aes(x=long, y=lat, group = grosup),
                                      fill = "grey", color = "red", size=0.1) +
                     geom_point(aes_string(x = "long", y = "lat"), alpha = 0.8, size=0.1)

ggsave(filename = "loc_1_percent.png", plot = loc_1_percent_map, device = "png",
       width = 5, height = 3, units = "in", dpi=400, path=plot_dir)



