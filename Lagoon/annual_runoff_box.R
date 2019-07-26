rm(list=ls())
library(lubridate)
library(ggpubr)
library(purrr)
library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)
options(digit=9)
options(digits=9)

##################################################################################
#
#    Core paths
#
source_path_1 = "/Users/hn/Documents/GitHub/Kirti/Lagoon/core_lagoon.R"
source_path_2 = "/Users/hn/Documents/GitHub/Kirti/Lagoon/core_plot_lagoon.R"
source(source_path_1)
source(source_path_2)
#
##################################################################################
#
#    file directories
#
in_dir <- "/Users/hn/Desktop/Desktop/Kirti/check_point/lagoon/runoff/"
param_dir <- "/Users/hn/Documents/GitHub/Kirti/Lagoon/parameters/"
plot_dir <- paste0(in_dir, "plots/")
#
##################################################################################
#
#    file readings
#
file_N <- "all_ann_cum_runoff_LD.rds"

ann_cum_runoff <- data.table(readRDS(paste0(in_dir, file_N)))
head(ann_cum_runoff, 2)


dt <- ann_cum_runoff
#########
######### Clean up the data
#########
dt <- dt %>% 
      filter(time_period != "1950-2005" & time_period != "2006-2025") %>% 
      data.table()

needed_cols <- c("location", "year", "time_period", 
                 "model", "emission", "cluster", 
                 "annual_cum_runbase")

dt <- subset(dt, select = needed_cols)

#########
######### Make proper columns
#########
time_label <- c("1979-2016", "2026-2050", "2051-2075", "2076-2099")
categ_label <- c("most precip", "less precip", 
                 "lesser precip", "least precip")

dt$time_period <- factor(dt$time_period, 
                         levels=time_label)

dt$cluster <- factor(dt$cluster, 
                     levels=categ_label)

melted <- melt(dt, id = c("location", "year", 
                          "time_period", "model", "emission",
                          "cluster"))
head(melted, 2)

#########
#########  PLOT properties and cosmetic stuff
#########
#
# column to be plotted
#

plot_col <- "annual_cum_runbase"
y_lab <- " runoff + base flow"

color_ord = c("grey47", "dodgerblue2", "olivedrab4", "red")
color_ord = c("grey47", "dodgerblue2", "olivedrab4", "gold")
ax_txt_size <- 6; ax_ttl_size <- 7; box_width = 0.53
the <- theme(plot.margin = unit(c(t=.1, r=.2, b=.1, l=0.2), "cm"),
               panel.border = element_rect(fill=NA, size=.3),
               panel.grid.major = element_line(size = 0.05),
               panel.grid.minor = element_blank(),
               panel.spacing = unit(.35, "line"),
               legend.position = "bottom", 
               legend.key.size = unit(.6, "line"),
               legend.spacing.x = unit(.1, 'line'),
               panel.spacing.y = unit(.5, 'line'),
               legend.text = element_text(size = ax_ttl_size, face="bold"),
               legend.margin = margin(t=.1, r=0, b=0, l=0, unit = 'line'),
               legend.title = element_blank(),
               plot.title = element_text(size = ax_ttl_size, face = "bold"),
               plot.subtitle = element_text(face = "bold"),
               strip.text.x = element_text(size = ax_ttl_size, face = "bold",
                                           margin = margin(.15, 0, .15, 0, "line")),
               axis.ticks = element_line(size = .1, color = "black"),
               axis.text.y = element_text(size = ax_txt_size, 
                                          face = "bold", color = "black"),
               axis.text.x = element_text(size = ax_txt_size, 
                                          face = "bold", color="black",
                                          margin=margin(t=.05, r=5, l=5, b=0,"pt")
                                          ),
               axis.title.y = element_text(size = ax_ttl_size, 
                                           face = "bold", 
                                           margin = margin(t=0, r=2, b=0, l=0)),
               axis.title.x = element_blank()
                    )

ann_box <- ggplot(data = melted, 
                  aes(x=cluster, y=value, fill=time_period)) +
           the + 
           geom_boxplot(outlier.size = - 0.3, notch=F, 
                        width = box_width, lwd=.1, 
                        position = position_dodge(0.6)) +
           # labs(x="", y="") + # theme_bw() + 
           facet_grid(~ emission) +
           ylab(y_lab) +   
           scale_fill_manual(values = color_ord,
                             labels = time_label)




