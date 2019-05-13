
library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggpubr)

options(digits=9)
options(digit=9)

##########################################################################################
boxplot_frost_dayofyear <- function(dt, kth_day){

  box_width <- 0.25
  color_ord = c("grey47", "dodgerblue", "olivedrab4", "khaki2", "black", "red")
  categ_lab = c("1950-2005 (observed)", "1979-2016 (historical)", 
                "2006-2025", "2026-2050", "2051-2075", "2076-2095")

  if (kth_day==1){
    title_ <- "First frost day"
   } else if (kth_day==5){
    title_ <- "Fifth frost day"
  }

  the_theme <- theme(plot.title = element_text(size=13, face="bold"),
                     panel.grid.minor = element_blank(),
                     panel.spacing=unit(.5, "cm"),
                     legend.margin=margin(t=.2, r = 0, b = .1, l = 0, unit = 'cm'),
                     legend.title = element_blank(),
                     legend.position="bottom", 
                     legend.key.size = unit(1.5, "line"),
                     legend.spacing.x = unit(.05, 'cm'),
                     panel.grid.major = element_line(size = 0.1),
                     axis.ticks = element_line(color="black", size = .2),
                     strip.text = element_text(size=12, face = "bold"),
                     legend.text=element_text(size=12),
                     axis.title.x = element_text(size= 12, face = "bold",  margin = margin(t=8, r=0, b=0, l=0)),    
                     axis.text.x = element_blank(), # element_text(size= 12, face = "plain", color="black", angle=-30),
                     axis.text.y = element_text(size=12, color="black"),
                     axis.title.y = element_blank(),
                     axis.ticks.y = element_blank())

  safe_b <- ggplot(data = dt, aes(x=time_period, y= dayofyear, fill=time_period)) +
            geom_boxplot(outlier.size=-.25, notch=F, width=box_width, lwd=.3) +            
            facet_grid(~ emission) +
            labs(x="", y="day of year") +
            the_theme + 
            scale_fill_manual(values = color_ord,
                      name = "Time\nPeriod", 
                      labels = categ_lab) + 
            scale_color_manual(values = color_ord,
                       name = "Time\nPeriod", 
                       limits = color_ord,
                       labels = categ_lab) + 
            scale_x_discrete(breaks = c("1979-2016", "1950-2005", "2006-2025", 
            	                        "2026-2050", "2051-2075", "2076-2095"),
                             labels = categ_lab)  +
            ggtitle(lab=title_)

  return(safe_b)
}

boxplot_frost_median_dayofyear <- function(dt, kth_day){

  box_width <- 0.25
  color_ord = c("grey47", "dodgerblue", "olivedrab4", "khaki2", "black", "red")
  categ_lab = c("1950-2005 (observed)", "1979-2016 (historical)", 
                "2006-2025", "2026-2050", "2051-2075", "2076-2095")

  if (kth_day==1){
    title_ <- "First frost day"
   } else if (kth_day==5){
    title_ <- "Fifth frost day"
  }

  the_theme <- theme(plot.title = element_text(size=13, face="bold"),
                     panel.grid.minor = element_blank(),
                     panel.spacing=unit(.5, "cm"),
                     legend.margin=margin(t=.2, r = 0, b = .1, l = 0, unit = 'cm'),
                     legend.title = element_blank(),
                     legend.position="bottom", 
                     legend.key.size = unit(1.5, "line"),
                     legend.spacing.x = unit(.05, 'cm'),
                     panel.grid.major = element_line(size = 0.1),
                     axis.ticks = element_line(color="black", size = .2),
                     strip.text = element_text(size=12, face = "bold"),
                     legend.text=element_text(size=12),
                     axis.title.x = element_text(size= 12, face = "bold",  margin = margin(t=8, r=0, b=0, l=0)),    
                     axis.text.x = element_blank(), # element_text(size= 12, face = "plain", color="black", angle=-30),
                     axis.text.y = element_text(size=12, color="black"),
                     axis.title.y = element_blank(),
                     axis.ticks.y = element_blank())

  safe_b <- ggplot(data = dt, aes(x=time_period, y= median, fill=time_period)) +
            geom_boxplot(outlier.size=-.25, notch=F, width=box_width, lwd=.3) +            
            facet_grid(~ emission) +
            labs(x="", y="day of year (median)") +
            the_theme + 
            scale_fill_manual(values = color_ord,
                      name = "Time\nPeriod", 
                      labels = categ_lab) + 
            scale_color_manual(values = color_ord,
                       name = "Time\nPeriod", 
                       limits = color_ord,
                       labels = categ_lab) + 
            scale_x_discrete(breaks = c("1979-2016", "1950-2005", "2006-2025", 
            	                        "2026-2050", "2051-2075", "2076-2095"),
                             labels = categ_lab)  +
            ggtitle(lab=title_)

  return(safe_b)
}


TS_frost_dayofyear <- function(data){
  title_ <- "First frost day of year"
  y_lab <- "day of year"
  
  acc_plot <- ggplot(data = data) +
              geom_point(aes(x = year, y = dayofyear, fill = model),
                         alpha = 0.25, shape = 21) +
              # geom_smooth(aes(x = year, y = dayofyear, color = model),
              #             method = "lm", size=1.2, se = F) +
              facet_grid( ~ emission) +
              scale_color_viridis_d(option = "plasma", begin = 0, end = .7,
                                    name = "Model", 
                                    aesthetics = c("color", "fill")) +
              ylab(y_lab) + xlab("year") +
              ggtitle(label = title_) +
              theme(plot.title = element_text(size = 14, face="bold", color="black"),
                    plot.subtitle = element_text(size = 12, face="plain", color="black"),
                    axis.text.x = element_text(size = 10, face = "bold", color="black"),
                    axis.text.y = element_text(size = 10, face = "bold", color="black"),
                    axis.title.x = element_text(size = 12, face = "bold", color="black", 
                                                margin = margin(t=8, r=0, b=0, l=0)),
                    axis.title.y = element_text(size = 12, face = "bold", color="black",
                                                margin = margin(t=0, r=8, b=0, l=0)),
                    strip.text = element_text(size=14, face = "bold"),
                    legend.margin=margin(t=.1, r=0, b=0, l=0, unit='cm'),
                    legend.title = element_blank(),
                    legend.position="bottom", 
                    legend.key.size = unit(1.5, "line"),
                    legend.spacing.x = unit(.05, 'cm'),
                    legend.text=element_text(size=12),
                    panel.spacing.x =unit(.75, "cm")
                    )
  return(acc_plot)
}

TS_frost_median_dayofyear <- function(data){
  title_ <- "First frost day of year"
  y_lab <- "day of year (median)"
  
  acc_plot <- ggplot(data = data) +
              geom_point(aes(x = year, y = median, fill = model),
                         alpha = 0.25, shape = 21) +
              geom_smooth(aes(x = year, y = y, color = model),
                          method = "lm", size=1.2, se = F) +
              scale_color_viridis_d(option = "plasma", begin = 0, end = .7,
                                    name = "Model", 
                                    aesthetics = c("color", "fill")) +              
              ylab(y_lab) + xlab("year") +
              ggtitle(label = title_) +
              theme(plot.title = element_text(size = 14, face="bold", color="black"),
                    plot.subtitle = element_text(size = 12, face="plain", color="black"),
                    axis.text.x = element_text(size = 10, face = "bold", color="black"),
                    axis.text.y = element_text(size = 10, face = "bold", color="black"),
                    axis.title.x = element_text(size = 12, face = "bold", color="black", 
                                                margin = margin(t=8, r=0, b=0, l=0)),
                    axis.title.y = element_text(size = 12, face = "bold", color="black",
                                                margin = margin(t=0, r=8, b=0, l=0)),
                    strip.text = element_text(size=14, face = "bold"),
                    legend.margin=margin(t=.1, r=0, b=0, l=0, unit='cm'),
                    legend.title = element_blank(),
                    legend.position="bottom", 
                    legend.key.size = unit(1.5, "line"),
                    legend.spacing.x = unit(.05, 'cm'),
                    legend.text=element_text(size=12),
                    panel.spacing.x =unit(.75, "cm")
                    )
  return(acc_plot)
}
##########################################################################################
           
in_dir <- "/Users/hn/Desktop/Desktop/Kirti/check_point/chilling/frost_bloom/"
emission_types <- c("rcp45", "rcp85")
name_pref <- "dt_for_frost_"
file_ext <- ".rds"
time_periods <- c("1979-2016", "1950-2005", "2006-2025", "2026-2050", "2051-2075", "2076-2095")

####################
#################### 5th
####################

fifth_frost_45 <- data.table(readRDS(paste0(in_dir, "fifth_frost_45.rds")))
fifth_frost_85 <- data.table(readRDS(paste0(in_dir, "fifth_frost_85.rds")))
fifth_frost <- rbind(fifth_frost_45, fifth_frost_85)
rm(fifth_frost_45, fifth_frost_85)

fifth_frost$time_period <- factor(fifth_frost$time_period, order=T, levels=time_periods)
fifth_frost_box <- boxplot_frost_dayofyear(fifth_frost, kth_day=5)

ggsave(filename = "fifth_frost_box.png",
       path = in_dir, 
       plot = fifth_frost_box,
       width = 10, height = 10, units = "in",
       dpi = 300, 
       device = "png",
       limitsize = FALSE)

fifth_frost_TS <- TS_frost_dayofyear(fifth_frost)
ggsave(filename = "fifth_frost_TS.png",
       path = in_dir, 
       plot = fifth_frost_TS,
       width = 10, height = 10, units = "in",
       dpi = 300, 
       device = "png",
       limitsize = FALSE)

rm(fifth_frost, fifth_frost_box)


####################
#################### 1st
####################

first_frost_45 <- data.table(readRDS(paste0(in_dir, "first_frost_45.rds")))
first_frost_85 <- data.table(readRDS(paste0(in_dir, "first_frost_85.rds")))
first_frost <- rbind(first_frost_45, first_frost_85)
rm(first_frost_45, first_frost_85)

first_frost$time_period <- factor(first_frost$time_period, order=T, levels=time_periods)
first_frost_box <- boxplot_frost_dayofyear(first_frost, kth_day=1)

ggsave(filename = "first_frost_box.png",
       path = in_dir, 
       plot = first_frost_box,
       width = 10, height = 10, units = "in",
       dpi = 300, 
       device = "png",
       limitsize = FALSE)


first_frost_TS <- TS_frost_dayofyear(first_frost)
ggsave(filename = "first_frost_TS.png",
       path = in_dir, 
       plot = first_frost_TS,
       width = 10, height = 10, units = "in",
       dpi = 300, 
       device = "png",
       limitsize = FALSE)

rm(first_frost, first_frost_box)

####################
######################################## Medians
####################

####################
#################### 5th
####################

# fifth_frost_medians_45 <- data.table(readRDS(paste0(in_dir, "fifth_frost_medians_45.rds")))
# fifth_frost_medians_85 <- data.table(readRDS(paste0(in_dir, "fifth_frost_medians_85.rds")))
# fifth_frost_medians_45$emission <- "RCP 4.5"
# fifth_frost_medians_85$emission <- "RCP 8.5"
# fifth_frost_medians <- rbind(fifth_frost_medians_45, fifth_frost_medians_85)
# rm(fifth_frost_medians_45, fifth_frost_medians_85)

fifth_frost_medians <- data.table(readRDS(paste0(in_dir, "fifth_frost_medians.rds")))

fifth_frost_medians$time_period <- factor(fifth_frost_medians$time_period, order=T, levels=time_periods)
fifth_frost_medians_box <- boxplot_frost_median_dayofyear(fifth_frost_medians, kth_day=1)

ggsave(filename = "fifth_frost_medians_box.png",
       path = in_dir, 
       plot = fifth_frost_medians_box,
       width = 10, height = 10, units = "in",
       dpi = 300, 
       device = "png",
       limitsize = FALSE)

rm(fifth_frost_medians, fifth_frost_medians_box)
####################
#################### 1st
####################
first_frost_medians <- data.table(readRDS(paste0(in_dir, "first_frost_medians.rds")))

first_frost_medians$time_period <- factor(first_frost_medians$time_period, order=T, levels=time_periods)
first_frost_medians_box <- boxplot_frost_median_dayofyear(first_frost_medians, kth_day=1)

ggsave(filename = "first_frost_medians_box.png",
       path = in_dir, 
       plot = first_frost_medians_box,
       width = 10, height = 10, units = "in",
       dpi = 300, 
       device = "png",
       limitsize = FALSE)

TS_frost_median_dayofyear(first_frost_medians)




