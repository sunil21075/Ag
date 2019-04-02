rm(list=ls())
library(data.table)
library(dplyr)
library(ggmap)
library(ggplot2)
library(ggpubr) # for ggarrange
options(digit=9)
options(digits=9)

source_path = "chill_plot_core.R"
source(source_path)

#################
## Plotting functions are in safe_chill_historicalRCPs.R
## or other files in : chill_plot_core.R
## 
#################

setwd("/Users/hn/Desktop/Desktop/Kirti/check_point/chilling/non_overlapping/different_chill_start/")
files <- dir(pattern = ".rds")

for (file in files){
  datas = data.table(readRDS(file))
  datas <- datas %>% filter(model != "observed")
  chill_season_start = unlist(strsplit(file, split='[.]'))[1]
  if (chill_season_start == )

  datas <- pick_up_okanagan_rich(datas) # Pick up okanagan And Richland
  datas <- within(datas, remove(thresh_20, thresh_25, thresh_30, thresh_35,
                                thresh_40, thresh_45, thresh_50, thresh_55, 
                                thresh_60, thresh_65,
                                thresh_70, thresh_75, sum))
  information = produce_data_4_plots(datas)

  safe_jan <- safe_box_plot(information[[1]], due="Jan.", chill_start = chill_season_start)
  safe_feb <- safe_box_plot(information[[4]], due="Feb.", chill_start = chill_season_start)
  safe_mar <- safe_box_plot(information[[7]], due="Mar.", chill_start = chill_season_start)

  # out_dir
  output_name = paste0("start_", chill_season_start, "_Jan_1.png")
  ggsave(output_name, safe_jan, path="/Users/hn/Desktop/", width=7, height=7, unit="in", dpi=300)

  output_name = paste0("start_", chill_season_start, "_Feb_1.png")
  ggsave(output_name, safe_feb, path="/Users/hn/Desktop/", width=7, height=7, unit="in", dpi=300)

  output_name = paste0("start_", chill_season_start, "_Mar_1.png")
  ggsave(output_name, safe_mar, path="/Users/hn/Desktop/", width=7, height=7, unit="in", dpi=300)
}

#########################################################
#######                                           #######
#######      Above plots, combined into 1         #######
#######                                           #######
#########################################################

rm(list=ls())
library(data.table)
library(dplyr)
library(ggmap)
library(ggplot2)
options(digit=9)
options(digits=9)

setwd("/Users/hn/Desktop/Desktop/Kirti/check_point/chilling/non_overlapping/different_chill_start")

mid_sept <- data.table(readRDS("mid_sept.rds"))
oct <- data.table(readRDS("oct.rds"))
mid_oct <- data.table(readRDS("mid_oct.rds"))
nov <- data.table(readRDS("nov.rds"))
mid_nov <- data.table(readRDS("mid_nov.rds"))

mid_sept$start = "sept_15"
oct$start = "oct_1"
mid_oct$start = "oct_15"
nov$start = "nov_1"
mid_nov$start = "nov_15"

dt <- rbind(mid_sept, oct, mid_oct, nov, mid_nov)
rm(mid_sept, oct, mid_oct, nov, mid_nov)

start_dates <- c("sept_15", "oct_1", "oct_15", "nov_1", "nov_15")
dt$start <- factor(dt$start, levels = start_dates, order=T)

dt <- dt %>% filter(model != "observed")
dt <- pick_up_okanagan_rich(dt) # Pick up okanagan And Richland

dt <- within(dt, remove(thresh_20, thresh_25, thresh_30, thresh_35,
                        thresh_40, thresh_45, thresh_50, thresh_55, 
                        thresh_60, thresh_65,
                        thresh_70, thresh_75, sum))

information = produce_data_4_safe_chill_box_plots_new_seasons(dt)

safe_jan <- information[[1]]
safe_feb <- information[[4]]
safe_mar <- information[[7]]
safe_apr <- information[[10]]

safe_jan$up_to <- "jan1"
safe_feb$up_to <- "feb1"
safe_mar$up_to <- "mar1"
safe_apr$up_to <- "apr1"

all_seasons_safe = rbind(safe_jan, safe_feb, safe_mar, safe_apr)
all_seasons_safe$location <- paste0(all_seasons_safe$lat, all_seasons_safe$long)

all_seasons_safe[location=="46.28125-119.34375"]$location <- "richland"
all_seasons_safe[location=="48.40625-119.53125"]$location <- "okanagan"

all_seasons_safe <- within(all_seasons_safe, remove(lat, long))

all_seasons_safe_dc <- dcast(all_seasons_safe,
                             time_period + scenario + climate_type + start + location + model ~ up_to,
                             value.var = "quan_90")


the_theme <-theme_bw() + 
            theme(plot.margin = unit(c(t=.2, r=.2, b=.2, l=0.2), "cm"),
                  plot.title = element_text(size = 30, face = "bold"),
                  panel.border = element_rect(fill=NA, size=.3),
                  panel.grid.major = element_line(size = 0.05),
                  panel.grid.minor = element_blank(),
                  panel.spacing.y = unit(.35, "cm"),
                  panel.spacing.x = unit(.25, "cm"),
                  legend.position = "bottom", 
                  legend.key.size = unit(3, "line"),
                  legend.spacing.x = unit(.2, 'cm'),
                  legend.title=element_text(size = 30),
                  legend.text = element_text(size = 25),
                  legend.margin = margin(t=0, r=0, b=0, l=0, unit = 'cm'),
                  strip.text.x = element_text(size = 22, color="black"),
                  strip.text.y = element_text(size = 22, color="black"),
                  axis.ticks = element_line(size = .1, color="black"),
                  axis.text.x = element_text(size = 20, face="bold", color="black"),
                  axis.text.y = element_text(size = 20, face="bold", color="black"),
                  axis.title.x = element_text(size = 22, face="plain", margin = margin(t=10, r=0, b=0, l=0)),
                  axis.title.y = element_text(size = 25, face="plain", margin = margin(t=0, r=10, b=0, l=0))
                  )

noch <- FALSE
box_width <- 1
color_ord = c("grey70" , "dodgerblue", "olivedrab4", "red", "yellow") # 
time_lab = c("Hist.", "'25-'50", "'51-'75", "'76-'99")


title_s <- "Safe chill by Jan. 1"
jan_1_box <- ggplot(data = all_seasons_safe_dc, aes(x=time_period, y=jan1, fill=start)) +
             geom_boxplot(outlier.size = -.3, notch= noch, width=box_width, lwd=.1) +
             facet_grid(~ scenario ~  climate_type) + 
             labs(x = "", y = "safe chill") + 
             scale_fill_manual(name = "chill season start",
                               values = color_ord,
                               labels = c("Sept. 15", "Oct. 1", "Oct. 15", "Nov. 1", "Nov. 15")) +
             scale_x_discrete(#breaks = x_breaks,
                              labels = time_lab) +
             ggtitle(title_s) +
             the_theme

title_s <- "Safe chill by Feb. 1"
feb_1_box <- ggplot(data = all_seasons_safe_dc, aes(x=time_period, y=feb1, fill=start)) +
             geom_boxplot(outlier.size = -.3, notch= noch, width=box_width, lwd=.1) +
             facet_grid(~ scenario ~  climate_type) + 
             labs(x = "", y = "") + 
             scale_fill_manual(name = "chill season start",
                               values = color_ord,
                               labels = c("Sept. 15", "Oct. 1", "Oct. 15", "Nov. 1", "Nov. 15")) +
             scale_x_discrete(#breaks = x_breaks,
                              labels = time_lab) +
             ggtitle(title_s) +
             the_theme

title_s <- "Safe chill by Mar. 1"
mar_1_box <- ggplot(data = all_seasons_safe_dc, aes(x=time_period, y=mar1, fill=start)) +
             geom_boxplot(outlier.size = -.3, notch= noch, width=box_width, lwd=.1) +
             facet_grid(~ scenario ~  climate_type) + 
             labs(x = "", y = "safe chill") + 
             scale_fill_manual(name = "chill season start",
                               values = color_ord,
                               labels = c("Sept. 15", "Oct. 1", "Oct. 15", "Nov. 1", "Nov. 15")) +
             scale_x_discrete(#breaks = x_breaks,
                              labels = time_lab) +
             ggtitle(title_s) +
             the_theme

title_s <- "Safe chill by Apr. 1"
apr_1_box <- ggplot(data = all_seasons_safe_dc, aes(x=time_period, y=apr1, fill=start)) +
             geom_boxplot(outlier.size = -.3, notch= noch, width=box_width, lwd=.1) +
             facet_grid(~ scenario ~  climate_type) + 
             labs(x = "", y = "") + 
             scale_fill_manual(name = "chill season start",
                               values = color_ord,
                               labels = c("Sept. 15", "Oct. 1", "Oct. 15", "Nov. 1", "Nov. 15")) +
             scale_x_discrete(#breaks = x_breaks,
                              labels = time_lab) +
             ggtitle(title_s) +
             the_theme

all_boxes <- ggarrange(jan_1_box, feb_1_box,
                       mar_1_box, apr_1_box,
                       ncol = 2, nrow = 2, common.legend = T,
                       legend = "bottom")

ggsave("all_chills.png", all_boxes,
       path = "/Users/hn/Desktop/", width=25, height=15, unit="in", dpi=250)

#########################################################
#
#      Box plot of CPs
#
#########################################################
rm(list=ls())
library(data.table)
library(dplyr)
library(ggmap)
library(ggplot2)
options(digit=9)
options(digits=9)

setwd("/Users/hn/Desktop/Desktop/Kirti/check_point/chilling/non_overlapping/different_chill_start")

mid_sept <- data.table(readRDS("mid_sept.rds"))
oct <- data.table(readRDS("oct.rds"))
mid_oct <- data.table(readRDS("mid_oct.rds"))
nov <- data.table(readRDS("nov.rds"))
mid_nov <- data.table(readRDS("mid_nov.rds"))

mid_sept$start = "sept_15"
oct$start = "oct_1"
mid_oct$start = "oct_15"
nov$start = "nov_1"
mid_nov$start = "nov_15"

dt <- rbind(mid_sept, oct, mid_oct, nov, mid_nov)
rm(mid_sept, oct, mid_oct, nov, mid_nov)

start_dates <- c("sept_15", "oct_1", "oct_15", "nov_1", "nov_15")
dt$start <- factor(dt$start, levels = start_dates, order=T)

dt <- within(dt, remove(thresh_20, thresh_25, thresh_30, thresh_35,
                        thresh_40, thresh_45, thresh_50, thresh_55, thresh_60, thresh_65,
                        thresh_70, thresh_75, sum, model))

dt <- pick_up_okanagan_rich(dt) # Pick up Omak And Richland
dt <- organize_non_over_time_period_two_hist(dt)

# make data cleaner for painless melting!
dt <- within(dt, remove(year, lat, long, location, chill_season))

the_theme <-theme_bw() + 
            theme(plot.margin = unit(c(t=.2, r=.2, b=.2, l=0.2), "cm"),
                  plot.title = element_text(size = 30, face = "bold"),
                  panel.border = element_rect(fill=NA, size=.3),
                  panel.grid.major = element_line(size = 0.05),
                  panel.grid.minor = element_blank(),
                  panel.spacing.y = unit(.35, "cm"),
                  panel.spacing.x = unit(.25, "cm"),
                  legend.position = "bottom", 
                  legend.key.size = unit(3, "line"),
                  legend.spacing.x = unit(.2, 'cm'),
                  legend.title=element_text(size = 30),
                  legend.text = element_text(size = 25),
                  legend.margin = margin(t=0, r=0, b=0, l=0, unit = 'cm'),
                  strip.text.x = element_text(size = 22, color="black"),
                  strip.text.y = element_text(size = 22, color="black"),
                  axis.ticks = element_line(size = .1, color="black"),
                  axis.text.x = element_text(size = 20, face="bold", color="black"),
                  axis.text.y = element_text(size = 20, face="bold", color="black"),
                  axis.title.x = element_text(size = 22, face="plain", margin = margin(t=10, r=0, b=0, l=0)),
                  axis.title.y = element_text(size = 25, face="plain", margin = margin(t=0, r=10, b=0, l=0))
                  )

noch <- FALSE
box_width <- 1
color_ord = c("grey70" , "dodgerblue", "olivedrab4", "red", "yellow") # 
time_lab = c("Hist.", "'25-'50", "'51-'75", "'76-'99")

title_s <- "Accumulated CP by Jan. 1"
jan_1_box <- ggplot(data = dt, aes(x=time_period, y=sum_J1, fill=start)) +
             geom_boxplot(outlier.size = -.3, notch= noch, width=box_width, lwd=.1) +
             facet_grid(~ scenario ~  climate_type) + 
             labs(x = "", y = "accumulated CP") + 
             scale_fill_manual(name = "chill season start",
                               values = color_ord,
                               labels = c("Sept. 15", "Oct. 1", "Oct. 15", "Nov. 1", "Nov. 15")) +
             scale_x_discrete(labels = time_lab) +
             ggtitle(title_s) +
             the_theme

title_s <- "Accumulated CP by Feb. 1"
feb_1_box <- ggplot(data = dt, aes(x=time_period, y=sum_F1, fill=start)) +
             geom_boxplot(outlier.size = -.3, notch= noch, width=box_width, lwd=.1) +
             facet_grid(~ scenario ~  climate_type) + 
             labs(x = "", y = "") + 
             scale_fill_manual(name = "chill season start",
                               values = color_ord,
                               labels = c("Sept. 15", "Oct. 1", "Oct. 15", "Nov. 1", "Nov. 15")) +
             scale_x_discrete(labels = time_lab) +
             ggtitle(title_s) +
             the_theme

title_s <- "Accumulated CP by Mar. 1"
mar_1_box <- ggplot(data = dt, aes(x=time_period, y=sum_M1, fill=start)) +
             geom_boxplot(outlier.size = -.3, notch= noch, width=box_width, lwd=.1) +
             facet_grid(~ scenario ~  climate_type) + 
             labs(x = "", y = "accumulated CP") + 
             scale_fill_manual(name = "chill season start",
                               values = color_ord,
                               labels = c("Sept. 15", "Oct. 1", "Oct. 15", "Nov. 1", "Nov. 15")) +
             scale_x_discrete(labels = time_lab) +
             ggtitle(title_s) +
             the_theme

title_s <- "Accumulated CP by Apr. 1"
apr_1_box <- ggplot(data = dt, aes(x=time_period, y=sum_A1, fill=start)) +
             geom_boxplot(outlier.size = -.3, notch= noch, width=box_width, lwd=.1) +
             facet_grid(~ scenario ~  climate_type) + 
             labs(x = "", y = "") + 
             scale_fill_manual(name = "chill season start",
                               values = color_ord,
                               labels = c("Sept. 15", "Oct. 1", "Oct. 15", "Nov. 1", "Nov. 15")) +
             scale_x_discrete(labels = time_lab) +
             ggtitle(title_s) +
             the_theme

all_boxes <- ggarrange(jan_1_box, feb_1_box,
                       mar_1_box, apr_1_box,
                       ncol = 2, nrow = 2, common.legend = T,
                       legend = "bottom")

ggsave("all_boxes.png", all_boxes,
       path = "/Users/hn/Desktop/", width=25, height=15, unit="in", dpi=250)

################################################################
################################################################
################################################################
################################################################
