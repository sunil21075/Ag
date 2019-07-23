library(ggmap)
library(ggpubr)
library(lubridate)
library(purrr)
library(scales)
library(tidyverse)
# library(ggplot2)

options(digits=9)
options(digits=9)
############################################################
#
#                         Functions
#
############################################################
one_time_medians <- function(dt, min, max, ttl, subttl){
  tgt_col <- "twenty_five_years"
  x <- sapply(dt$location, 
              function(x) strsplit(x, "_")[[1]], 
              USE.NAMES=FALSE)
  lat <- as.numeric(x[1, ]); long <- as.numeric(x[2, ])
  dt$lat <- lat; dt$long <- long;

  states <- map_data("state")
  states_cluster <- subset(states, 
                           region %in% c("washington"))
  dt %>%
  ggplot() +
  geom_polygon(data = states_cluster, 
               aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  geom_point(aes_string(x = "long", y = "lat",
                        color = tgt_col), 
             alpha = 1,
             size=.3) +
  scale_color_viridis_c(option = "plasma", 
                        name = "storm", direction = -1,
                        limits = c(min, max),
                        breaks = pretty_breaks(n = 4)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12, face="plain"),
        legend.title = element_blank(),
        # legend.justification = c(.93, .9),
        # legend.position = c(.93, .9),
        legend.position = "top",
        strip.text = element_text(size=14, face="bold"))+
  ggtitle(ttl, subtitle=subttl)
}

all_mods_map <- function(dt, min, max, ttl){
  tgt_col <- "twenty_five_years"
  x <- sapply(dt$location, 
              function(x) strsplit(x, "_")[[1]], 
              USE.NAMES=FALSE)
  lat <- as.numeric(x[1, ]); long <- as.numeric(x[2, ])
  dt$lat <- lat; dt$long <- long;

  states <- map_data("state")
  states_cluster <- subset(states, 
                           region %in% c("washington"))
  dt %>%
  ggplot() +
  geom_polygon(data = states_cluster, 
               aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  geom_point(aes_string(x = "long", y = "lat",
                        color = tgt_col), 
             alpha = 1,
             size = .15) +
  facet_wrap(~ model) +
  scale_color_viridis_c(option = "plasma", 
                        name = "storm", direction = -1,
                        limits = c(min, max),
                        breaks = pretty_breaks(n = 4)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12, face="plain"),
        legend.title = element_blank(),
        legend.position = "top",
        legend.margin = margin(t=-1, r=0, b=0, l=0, unit = 'line'),
        strip.text = element_text(size=14, face="bold"))+
  ggtitle(ttl)
}

obs_hist_map <- function(dt, min, max) {
  tgt_col <- "twenty_five_years"
  x <- sapply(dt$location, 
              function(x) strsplit(x, "_")[[1]], 
              USE.NAMES=FALSE)
  lat <- as.numeric(x[1, ]); long <- as.numeric(x[2, ])
  dt$lat <- lat; dt$long <- long;

  states <- map_data("state")
  states_cluster <- subset(states, 
                           region %in% c("washington"))
  dt %>%
  ggplot() +
  geom_polygon(data = states_cluster, 
               aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  geom_point(aes_string(x = "long", y = "lat",
                        color = tgt_col), 
                        alpha = 1,
                        size=.3) +
  scale_color_viridis_c(option = "plasma", 
                        name = "storm", direction = -1,
                        limits = c(min, max),
                        breaks = pretty_breaks(n = 4)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12, face="plain"),
        legend.title = element_blank(),
        legend.justification = c(.93, .9),
        legend.position = c(.93, .9),
        strip.text = element_text(size=14, face="bold")) +
  ggtitle("Observed historical")
}

box_trend_monthly <- function(dt, p_type="trend", trend_type="median"){
  #
  # input p_type is in {box, trend} (box plot or line plot)
  #   trend_type is in {mean, median} (line plot)
  #
  dt <- within(dt, remove(day, precip, model))
  dt$cluster <- as.character(dt$cluster)

  cluster_label <- as.character(c(4, 3, 2, 1))
  str_labels <- c("4" = "most precip.", 
                  "3" ="less precip.", 
                  "2" = "lesser precip.", 
                  "1" = "least precip.")
  
  month_names <- c("1" = "Jan.", "2" = "Feb.", "3" = "Mar.", 
                   "4" = "Apr.", "5" = "May.", "6" = "Jun.", 
                   "7" = "Jul.", "8" = "Aug.", "9" = "Sept.", 
                   "10" = "Oct.", "11" = "Nov.", "12" = "Dec.")
  
  time_p_lbl <- c("1950-2005", "1979-2016", 
                  "2006-2025", "2026-2050",
                  "2051-2075", "2076-2099")
  
  if (p_type=="box"){
    dt <- within(dt, remove(year))
    # color_ord = c("red", "purple", "dodgerblue2", "blue4")
    color_ord = c("grey47", "dodgerblue2", "olivedrab4", "red",
                  "blue3", "gold")
    melted <- melt(dt, id = c("location", "month",
                              "time_period", "emission",
                              "cluster"))
    
    melted$cluster <- factor(melted$cluster, 
                             levels=cluster_label)
    melted$month <- factor(melted$month, levels=1:12)
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
                 axis.title.x = element_text(size = ax_ttl_size , face = "bold",
                                             margin = margin(t=2, r=0, b=-10, l=0))
                      )

    box_p <- ggplot(data = melted, 
                    aes(x=month, y=value, fill=time_period)) +
             the + 
             geom_boxplot(outlier.size = - 0.3, notch=F, 
                        width = box_width, lwd=.1, 
                        position = position_dodge(0.6)) +
             # labs(x="", y="") + # theme_bw() + 
             facet_grid(~ emission ~ cluster,
                        labeller=labeller(cluster = str_labels)) +
             xlab("month") + 
             ylab("monthly cum. precip. (mm)") + 
             scale_x_discrete(breaks=1:12,
                              labels=month_names) +  
             scale_fill_manual(values = color_ord,
                               name = "time\nperiod", 
                               labels = time_p_lbl)
    return(box_p)

  } else {
    ax_txt_size <- 12; ax_ttl_size <- 14;
    the <- theme(plot.margin = unit(c(t=.1, r=.2, b=.1, l=0.2), "cm"),
                 panel.border = element_rect(fill=NA, size=.3),
                 panel.grid.major = element_line(size = 0.05),
                 panel.grid.minor = element_blank(),
                 legend.position = "bottom", 
                 legend.key.size = unit(1.2, "line"),
                 legend.spacing.x = unit(1, 'line'),
                 panel.spacing.x = unit(2, 'line'),
                 panel.spacing.y = unit(1, 'line'),
                 legend.text = element_text(size = ax_ttl_size, face="bold"),
                 legend.margin = margin(t=.1, r=0, b=0, l=0, unit = 'line'),
                 legend.title = element_blank(),
                 plot.title = element_text(size = ax_ttl_size, face = "bold"),
                 plot.subtitle = element_text(face = "bold"),
                 strip.text.x = element_text(size = ax_ttl_size, face = "bold",
                                             margin = margin(.15, 0, .15, 0, "line")),
                 strip.text.y = element_text(size = ax_ttl_size, face = "bold",
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
                 axis.title.x = element_text(size = ax_ttl_size , face = "bold",
                                             margin = margin(t=2, r=0, b=-10, l=0))
                      )
  
    if (trend_type=="median"){
      dt <- dt %>%
            group_by(time_period, month, 
                     emission, cluster, year) %>%
            summarise(stat_col=median(monthly_cum_precip))%>%
            data.table()
      } else {
        dt <- dt %>%
              group_by(time_period, month, 
                       emission, cluster, year) %>%
              summarise(stat_col=mean(monthly_cum_precip))%>%
              data.table()
      }
      
    dt$month <- as.character(dt$month)
    dt$month <- factor(dt$month, levels=as.character(1:12))
    line_p <- ggplot(data=dt, 
                     aes(x=year, y=stat_col, 
                         group=time_period, 
                         color=time_period)) +
              geom_line() +
              the + 
              facet_grid(~ emission ~ cluster  ~ month,
                         labeller=labeller(cluster = str_labels,
                                           month = month_names))
      return(line_p)
  }
}

cum_box_cluster_x <- function(dt, tgt_col){
  cluster_label <- c(4, 3, 2, 1)
  categ_label <- c("most precip", "less precip", 
                   "lesser precip", "least precip")
  time_label <- c("1950-2005", "1979-2016", 
                   "2006-2025", "2026-2050",
                   "2051-2075", "2076-2099")

  # color_ord = c("red", "purple", "dodgerblue2", "blue4")
  color_ord = c("grey47", "dodgerblue2", "olivedrab4", "red",
                "blue3", "gold")
  
  # medians <- data.frame(dt) %>% 
  #            group_by(cluster, time_period, emission) %>% 
  #            summarise( medians = median(get(tgt_col)))  %>% 
  #            data.table()
  dt <- within(dt, remove( day, precip, wtr_yr))
  if (tgt_col=="annual_cum_precip"){
    dt <- within(dt, remove(month, day))
  }  
  melted <- melt(dt, id = c("location", "year", 
                            "time_period", "model", "emission",
                            "cluster"))
  
  melted$cluster <- factor(melted$cluster, levels=c(4, 3, 2, 1))

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
               axis.title.x = element_text(size = ax_ttl_size , face = "bold",
                                           margin = margin(t=2, r=0, b=-10, l=0))
                    )

  box_p <- ggplot(data = melted, 
                  aes(x=cluster, y=value, fill=time_period)) +
           the + 
           geom_boxplot(outlier.size = - 0.3, notch=F, 
                        width = box_width, lwd=.1, 
                        position = position_dodge(0.6)) +
           # labs(x="", y="") + # theme_bw() + 
           facet_grid(~ emission) +
           xlab("time period") + 
           ylab("annual cum. precip. (mm)") + 
           scale_x_discrete(breaks=c(4, 3, 2, 1),
                            labels=categ_label) +  
           scale_fill_manual(values = color_ord,
                             name = "time\nperiod", 
                             labels = time_label)
  
  return(box_p)
}

cum_clust_box_plots <- function(dt, tgt_col){
  cluster_label <- c(4, 3, 2, 1)
  categ_label <- c("most precip", "less precip", 
                   "lesser precip", "least precip")

  # color_ord = c("red", "purple", "dodgerblue2", "blue4")
  color_ord = c("grey47", "dodgerblue2", "olivedrab4", "red")
  
  # medians <- data.frame(dt) %>% 
  #            group_by(cluster, time_period, emission) %>% 
  #            summarise( medians = median(get(tgt_col)))  %>% 
  #            data.table()
  dt <- within(dt, remove(day, precip, wtr_yr))
  if (tgt_col=="annual_cum_precip"){
    dt <- within(dt, remove(month, day))
  }

  dt <- dt %>% filter(get(tgt_col) >= 0)%>% data.table()
  
  melted <- melt(dt, id = c("location", "year", 
                            "time_period", "model", "emission",
                            "cluster"))
  
  melted$cluster <- factor(melted$cluster, levels=c(4, 3, 2, 1))

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
               axis.title.x = element_text(size = ax_ttl_size , face = "bold",
                                           margin = margin(t=2, r=0, b=-10, l=0))
                    )

  box_p <- ggplot(data = melted, 
                  aes(x=time_period, y=value, fill=cluster)) +
           the +
           geom_boxplot(outlier.size = - 0.3, notch=F, 
                        width = box_width, lwd=.1, 
                        position = position_dodge(0.6)) +
           # labs(x="", y="") + # theme_bw() + 
           facet_grid(~ emission) +
           xlab("time period") + 
           ylab("annual cum. precip. (mm)") + 
           scale_fill_manual(values = color_ord,
                             name = "precip\nlevel", 
                             labels = categ_label)
           
  return(box_p)
}

storm_box_plot <- function(data_tb){
  categ_lab <- sort(unique(data_tb$return_period))
  color_ord = c("grey47", # "blue3", 
                "olivedrab4", 
                "red", "steelblue1", "gold")
  x_ticks <- c("5", "10", "15", "20", "25")

  # medians <- data.frame(data_tb) %>% 
  #            group_by(return_period, emission) %>% 
  #            summarise( med_5 = median(five_years),
  #                      med_10 = median(ten_years),
  #                      med_15 = median(fifteen_years),
  #                      med_20 = median(twenty_years),
  #                      med_25 = median(twenty_five_years))  %>% 
  #            data.table()

  melted <- melt(data_tb,  id = c("location", "model", 
                                  "return_period", "emission"))
  
  ax_txt_size <- 5; ax_ttl_size <- 6; box_width = 0.53
  the <- theme(plot.margin = unit(c(t=.1, r=.2, b=.1, l=0.2), "cm"),
               panel.border = element_rect(fill=NA, size=.3),
               panel.grid.major = element_line(size = 0.05),
               panel.grid.minor = element_blank(),
               panel.spacing = unit(.35, "line"),
               legend.position = "bottom", 
               legend.key.size = unit(.6, "line"),
               legend.spacing.x = unit(.1, 'line'),
               panel.spacing.y = unit(.5, 'line'),
               legend.text = element_text(size = ax_ttl_size),
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
               axis.title.x = element_text(size = ax_ttl_size , face = "bold",
                                           margin = margin(t=2, r=0, b=-10, l=0))
                    )

  box_p <- ggplot(data = melted, 
                  aes(x=variable, y=value, fill=return_period)) +
           geom_boxplot(outlier.size = - 0.3, notch=F, 
                        width = box_width, lwd=.1, 
                        position = position_dodge(0.6)) +
           # labs(x="", y="") + # theme_bw() + 
           facet_grid(~ emission) +
           scale_x_discrete(labels=c("five_years" = "5", 
                                     "ten_years" = "10",
                                     "fifteen_years" = "15",
                                     "twenty_years" = "20",
                                     "twenty_five_years" = "25")) + 
           xlab("time interval (years)") + 
           ylab("24 hr design storm int. (mm/hr)") + 
           scale_fill_manual(values = color_ord,
                             name = "Return\nPeriod", 
                             labels = categ_lab) + 
           the
}

geo_map_of_clusters <- function(obs_w_clusters){
  obs_w_clusters <- subset(obs_w_clusters, 
                           select=c(location, cluster))
  obs_w_clusters <- unique(obs_w_clusters)
  
  x <- sapply(obs_w_clusters$location, 
              function(x) strsplit(x, "_")[[1]], 
              USE.NAMES=FALSE)
  lat = as.numeric(x[1, ]); long = as.numeric(x[2, ])
  
  obs_w_clusters$lat <- lat
  obs_w_clusters$long <- long
  obs_w_clusters <- within(obs_w_clusters, remove(location))

  states <- map_data("state")
  WA_state <- subset(states, region %in% c("washington"))

  # "grey47",
  color_ord = c("red", "purple", "dodgerblue2", "blue4")
  # color_ord = c("00CCFF", "006699", "maroon3", "red")
  # color_ord = c("red", "maroon3", "royalblue3", "steelblue1")
  categ_lab = c("least precip", "lesser precip",
                "less precip", "most precip")

  the_theme <- theme(plot.margin = unit(c(t=.2, r=.2, b=.2, l=0.2), "cm"),
                     panel.border = element_rect(fill=NA, size=.3),
                     legend.position = "bottom",
                     legend.key.size = unit(1, "line"),
                     legend.spacing.x = unit(.1, 'line'),
                     panel.spacing.y = unit(.5, 'line'),
                     legend.text = element_text(size = 10, face="bold"),
                     legend.margin = margin(t=.4, r=0, b=0, l=0, unit = 'line'),
                     legend.title = element_blank(),
                     plot.title = element_text(size = 13, face = "bold"),
                     plot.subtitle = element_text(size = 9, face = "bold"),
                     axis.ticks = element_blank(),
                     axis.text.y = element_blank(),
                     axis.text.x = element_blank(),
                     axis.title.y = element_blank(),
                     axis.title.x = element_blank())

  cluster_plot <- obs_w_clusters %>%
                  ggplot() +
                  geom_polygon(data = WA_state, 
                              aes(x=long, y=lat, group = group),
                                  fill = "grey", color = "black", size=0.5) +
                  geom_point(aes_string(x = "long", y = "lat", color="cluster"), 
                             alpha = 1, size=0.8) + 
                  scale_color_manual(values = color_ord,
                                    name = "Precip.\n", 
                                    labels = categ_lab) + 
                  the_theme +
                  # size of dot inside the legend
                  guides(colour = guide_legend(override.aes = list(size=3))) + 
                  labs(title = "Groups of grids based on annual precip.",
                       subtitle = "averaged over 38 years.")
                  ggtitle("Groups of grids")
  return(cluster_plot)
}

satellite_map_of_clusters <- function(obs_w_clusters){
  obs_w_clusters <- subset(obs_w_clusters, 
                           select=c(location, cluster))
  obs_w_clusters <- unique(obs_w_clusters)
  
  x <- sapply(obs_w_clusters$location, 
              function(x) strsplit(x, "_")[[1]], 
              USE.NAMES=FALSE)
  lat = as.numeric(x[1, ]); long = as.numeric(x[2, ])
  
  obs_w_clusters$lat <- lat
  obs_w_clusters$long <- long
  obs_w_clusters <- within(obs_w_clusters, remove(location))
       
  sh_dir_1 <- "/Users/hn/Documents/GitHub/large_4_GitHub/"
  sh_dir <- paste0(sh_dir_1, "4_analog_web_site/tl_2017_us_county_simple/")
  counties <- rgdal::readOGR(dsn=path.expand(sh_dir), 
                             layer = "tl_2017_us_county")

  # Extract just the three states OR: 41, WA:53, ID: 16
  counties <- counties[counties@data$STATEFP %in% c("53"), ]

  # ggplot(counties) + 
  # aes(long, lat, group=group) + 
  # geom_polygon() +
  # geom_path(color="white") +
  # coord_equal()

  # ggplot()+ 
  # geom_polygon(data=counties, aes(long, lat, group = group, fill = hole), 
  #              colour = alpha("white", 1/2), size = 0.7) + 
  # scale_fill_manual(values = c("grey47", "white")) + 
  # theme(legend.position="none")

  # "grey47",
  color_ord = c("blue4", "dodgerblue2", "purple", "red") 
  categ_lab = c("most precip", "less precip", 
                "lesser precip", "least precip")

  the_theme <- theme(plot.margin = unit(c(t=.2, r=.2, b=.2, l=0.2), "cm"),
                     panel.border = element_rect(fill=NA, size=.3),
                     legend.position = "bottom",
                     legend.key.size = unit(1, "line"),
                     legend.spacing.x = unit(.1, 'line'),
                     panel.spacing.y = unit(.5, 'line'),
                     legend.text = element_text(size = 10, face="bold"),
                     legend.margin = margin(t=.4, r=0, b=0, l=0, unit = 'line'),
                     legend.title = element_blank(),
                     plot.title = element_text(size = 13, face = "bold"),
                     plot.subtitle = element_text(size = 9, face = "bold"),
                     axis.ticks = element_blank(),
                     axis.text.y = element_blank(),
                     axis.text.x = element_blank(),
                     axis.title.y = element_blank(),
                     axis.title.x = element_blank())
   
  cluster_plot <- obs_w_clusters %>%
                  ggplot() +
                  qmap('washington', zoom = 6, maptype = 'satellite') +
                  geom_polygon(data = WA_state, 
                              aes(x=long, y=lat, group = group),
                                  fill = "grey", color = "black", size=0.5) +
                  geom_point(aes_string(x = "long", y = "lat", color="cluster"), 
                             alpha = 1, size=0.8) + 
                  scale_color_manual(values = color_ord,
                                    name = "Precip.\n", 
                                    labels = categ_lab) + 
                  the_theme +
                  # size of dot inside the legend
                  guides(colour = guide_legend(override.aes = list(size=3))) + 
                  labs(title = "Groups of grids based on annual precip.",
                       subtitle = "averaged over 38 years.")
                  ggtitle("Groups of grids")
  return(cluster_plot)
}


