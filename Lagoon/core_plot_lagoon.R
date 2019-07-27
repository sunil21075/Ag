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
#
# change the name and maintain the code so it is 
# funtional for wide range of data.
# lets say one column is location, and one column determines
# color of stuff on the map.
#
geo_map_of_diffs <- function(dt, col_col, minn, maxx, ttl, subttl){
  
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
                        color = col_col), 
             alpha = 1,
             size=.3) +
  guides(fill = guide_colourbar(barwidth = .1, barheight = 20))+
  # scale_color_viridis_c(option = "plasma", 
  #                       name = "storm", direction = -1,
  #                       limits = c(min, max),
  #                       # begin = 0.5, end = 1,
  #                       breaks = pretty_breaks(n = 3)) +
  scale_color_gradient2(breaks = c((as.integer(minn*0.5)), 
                                    0,
                                    (as.integer(maxx/2)), 
                                    (as.integer((maxx-1)*0.85))),
                        labels = c((as.integer(minn*0.5)), 
                                   0, 
                                   (as.integer(maxx/2)),
                                   (as.integer((maxx-1)*0.85))),
                        low = "red", high = "blue", mid = "white",
                        space="Lab"
                        ) +
  # scale_color_continuous(breaks = c(as.integer(minn+1), 0, as.integer(maxx-1)),
  #                        labels = c(as.integer(minn+1), 0, as.integer(maxx-1)),
  #                        low = "red", high = "blue") + 
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

#
# Following works are on both precip and runoffs
#
ann_wtrYr_chunk_cum_box_cluster_x <- function(dt, y_lab, tgt_col){
  if (tgt_col=="annual_cum_runbase" | tgt_col=="chunk_cum_runbase"){
    dt <- within(dt, remove(evap, runoff, base_flow, run_p_base))
  }
  
  # toss unwanted time periods
  dt <- dt %>% 
        filter(# time_period != "1950-2005" & 
               time_period != "2006-2025") %>% 
        data.table()

  suppressWarnings({ dt <- within(dt, remove(month, day, precip, model, wtr_yr))})
  dt <- cluster_numeric_2_str(dt)
  
  medians <- data.frame(dt) %>% 
             group_by(cluster, time_period, emission) %>% 
             summarise( med = median(get(tgt_col))) %>% 
             data.table()

  melted <- melt(dt, id = c("location", "year", 
                            "time_period", "emission",
                            "cluster"))
  rm(dt)

  categ_label <- c("most precip", "less precip", 
                   "lesser precip", "least precip")
  
  # time_label <- c("1979-2016", "2026-2050", "2051-2075", "2076-2099")
  # color_ord = c("grey47", "dodgerblue2", "olivedrab4", "gold")
  
  time_label <- c("1950-2005", "1979-2016", "2026-2050", "2051-2075", "2076-2099")
  color_ord = c("red", "grey47", "dodgerblue2", "olivedrab4", "gold")

  melted$cluster <- factor(melted$cluster, levels=categ_label)
  melted$time_period <- factor(melted$time_period, levels=time_label)
  
  ax_txt_size <- 6; ax_ttl_size <- 8; box_width = 0.53

  the <- theme(plot.margin = unit(c(t=.1, r=.2, b=.1, l=0.2), "cm"),
               panel.border = element_rect(fill=NA, size=.3),
               panel.grid.major = element_line(size = 0.05),
               panel.grid.minor = element_blank(),
               panel.spacing = unit(.35, "line"),
               legend.position = "bottom", 
               legend.key.size = unit(.8, "line"),
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
  ########
  ########    PLOT
  ########
  ggplot(data = melted, aes(x=cluster, y=value, fill=time_period)) +
  the + 
  geom_boxplot(outlier.size = - 0.3, notch=F, 
                        width = box_width, lwd=.1, 
                        position = position_dodge(0.8)) +
  scale_x_discrete(expand=c(0.1, 0)) + 
  # labs(x="", y="") + # theme_bw() + 
  facet_grid(~ emission) +
  xlab("precip. group") +
  ylab(y_lab) + 
  scale_fill_manual(values = color_ord, labels = time_label) +
  geom_text(data = medians, 
            aes(label = sprintf("%1.0f", medians$med), y = medians$med), 
            size = 2, fontface = "bold",
            position = position_dodge(.8), vjust = -1
            # hjust = 1
            )

}

box_trend_monthly_cum <- function(dt, p_type="trend", 
                                  trend_type="median", y_lab, tgt_col){
  #
  # input p_type is in {box, trend} (box plot or line plot)
  #   trend_type is in {mean, median} (line plot)
  #

  dt <- within(dt, remove(day, precip, model))
  if ("evap" %in% colnames(dt)){
    dt <- within(dt, remove(evap, runoff, base_flow, run_p_base))
  }
  dt <- cluster_numeric_2_str(dt)

  categ_label <- c("most precip", "less precip", 
                   "lesser precip", "least precip")  
  dt$cluster <- factor(dt$cluster, levels=categ_label)
  
  month_names <- c("1" = "Jan.", "2" = "Feb.", "3" = "Mar.",
                   "4" = "Apr.", "5" = "May.", "6" = "Jun.", 
                   "7" = "Jul.", "8" = "Aug.", "9" = "Sept.", 
                   "10" = "Oct.", "11" = "Nov.", "12" = "Dec.")
  
  if (p_type=="box"){
    dt <- dt %>% 
          filter(# time_period != "1950-2005" & 
                 time_period != "2006-2025") %>% 
          data.table()
    
    time_lbl <- c("1979-2016", "2026-2050",
                  "2051-2075", "2076-2099")
    color_ord = c("grey47", "dodgerblue2", "olivedrab4", "gold")
 
    time_lbl <- c("1950-2005", "1979-2016", "2026-2050",
                  "2051-2075", "2076-2099")
    color_ord = c("red", "grey47", "dodgerblue2", "olivedrab4", "gold")

    dt$time_period <- factor(dt$time_period, levels=time_lbl)
    dt <- within(dt, remove(year))

    # medians <- data.frame(dt) %>% 
    #            group_by(cluster, time_period, emission, month) %>% 
    #            summarise( med = median(get(tgt_col))) %>% 
    #            data.table()

    melted <- melt(dt, id = c("location", "month",
                              "time_period", "emission",
                              "cluster"))
    rm(dt)

    melted$month <- factor(melted$month, 
                           levels=c(9, 10, 11, 12, 1, 2, 3, 
                                    4, 5, 6, 7, 8))

    ax_txt_size <- 8; ax_ttl_size <- 12; box_width = 0.53
    the <- theme(plot.margin = unit(c(t=.1, r=.2, b=.1, l=0.2), "cm"),
                 panel.border = element_rect(fill=NA, size=.3),
                 panel.grid.major = element_line(size = 0.05),
                 panel.grid.minor = element_blank(),
                 panel.spacing = unit(.35, "line"),
                 legend.position = "bottom", 
                 legend.key.size = unit(1, "line"),
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
    
    box_p <- ggplot(data = melted, 
                    aes(x=month, y=value, fill=time_period)) +
             the + 
             geom_boxplot(outlier.size = - 0.3, notch=F, 
                        width = box_width, lwd=.1, 
                        position = position_dodge(0.6)) +
             # labs(x="", y="") + # theme_bw() + 
             facet_grid(~ emission ~ cluster) +
             # xlab("month") + 
             ylab(y_lab) + 
             scale_x_discrete(# breaks=1:12,
                              labels=month_names,) +  
             scale_fill_manual(values = color_ord,
                               name = "time\nperiod", 
                               labels = time_lbl) + 
             scale_y_continuous(breaks=c(125, 250, 500, 1000, 
                                         1500, 2000, 2500)) 
             # +
             # geom_text(data = medians, 
             #           aes(label = sprintf("%1.0f", medians$med), y = medians$med), 
             #           size = 2, vjust = -1.4,
             #           position = position_dodge(.9)
             #           # hjust = 1
             #           )

             # + 
             # geom_hline(yintercept= 125, color = "white", size=.2)+
             # geom_hline(yintercept= 250, color = "white", size=.2)+
             # geom_hline(yintercept= 500, color = "white", size=.2)
            
    return(box_p)

  } else {
    time_lbl <- c("1950-2005", "1979-2016", "2006-2025", 
                  "2026-2050", "2051-2075", "2076-2099")
    dt$time_period <- factor(dt$time_period, levels=time_lbl)
  
    if (trend_type=="median"){
      dt <- dt %>%
            group_by(time_period, month, 
                     emission, cluster, year) %>%
            summarise(stat_col=median(monthly_cum_precip))%>%
            data.table()
      y_lab <- "medians of cum. monthly precip. (over models)"
      } else {
        dt <- dt %>%
              group_by(time_period, month, 
                       emission, cluster, year) %>%
              summarise(stat_col=mean(monthly_cum_precip))%>%
              data.table()
        y_lab <- "means of cum. monthly precip. (over models)"
    }
      
    dt$month <- as.character(dt$month)
    dt$month <- factor(dt$month, 
                       levels=as.character(c(9, 10, 11, 12, 
                                             1, 2, 3, 4, 5,
                                             6, 7, 8)))

    ax_txt_size <- 15; ax_ttl_size <- 17;
    the <- theme(plot.margin = unit(c(t=.1, r=.2, b=.1, l=0.2), "cm"),
                 panel.border = element_rect(fill=NA, size=.3),
                 panel.grid.major = element_line(size = 0.05),
                 panel.grid.minor = element_blank(),
                 legend.position = "bottom", 
                 legend.key.size = unit(1.5, "line"),
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
  
    line_p <- ggplot(data=dt, 
                     aes(x=year, y=stat_col, 
                         group=time_period, 
                         color=time_period)) +
              geom_line() +
              the + 
              facet_grid(~ emission ~ cluster ~ month,
                         labeller=labeller(month = month_names))+
              ylab(y_lab)

      return(line_p)
  }
}

############################################################
#      
#            STORM
#
#
one_time_medians_storm_geoMap <- function(dt, minn, maxx, ttl, subttl, differ=FALSE){
  # Storm related
  # one_time_medians means one-time in the sense of 25 years
  # strom, (not including 5, 10, 15, 20 years)

  tgt_col <- "twenty_five_years"
  x <- sapply(dt$location, 
              function(x) strsplit(x, "_")[[1]], 
              USE.NAMES=FALSE)
  lat <- as.numeric(x[1, ]); long <- as.numeric(x[2, ])
  dt$lat <- lat; dt$long <- long;

  states <- map_data("state")
  states_cluster <- subset(states, 
                           region %in% c("washington"))

   th <- theme(axis.title.y = element_blank(),
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
               strip.text = element_text(size=14, face="bold"))
  
  if (differ==TRUE){
    bbb <- dt %>%
          ggplot() +
          geom_polygon(data = states_cluster, 
                       aes(x = long, y = lat, group = group),
                       fill = "grey", color = "black") +
          geom_point(aes_string(x = "long", y = "lat",
                                color = tgt_col), 
                     alpha = 1, size=.3) +
          scale_color_gradient2(breaks = c((as.integer(minn*0.5)), 
                                            0,
                                            (as.integer(maxx/2)), 
                                            (as.integer((maxx-1)*0.85))),
                                labels = c((as.integer(minn*0.5)), 
                                           0, 
                                           (as.integer(maxx/2)),
                                           (as.integer((maxx-1)*0.85))),
                                low = "red", high = "blue", mid = "white",
                                space="Lab"
                                )  +
          th +
          ggtitle(ttl, subtitle=subttl)
      } else{
       bbb <- dt %>%
              ggplot() +
              geom_polygon(data = states_cluster, 
                           aes(x = long, y = lat, group = group),
                           fill = "grey", color = "black") +
              geom_point(aes_string(x = "long", y = "lat",
                                    color = tgt_col), 
                         alpha = 1, size=.3) +
              scale_color_viridis_c(option = "plasma", 
                                    name = "storm", direction = -1,
                                    limits = c(min, max),
                                    breaks = pretty_breaks(n = 4)) +
              th +
              ggtitle(ttl, subtitle=subttl)
    }
    return(bbb)
}

#####################################
#####################################
all_mods_map_storm <- function(dt, minn, maxx, ttl){
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
        strip.text = element_text(size=14, face="bold")) + 
  ggtitle(ttl)
}

obs_hist_map_storm <- function(dt, minn, maxx, fips_clust, tgt_col="twenty_five_years") {
  
  dt <- add_coord_from_location(dt)
  dt <- merge(dt, fips_clust, by="location", all.x=T)
  dt <- within(dt, remove(location, model, return_period))

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
                        limits = c(minn, maxx),
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
  # +
  # geom_polygon(fill = "transparent", color = "red", size = .1, 
  #               data = dt, aes(x = long, y = lat, group = cluster))

}

storm_box_plot <- function(data_tb){
  categ_lab <- sort(unique(data_tb$return_period))
  color_ord = c("grey47", # "blue3", 
                "olivedrab4", 
                "red", "steelblue1", "gold")
  x_ticks <- c("5", "10", "15", "20", "25")

  data_tb$cluster <- factor(data_tb$cluster, levels=c(4, 3, 2, 1))
  cluster_label <- as.character(c(4, 3, 2, 1))
  str_labels <- c("4" = "most precip.", 
                  "3" ="less precip.", 
                  "2" = "lesser precip.", 
                  "1" = "least precip.")

  # medians <- data.frame(data_tb) %>% 
  #            group_by(return_period, emission) %>% 
  #            summarise( med_5 = median(five_years),
  #                      med_10 = median(ten_years),
  #                      med_15 = median(fifteen_years),
  #                      med_20 = median(twenty_years),
  #                      med_25 = median(twenty_five_years))  %>% 
  #            data.table()

  melted <- melt(data_tb,  id = c("location", "model", "cluster",
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
           facet_grid(~ emission ~ cluster,
                      labeller=labeller(cluster = str_labels)) +
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
           scale_y_continuous(breaks = 1:20) + 
           the
}
############################################################
#      
#            Precip
#
#
ann_wtrYr_chunk_cumP_box_cluster_x <- function(dt, y_lab, tgt_col){
  # toss unwanted time periods
  dt <- dt %>% 
        filter(time_period != "1950-2005" & 
               time_period != "2006-2025") %>% 
        data.table()

  dt <- within(dt, remove(month, day, precip, model, wtr_yr))
  dt <- cluster_numeric_2_str(dt)
  # medians <- data.frame(dt) %>% 
  #            group_by(cluster, time_period, emission) %>% 
  #            summarise( medians = median(get(tgt_col)))  %>% 
  #            data.table()

  melted <- melt(dt, id = c("location", "year", 
                            "time_period", "emission",
                            "cluster"))

  categ_label <- c("most precip", "less precip", 
                   "lesser precip", "least precip")
  time_label <- c("1979-2016", "2026-2050", "2051-2075", "2076-2099")
  melted$cluster <- factor(melted$cluster, levels=categ_label)
  melted$time_period <- factor(melted$time_period, levels=time_label)
  
  color_ord = c("grey47", "dodgerblue2", "olivedrab4", "gold")
  ax_txt_size <- 6; ax_ttl_size <- 8; box_width = 0.53

  the <- theme(plot.margin = unit(c(t=.1, r=.2, b=.1, l=0.2), "cm"),
               panel.border = element_rect(fill=NA, size=.3),
               panel.grid.major = element_line(size = 0.05),
               panel.grid.minor = element_blank(),
               panel.spacing = unit(.35, "line"),
               legend.position = "bottom", 
               legend.key.size = unit(.8, "line"),
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
           xlab("precip. group") +
           ylab(y_lab) + 
           scale_fill_manual(values = color_ord,
                             labels = time_label)
  return(box_p)

}

cum_box_cluster_x <- function(dt, tgt_col, y_lab){
  dt <- dt %>% 
        filter(time_period != "1950-2005" & time_period != "2006-2025") %>% 
        data.table()

  cluster_label <- c(4, 3, 2, 1)
  categ_label <- c("most precip", "less precip", 
                   "lesser precip", "least precip")
  time_label <- c(# "1950-2005", 
                  "1979-2016", # "2006-2025",
                  "2026-2050",
                  "2051-2075", "2076-2099")

  color_ord = c("grey47", "dodgerblue2", "olivedrab4", "gold")
  # color_ord = c("grey47", "dodgerblue2", "olivedrab4", "red", "blue3", "gold")
  
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
           ylab(y_lab) + 
           scale_fill_manual(values = color_ord,
                             labels = time_label)
  
  return(box_p)
}

cum_clust_box_plots <- function(dt, tgt_col, y_lab){
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

  # dt <- dt %>% filter(get(tgt_col) >= 0)%>% data.table()
  
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
           ylab(y_lab) + 
           scale_fill_manual(values = color_ord,
                             name = "precip\nlevel", 
                             labels = categ_label)
           
  return(box_p)
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


