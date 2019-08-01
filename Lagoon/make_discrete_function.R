
require(ggplot2)
require(cowplot)

values <- c(0, 1, 2, 5, 10) # this vector is needed not only for the data frame cbar, but also for plotting
group <- letters[1:5]
diff_values <- c(0, diff(values))
cbar_df <- data.frame(x = 1, y = values, diff_values, group,  stringsAsFactors = FALSE)
# that's for the fake legend

iris2 <- iris #don't want to mess with your iris data set
              #I used iris because you hadn't provided data
iris2$cuts <- cut(iris2$Petal.Length, values) #the already offered 'cut-approach' 

p1  <- ggplot(iris2, aes(Sepal.Length, y = Sepal.Width, color = cuts))+ 
          geom_point() +
          scale_color_brewer("", palette = "Reds")

cbar_plot <- ggplot(cbar_df, aes(x, y = diff_values, fill = c(NA, rev(group[2:5])))) + 
  # I had to do implement this 'fill=' workaround 
  # in creating a new vector introducing an NA, 
  # and I had to flip the fills in order to fit to the scale... 
    geom_col(width = 0.1, show.legend = FALSE)  +
    geom_segment(y = values, yend = values, x = 0.9, xend = 1.05) +
    annotate(geom = 'text', x = 0.85, y = values, label = values) +
  # the numbers are quasi-randomly chosen but define the length of your ticks, obviously
    scale_x_continuous(expand = c(1,1)) + 
  # you might need to play around with the expand argument for the width of your legend
    scale_fill_brewer("", palette = "Reds", direction = -1) +  
  # don't know why you have to flip this again... 
    coord_flip() +
    theme_void()

plot_grid(p1, cbar_plot, nrow = 2)








geo_map_of_diffs_discrete_cuts <- function(dt, col_col, ttl, subttl){
  disc_colors <- c("white", "lightskyblue","deepskyblue",
                   "dodgeblue3", "dodgerblue4", "blue4")
  x <- sapply(dt$location,
              function(x) strsplit(x, "_")[[1]], 
              USE.NAMES=FALSE)

  lat <- as.numeric(x[1, ]); long <- as.numeric(x[2, ])
  dt$lat <- lat; dt$long <- long;
  
  states <- map_data("state")
  states_cluster <- subset(states, 
                           region %in% c("washington"))
  WA_counties <- map_data("county", "washington")
  
  dt %>%
  ggplot() +
  geom_polygon(data = states_cluster, 
               aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  geom_polygon(data=WA_counties, 
               aes(x=long, y=lat, group = group), 
               fill = NA, colour = "grey60", size=.3) + 
  geom_point(aes_string(x = "long", y = "lat", color = col_col), 
             alpha = 1, size=.3) +
  scale_color_manual(name = "qsec",
                     values = c("(-Inf,-0.5]"= "red4",
                                "(-0.5,-0.2]" = "red",
                                "(-0.2,-0.15]" = "brown1",
                                "(-0.15,-0.1]" = "tomato1",
                                "(-0.1,-0.05]" = "hotpink1",
                                "(-0.05,0]" = "white",
                                "(0,0.05]"  = "white",
                                "(0.05,0.1]" = "yellow",
                                "(0.1,0.15]" = "deepskyblue",
                                "(0.15,0.2]" = "dodgerblue3",
                                "(0.2,0.5]"  = "dodgerblue4",
                                "(0.5, Inf]"  = "blue4"
                                ),
                     # labels = c("<= -50%", 
                     #            "-50% < . <= -20%",
                     #            "-20% < . <= -15%",
                     #            "-15% < . <= -10%",
                     #            "-10% < . <= -5%",
                     #            "-5% < . <= 0%",
                     #            "0% < . <= 5%",
                     #            "5% < . <= 10%",
                     #            "10% < . <= 15%",
                     #            "15% < . <= 20%",
                     #            "20% < . <= 50%",
                     #            ">= 50%"
                     #            )
                     )+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 10, face="plain"),
        legend.title = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size=14, face="bold")) +  
  guides(colour = guide_legend(override.aes = list(size=2))) +
  ggtitle(ttl, subtitle=subttl)
  
}


geo_map_of_diffs <- function(dt, col_col, minn, maxx, ttl, subttl){
  color_limit <- max(abs(minn), abs(maxx))
  x <- sapply(dt$location, 
              function(x) strsplit(x, "_")[[1]], 
              USE.NAMES=FALSE)
  lat <- as.numeric(x[1, ]); long <- as.numeric(x[2, ])
  dt$lat <- lat; dt$long <- long;
  
  states <- map_data("state")
  states_cluster <- subset(states, 
                           region %in% c("washington"))
  WA_counties <- map_data("county", "washington")
  
  dt %>%
  ggplot() +
  geom_polygon(data = states_cluster, 
               aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  geom_polygon(data=WA_counties, 
               aes(x=long, y=lat, group = group), 
               fill = NA, colour = "grey60", size=.3) + 
  geom_point(aes_string(x = "long", y = "lat", color = col_col), 
             alpha = 1, size=.3) +
  guides(fill = guide_colourbar(barwidth = .1, barheight = 20))+
  # scale_color_viridis_c(option = "plasma", 
  #                       name = "storm", direction = -1,
  #                       limits = c(min, max),
  #                       # begin = 0.5, end = 1,
  #                       breaks = pretty_breaks(n = 3)) +
  
  # scale_color_gradient2(breaks = c((as.integer(minn*0.6)), 
  #                                   0,
  #                                   (as.integer(maxx*0.9)), 
  #                                   (as.integer((maxx)*0.9))),
                        
  #                       labels = c((as.integer(minn*0.6)), 
  #                                  0, 
  #                                  (as.integer(maxx*0.9)),
  #                                  (as.integer((maxx)*0.9))),

  #                       low = "red", high = "blue", mid = "white",
  #                       space="Lab"
  #                       ) +
  scale_color_gradient2(midpoint = 0, mid = "white", 
                        high = muted("blue"), low = muted("red"), 
                        guide = "colourbar", space = "Lab",
                        limit = c(-color_limit, color_limit)) + 
  # scale_color_continuous(breaks = c(as.integer(minn+1), 0, as.integer(maxx-1)),
  #                        labels = c(as.integer(minn+1), 0, as.integer(maxx-1)),
  #                        low = "red", high = "blue") + 
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 8, face="plain"),
        legend.title = element_blank(),
        # legend.justification = c(.93, .9),
        # legend.position = c(.93, .9),
        legend.position = "top",
        strip.text = element_text(size=14, face="bold"))+
  ggtitle(ttl, subtitle=subttl)
  
}



make_percentage_column_discrete <- function(dta){
  tps <- unique(dta$time_period)
  emissions <- c("RCP 4.5", "RCP 8.5")
  result <- data.table()
  for (tp in tps){
    for (em in emissions){
      print (em)
      print(tp)
      dt <- dta %>% filter(emission == em & time_period==tp) %>% data.table()
      minn <- min(dt$perc_diff)
      maxx <- max(dt$perc_diff)
      # c(0, 0.01, 0.02, 0.03, 0.05, 0.1, 0.15, 0.20, 0.40, 0.80)
      dt$perc_diff <- dt$perc_diff/100

      dt <- dt %>% 
            mutate(tagt_perc = case_when((perc_diff >= 0   & perc_diff <= 0.05) ~ 5,
                                         (perc_diff > 0.05 & perc_diff <= 0.1)  ~ 10,
                                         (perc_diff > 0.1  & perc_diff <= 0.15) ~ 15,
                                         (perc_diff > 0.15 & perc_diff <= 0.2)  ~ 20,
                                         (perc_diff > 0.2  & perc_diff <= 0.5)  ~ 50,
                                         (perc_diff > 0.5) ~ 100,
                                         (perc_diff < 0     & perc_diff >= -0.05) ~ -5,
                                         (perc_diff < -0.05 & perc_diff >= -0.1)  ~ -10,
                                         (perc_diff < -0.1  & perc_diff >= -0.15) ~ -15,
                                         (perc_diff < -0.15 & perc_diff >= -0.2)  ~ -20,
                                         (perc_diff < -0.2  & perc_diff >= -0.5)  ~ -50,
                                         (perc_diff < -.5) ~ -100
                                         )
                  ) %>%
            data.table()
    result <- rbind(result, dt)

    }
  }
  return(result)
}