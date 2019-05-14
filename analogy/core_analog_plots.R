
library(data.table)
library(dplyr)
library(ggmap)
library(ggpubr)

options(digits=9)
options(digit=9)
########################################################################################################
#################
#################    Donut functions
#################
########################################################################################################
plot_the_map <- function(a_dt, county2, title_p, 
                         target_county_map_info, 
                         most_similar_cnty_map_info){
  curr_plot <- ggplot(a_dt, aes(long, lat, group = group)) + 
               geom_polygon(data = county2, fill="lightgrey") +
               geom_polygon(data = most_similar_cnty_F1_map_info, color="red", size = 1) +
               geom_polygon(data = target_county_map_info, color="yellow", size = 1) +
               geom_polygon(aes(fill = analog_freq), colour = rgb(1, 1, .11, .2), size = .01)+
               coord_quickmap() + 
               theme(legend.title = element_blank(),
                     axis.text.x = element_blank(),
                     axis.text.y = element_blank(),
                     axis.ticks.x = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title.x = element_blank(),
                     axis.title.y = element_blank()) + 
               ggtitle(title_p)
   return(curr_plot) 
}

plot_the_pie <- function(DT, titl, subtitle){
  pp <- ggplot(DT, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
        geom_rect(colour="grey30") +
        coord_polar(theta="y") +
        xlim(c(0, 4)) +
        theme(plot.title = element_text(size=16, face="bold"), 
              panel.grid=element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              legend.spacing.x = unit(.2, 'cm'),
              legend.title = element_blank(),
              legend.position = "bottom",
              legend.key.size = unit(1.6, "line"),
              legend.text = element_text(size=26)) +
        labs(title=titl) + 
        annotate("text", x = 0, y = 0, 
                 label = paste0(as.integer(DT[1,2]), "/", as.integer(DT[1,2] + DT[2,2]), 
                         "\n",
                         "most similar to ", subtitle)) 
  return(pp)
}


plot_the_pie_all_possible <- function(dat, titl){
  pp <- ggplot(DT, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
        geom_rect(colour="grey30") +
        coord_polar(theta="y") +
        xlim(c(0, 4)) +
        theme(plot.title = element_text(size=16, face="bold"), 
              panel.grid=element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              legend.spacing.x = unit(.2, 'cm'),
              legend.title = element_blank(),
              legend.position = "bottom",
              legend.key.size = unit(1.6, "line"),
              legend.text = element_text(size=26)) +
        labs(title=titl) + 
        annotate("text", x = 0, y = 0, 
                 label = paste(as.integer(DT[1,2]), as.integer(DT[1,2] + DT[2,2]), sep="/"))
  return(pp)
}

plot_the_pie_Q2 <- function(dat, titl){
  pp <- ggplot(DT, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
        geom_rect(colour="grey30") +
        coord_polar(theta="y") +
        xlim(c(0, 4)) +
        theme(plot.title = element_text(size=16, face="bold"), 
              panel.grid=element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              legend.spacing.x = unit(.2, 'cm'),
              legend.title = element_blank(),
              legend.position = "bottom",
              legend.key.size = unit(1.6, "line"),
              legend.text = element_text(size=26)) +
        labs(title=titl) + 
        annotate("text", x = 0, y = 0, 
                 label = paste(as.integer(DT[1,2]), as.integer(DT[1,2] + DT[2,2]), sep="/")) 
  return(pp)
}

plot_the_pie_Q3 <- function(dat, titl){
  pp <- ggplot(DT, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
        geom_rect(colour="grey30") +
        coord_polar(theta="y") +
        xlim(c(0, 4)) +
        theme(plot.title = element_text(size=16, face="bold"), 
              panel.grid=element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              legend.spacing.x = unit(.2, 'cm'),
              legend.title = element_blank(),
              legend.position = "bottom",
              legend.key.size = unit(1.6, "line"),
              legend.text = element_text(size=26)) +
        labs(title=titl) + 
        annotate("text", x = 0, y = 0, 
                 label = paste(as.integer(DT[1,2]), as.integer(DT[1,2] + DT[2,2]), sep="/")) 
  return(pp)
}


########################################################################################################
########################################################################################################
########################################################################################################

plot_100_NN_geo_map <- function(NNs, dists, sigmas, use_sigma=T){
  # For a given location, i.e. a vector,
  # plot the geographical map of 100 NNs
  # based on color
  # input: NNs: data frame of nearest neighbors
  #      dists: distances to the location of interest
  #     sigmas: sigma_dissimilarity between location of interest and other locations
  #  use_sigma: Wheter to use sigma_diss or distances as color codes
  # 
  # output: geographical map of ONE location of interest and its analogs
  #
  year_of_int <- NNs$year
  location_of_int <- NNs$location
  location_of_int <- c(unlist(strsplit(location_of_int, "_"))[1], unlist(strsplit(location_of_int, "_"))[2])
  location_of_int <- as.numeric(location_of_int)
  
  analogs <- NNs[, seq(2, ncol(NNs_int), 2)]
  
  analogs <- within(analogs, remove(location))
  dists <- within(dists, remove(year, location))
  sigmas <- within(sigmas, remove(year, location))

  x <- sapply(analogs, function(x) strsplit(x, "_")[[1]], USE.NAMES=FALSE)
  lat = x[1, ]
  long = x[2, ]
  
  dt <- setNames(data.table(matrix(nrow = length(sigmas), ncol = 4)), 
                            c("lat", "long", "distances", "sigmas"))
  
  dt$lat = as.numeric(lat)
  dt$long = as.numeric(long)
  dt$distances = as.numeric(dists)
  dt$sigmas = as.numeric(sigmas)

  
  states <- map_data("state")
}






