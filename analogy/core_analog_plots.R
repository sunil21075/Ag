
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
plot_f_h_2_features_all_models <- function(future_dt, top3_data, hist_dt){
  # input: future_dt: future features for one fip (target county), one time period, all models
  #        top3_data: data table including top three similar analogs of given county.
  #        hist_dt: all historical data
  # output: scatter plot of both of these in the same plot
  all_models <- sort(unique(future_dt$model))

  for (a_model in all_models){

    ## pick up a model, and its corresponding analog
    # future
    future_dt_a_model <- future_dt %>% filter(model == a_model) %>% data.table()
    
    # History
    most_similar_county_fip <- top3_data$top_1_fip[top3_data$model == a_model]
    hist_dt_curr <- hist_dt %>% filter(fips == most_similar_county_fip) %>% data.table()
    
    assign(x= paste0("plot_", gsub("-", "_", a_model)),
           value = {plot_f_h_2_features_1_model(future_dt_a_model, hist_dt_curr)})
  }
  assign(x = "plot" , 
         value={ggarrange(plotlist = list(plot_bcc_csm1_1_m, 
                                          plot_BNU_ESM, 
                                          plot_CanESM2,
                                          plot_CNRM_CM5, 
                                          plot_GFDL_ESM2G, 
                                          plot_GFDL_ESM2M
                                          ),
                          ncol = 1, nrow = length(all_models), 
                          common.legend = TRUE, 
                          legend = "bottom")})
  return(plot)
}

plot_f_h_2_features_1_model <- function(f_data, hist_data){
  # input: future_dt: future features for one fip (target county), one time period, one model
  #        top3_data: data table including top three similar analogs of given county.
  #        hist_dt: all historical data
  # output: scatter plot of both of these in the same plot
  
  # extract model name for use in title
  model <- unique(f_data$model)
  time_frame <- unique(f_data$time_period)
  target_county_name <- unique(f_data$st_county)
  analog_county_name <- unique(hist_data$st_county)

  target_county_name <- paste0(unlist(strsplit(target_county_name, "_"))[2], ", ",
                               unlist(strsplit(target_county_name, "_"))[1])
  analog_county_name <- paste0(unlist(strsplit(analog_county_name, "_"))[2], ", ",
                               unlist(strsplit(analog_county_name, "_"))[1])

  mini_inf <- paste0(" (", time_frame, ", ", model, ")")
  plt_title <- paste0(target_county_name, mini_inf)
  plt_subtitle <- paste0("Analog: ", analog_county_name, " (1979-2015, observed)")
  plot_data <- rbind(hist_data, f_data)

  the_theme <- theme(plot.title = element_text(size = 30, face="bold", color="black"),
                     plot.subtitle = element_text(size = 26, face="plain", color="black"),
                     axis.text.x = element_text(size = 20, face = "bold", color="black"),
                     axis.text.y = element_text(size = 20, face = "bold", color="black"),
                     axis.title.x = element_text(size = 30, face = "bold", color="black", 
                                                 margin = margin(t=8, r=0, b=8, l=0)),
                     axis.title.y = element_text(size = 30, face = "bold", color="black",
                                                 margin = margin(t=0, r=8, b=0, l=0)),
                     strip.text = element_text(size=30, face = "bold"),
                     legend.margin=margin(t=.1, r=0, b=0, l=0, unit='cm'),
                     legend.title = element_blank(),
                     legend.position="bottom", 
                     legend.key.size = unit(1.5, "line"),
                     legend.spacing.x = unit(.05, 'cm'),
                     legend.text=element_text(size=12),
                     panel.spacing.x =unit(.75, "cm"))

  plt <- ggplot(data = plot_data) +
         geom_point(aes(x = CumDDinF_Aug23, y = yearly_precip, fill = time_period),
                    alpha = .5, shape = 21, size=9) +
         ylab("annual precipitation (mm)") +
         xlab("Cum. DD (F) by Aug 23") + 
         ggtitle(label = plt_title,
                 subtitle = plt_subtitle) + 
         guides(colour = guide_legend(override.aes = list(size=100))) + 
         the_theme
  return(plt)
}


plot_the_map <- function(a_dt, county2, title_p, 
                         target_county_map_info, 
                         most_similar_cnty_map_info){
  curr_plot <- ggplot(a_dt, aes(long, lat, group = group)) + 
               geom_polygon(data = county2, fill="lightgrey") +
               geom_polygon(data = most_similar_cnty_map_info, color="red", size = 1) +
               geom_polygon(data = target_county_map_info, color="yellow", size = .75) +
               geom_polygon(aes(fill = analog_freq), colour = rgb(1, 1, .11, .2), size = .01)+
               coord_quickmap() + 
               theme(plot.title = element_text(size=20, face="bold"),
                     plot.margin = unit(c(t=.5, r=0.1, b= -2, l=0.1), "cm"),
                     legend.title = element_blank(),
                     legend.position = "bottom",
                     legend.key.size = unit(3.3, "line"),
                     legend.text = element_text(size=10, face="bold"),
                     legend.margin = margin(t=.5, r=0, b=1, l=0, unit = 'cm'),
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
        theme(plot.title = element_text(size=20, face="bold"), 
              plot.margin = unit(c(t=-2, r=0, b=.5, l=0), "cm"),
              panel.grid=element_blank(),
              legend.spacing.x = unit(.2, 'cm'),
              legend.title = element_blank(),
              legend.position = "bottom",
              legend.key.size = unit(1.6, "line"),
              legend.text = element_text(size=26)) +
        theme(axis.text=element_blank()) + 
        theme(axis.title=element_blank()) + 
        theme(axis.ticks = element_blank()) +
        labs(title=titl) + 
        annotate("text", x = 0, y = 0, colour = "red", size = 8,
                 label = paste0(as.integer(DT[1,2]), "/", as.integer(DT[1,2] + DT[2,2]), 
                         "\n",
                         "most similar to ", "\n", 
                         subtitle)) 
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






