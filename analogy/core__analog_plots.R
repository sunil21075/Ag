
library(data.table)
library(dplyr)
library(ggmap)
library(ggpubr)

options(digits=9)
options(digit=9)


plot <- function()

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






