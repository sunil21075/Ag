################################################
#
# pick one warm location and one cold location 
# compute HOURLY temperature data.
# for all models and scenarios 
# for each month between September and 
# April (make sure you use appropriate 
# season rather than the calendar year, 
# like Matt did. eg september 2018 to april 2019 
# will be one season, and it will cross two 
# consecutive calendar years),
# find the total number of hours the hourly 
# temperature is in the following ranges
# (-inf, -2)
# (-2, 4)
# (4, 6)
# (6, 8)
# (8, 13)
# (13, 16)
# (16, inf)

# The make the following plot for each location and month
# A panel of model_scenario combinations (like Matt's maps) 
# with the following time series plot 

# X axis :  annual time series
# Y axis: box plot of hours in these ranges 
# Grouped by the 5 ranges above (so five trend lines in each plot)
################################################

rm(list=ls())
library(data.table)
library(dplyr)
library(ggplot2)

A = data.table(A)
A$.id = as.character(A$.id)
colnames(A)[colnames(A)==".id"] <- "ID"
A$ID = paste0(unlist(strsplit(A$ID, split="_"))[4], unlist(strsplit(A$ID, split="_"))[5])
A$ID = unlist(strsplit(A$ID, split=".txt"))
# What is the ID for? just drop it!



