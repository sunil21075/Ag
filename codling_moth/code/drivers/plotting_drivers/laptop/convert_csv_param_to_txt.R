rm(list=ls())
library(chron)
library(data.table)
library(reshape2)
library(dplyr)
library(foreach)
library(iterators)
library(ggplot2)
rm(list=ls())
CodlingMothparameters_5_w <- read.csv("/Users/hn/Documents/GitHub/Kirti/Codling_moth_Code/sensitivity/wider_intervals/CodlingMothparameters_5.csv")
CodlingMothparameters_5_w = within(CodlingMothparameters_5_w, remove(X))
write.table(CodlingMothparameters_5_w, "/Users/hn/Documents/GitHub/Kirti/Codling_moth_Code/sensitivity/wider_intervals/CodlingMothparameters_5_w.txt", 
            row.names = FALSE, col.names = TRUE, sep = ",")

rm(list=ls())
CodlingMothparameters_10_w <- read.csv("/Users/hn/Documents/GitHub/Kirti/Codling_moth_Code/sensitivity/wider_intervals/CodlingMothparameters_10.csv")
CodlingMothparameters_10_w = within(CodlingMothparameters_10_w, remove(X))
write.table(CodlingMothparameters_10_w, "/Users/hn/Documents/GitHub/Kirti/Codling_moth_Code/sensitivity/wider_intervals/CodlingMothparameters_10_w.txt", 
            row.names = FALSE, col.names = TRUE, sep = ",")


rm(list=ls())
CodlingMothparameters_15_w <- read.csv("/Users/hn/Documents/GitHub/Kirti/Codling_moth_Code/sensitivity/wider_intervals/CodlingMothparameters_15.csv")
CodlingMothparameters_15_w = within(CodlingMothparameters_15_w, remove(X))
write.table(CodlingMothparameters_15_w, "/Users/hn/Documents/GitHub/Kirti/Codling_moth_Code/sensitivity/wider_intervals/CodlingMothparameters_15_w.txt", 
            row.names = FALSE, col.names = TRUE, sep = ",")

rm(list=ls())
CodlingMothparameters_20_w <- read.csv("/Users/hn/Documents/GitHub/Kirti/Codling_moth_Code/sensitivity/wider_intervals/CodlingMothparameters_20.csv")
CodlingMothparameters_20_w = within(CodlingMothparameters_20_w, remove(X))
write.table(CodlingMothparameters_20_w, "/Users/hn/Documents/GitHub/Kirti/Codling_moth_Code/sensitivity/wider_intervals/CodlingMothparameters_20_w.txt", 
            row.names = FALSE, col.names = TRUE, sep = ",")


