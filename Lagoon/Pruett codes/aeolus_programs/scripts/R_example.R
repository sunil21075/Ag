install.packages("tidyverse", lib = "r_lib", repos = "http://ftp.osuosl.org/pub/cran/")

library(tibble, lib.loc = "r_lib")
library(dplyr, lib.loc = "r_lib")
library(readr, lib.loc = "r_lib")

df <- tibble(set = seq(1,10,1))

write_csv(df, "test.csv")


