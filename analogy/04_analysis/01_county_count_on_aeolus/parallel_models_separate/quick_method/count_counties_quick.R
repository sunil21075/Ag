
.libPaths("/data/hydro/R_libs35")
.libPaths()
library(lubridate)
library(purrr)
library(tidyverse)
library(data.table)
library(dplyr)

source_path = "/home/hnoorazar/analog_codes/core_analog.R"
source(source_path)

options(digit=9)
options(digits=9)

# Check current folder
print("does this look right?")
getwd()
start_time <- Sys.time()

######################################################################
##                                                                  ##
##                     Terminal arguments                           ##
##                                                                  ##
######################################################################

args = commandArgs(trailingOnly=TRUE)
carbon_type= args[1] # rcp45 or rcp85
precip_type= args[2] # include precip or no_precip
sigma_bd= args[3]    # sigma cut off for sigma dissimilarity 1 or 2 or 3 or what?
all_model_names = args[4]

n_nghs = 47841
######################################################################
##                                                                  ##
##                     set up directories                           ##
##                                                                  ##
######################################################################

main_in <- file.path("/data/hydro/users/Hossein/analog/03_analogs/location_level/w_gen3/")
dt_dir <- file.path(main_in, precip_type, n_nghs, carbon_type, "merged/")

main_out <- file.path("/data/hydro/users/Hossein/analog/04_analysis/")
out_dir <- file.path(main_out, precip_type, n_nghs, "/quick/")

if (dir.exists(out_dir) == F) { dir.create(path = out_dir, recursive = T) }
print (out_dir)

param_dir <- "/home/hnoorazar/analog_codes/parameters/"

######################################################################
##                                                                  ##
##                          read files                              ##
##                                                                  ##
######################################################################

all_model_names <- c("bcc-csm1-1-m", "BNU-ESM", "CanESM2", "CNRM-CM5", "GFDL-ESM2G", "GFDL-ESM2M")
time_periods <- c("_2026_2050", "_2051_2075", "_2076_2095")

for (model_type in all_model_names){
  for (time in time_periods){
    print (paste0(time, " - ", model_type))
    st_time <- Sys.time()
    NNs_name <- paste0(dt_dir, "/NN_loc_year_tb_", model_type, time, ".rds")
    # dist_name <- paste0(dt_dir, "/NN_dist_tb_", model_type, time,  ".rds")
    sigma_name <- paste0(dt_dir, "/NN_sigma_tb_", model_type, time, ".rds")

    NNs <- data.table(readRDS(NNs_name))
    sigmas <- data.table(readRDS(sigma_name))

    print (dim(NNs))
    # print (dim(dists))
    print (dim(sigmas))
    
    county_list <- data.table(read.csv(paste0(param_dir, "/Min_fips_st_county_location.csv"), 
                                       header=T, sep=",", as.is=T)
                              )
    print ("line 76")
    a_model_analog_output <- count_analogs_counties_quick(NNs, sigmas, county_list, sigma_bd=sigma_bd)
    a_model_novel_output <- count_novel_quick(NNs, sigmas, county_list, novel_bd=4)


    a_model_analog_output$model <- model_type
    a_model_novel_output$model <- model_type

    saveRDS(a_model_analog_output,paste0(out_dir, "analog_",model_type, "_", carbon_type, time, ".rds"))
    saveRDS(a_model_novel_output, paste0(out_dir, "novel_", model_type, "_", carbon_type, time, ".rds"))
    print (Sys.time() - st_time)
  }  
}











