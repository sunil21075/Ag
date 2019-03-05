.libPaths("/data/hydro/R_libs35")
.libPaths()
library(data.table)
library(dplyr)
library(geepack)
library(chron)

source_path = "/home/hnoorazar/analog_codes/core_analog.R"
source(source_path)

options(digits=9)
options(digit=9)

param_dir = "/home/hnoorazar/cleaner_codes/parameters/local/data_bases/"
main_in_dir <- "/data/hydro/users/Hossein/analog/"
main_out_dir <- "/data/hydro/users/Hossein/analog/local/ready_features/"

##################################################################
##
##            Terminal Arguments
##
##################################################################
args = commandArgs(trailingOnly=TRUE)
carbon_type = args[1] # either rcp45 or rcp85

##################################################################


#### Create First Flight Median Day of Year

data_45 <- data.table(readRDS(paste0("main_in_dir", "short_combined_CM_". carbon_type, ".rds")))
FF_45 <- generate_mDoY_FF(data_45)

print ("dim(FF_45)")
print (dim(FF_45))
print ("_________________________________")

##
## Create No. of Generations. We only want Larva by Aug 23.
## There are 19 models, hence, we have to take averages of number of generations
##
gen_45 <- data.table(readRDS(paste0("main_in_dir", "generations_rcp45.rds")))
gen_45 <- subset(gen_45, select = c("year", "location", "NumLarvaGens_Aug"))

gen_45 <- gen_45 %>% 
          group_by() %>% 
          summarise_at(vars(NumLarvaGens_Aug), funs(mean(., na.rm=TRUE)))

print (dim(gen_45))
print ("_________________________________")












