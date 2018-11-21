#!/share/apps/R-3.4.3_gcc/bin/Rscript

library(MESS,       lib.loc="/home/hnoorazar/R/R_libs/")
library(dplyr,      lib.loc="/home/hnoorazar/R/R_libs/")
library(chron,      lib.loc="/home/hnoorazar/R/R_libs/")
library(geepack,    lib.loc="/home/hnoorazar/R/R_libs/")
library(data.table, lib.loc="/home/hnoorazar/R/R_libs/")

library(ggplot2, lib.loc="/share/apps/R-3.2.2_gcc/lib64/R/library/")


source_path1 = "/home/hnoorazar/cleaner_codes/core.R"
source_path2 = "/home/hnoorazar/cleaner_codes/core_plot.R"
source(source_path1)
source(source_path2)

input_dir = "/data/hydro/users/Hossein/codling_moth_new/local/processed/"
plot_path = "/data/hydro/users/Hossein/codling_moth_new/local/processed/plots_local_2015/"
color_ord = c("grey70", "dodgerblue", "olivedrab4", "red")

stages = c("Larva", "Adult")
dead_lines = c("Aug", "Nov")
versions = c("rcp45", "rcp85")

file_pref = "generations_" 
file_mid = "_combined_CMPOP_"
file_end = ".rds"

for (dead_line in dead_lines){
     for (version in versions){
          file_name = paste0(file_pref, dead_line, file_mid)
          file_name = paste0(file_name, version, file_end)
          for (stage in stages){
               plot_No_generations(input_dir,
                                   file_name,
                                   stage,
                                   dead_line = dead_line,
                                   box_width=.25,
                                   plot_path,
                                   version=version,
                                   color_ord)
         }
     }
 }