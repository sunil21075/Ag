rm(list=ls())
library(data.table)
library(rgdal)
library(dplyr)
library(sp)
# library(sf)
library(foreign)

source_1 = "/Users/hn/Documents/00_GitHub/Ag/remote_sensing/remote_core.R"
source(source_1)
options(digits=9)
options(digit=9)
##########
########## Directories
##########
data_dir <- paste0("/Users/hn/Documents/01_research_data/", 
                   "remote_sensing/00_shapeFiles/02_correct_years/", 
                   "05_filtered_shapefiles/Grant/all_fields/Grant_2017/")

Grant2017 <- readOGR(paste0(data_dir, "/Grant_2017.shp"),
                     layer = "Grant_2017", 
                     GDAL1_integer64_policy = TRUE)

############################################################################
#######
####### 
#######

first_Id = Grant2017@data$ID[1]
samples = Grant2017[Grant2017@data$ID == first_Id, ]

for (plt in unique(Grant2017@data$CropTyp)){
  curr_sp <- Grant2017[Grant2017@data$CropTyp == plt, ]
  n <- min(100, nrow(curr_sp@data))
  curr_sp <- curr_sp[1:n, ]
  samples <- rbind(samples, curr_sp)

}


write_dir <- paste0("/Users/hn/Documents/01_research_data", 
                   "/remote_sensing/00_shapeFiles/02_correct_years/", 
                   "05_filtered_shapefiles/Grant2017_samples_4_Kirti_plot/")
if (dir.exists(file.path(write_dir)) == F){
  dir.create(path=file.path(write_dir), recursive=T)
}

writeOGR(obj = samples,
         dsn = write_dir, 
         layer="Grant2017_samples_4_Kirti_plot", 
         driver="ESRI Shapefile")





