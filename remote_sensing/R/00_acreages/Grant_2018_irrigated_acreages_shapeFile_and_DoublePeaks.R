rm(list=ls())
library(data.table)
library(rgdal)
library(dplyr)
library(sp)
# library(sf)
library(foreign)

source_1 = "/Users/hn/Documents/00_GitHub/Ag/remote_sensing/R/remote_core.R"
source(source_1)
options(digits=9)
options(digit=9)

##############################################################################

data_dir <- paste0("/Users/hn/Documents/01_research_data/", 
                   "remote_sensing/00_shapeFiles/0002_final_shapeFiles/", 
                   "0002_irrigated_eastern/Grant_irrigated_2018/")

##############################################################################

Grant2018_Irrigated <- readOGR(paste0(data_dir, "/Grant_irrigated_2018.shp"),
                               layer = "Grant_irrigated_2018", 
                               GDAL1_integer64_policy = TRUE)

Grant2018_Irrigated <- Grant2018_Irrigated@data
sum(Grant2018_Irrigated$ExctAcr)

Grant2018_Irrigated_lastSrvyDate <- Grant2018_Irrigated[grepl("2018", Grant2018_Irrigated$LstSrvD), ]
sum(Grant2018_Irrigated_lastSrvyDate$ExctAcr)

Grant2018_Irrigated_double_by_notes <- filter_double_by_Notes(Grant2018_Irrigated)
sum(Grant2018_Irrigated_double_by_notes$ExctAcr)

Grant2018_Irrigated_lastSrvyDate_double_by_notes <- filter_double_by_Notes(Grant2018_Irrigated_lastSrvyDate)
sum(Grant2018_Irrigated_lastSrvyDate_double_by_notes$ExctAcr)


Grant2018_Irrigated_double_by_notes_NASS_out <- Grant2018_Irrigated_double_by_notes %>%
                                                filter(DataSrc != "NASS") %>%
                                                data.table()
sum(Grant2018_Irrigated_double_by_notes_NASS_out$ExctAcr)




##############################################################################
#####
#####        Double peak section
##### 
##############################################################################
                    Grant_Irrigated_EVI_2018_NassIn_CorrectYears
peak_dir <- paste0("/Users/hn/Documents/01_research_data/remote_sensing/", 
                   "01_NDVI_TS/04_Irrigated_eastern_Cloud70/Grant_2018_irrigated/", 
                   "savitzky/")


peak_dir_NassIn_CorrectYears_delta_2 <- paste0(peak_dir, "Grant_Irrigated_EVI_2018_NassIn_CorrectYears/delta_0.2/")

Grant_Irrigated_EVI_2018_NassIn_CorrectYears <- read.csv(paste0(peak_dir_NassIn_CorrectYears_delta_2, 
                                                                "all_poly_and_maxs_savitzky.csv"),
                                                             as.is=TRUE)

# drop the last empty row!!!! dammit
L <- nrow(Grant_Irrigated_EVI_2018_NassIn_CorrectYears)
Grant_Irrigated_EVI_2018_NassIn_CorrectYears <- Grant_Irrigated_EVI_2018_NassIn_CorrectYears[-c(L), ]

Grant_Irrigated_EVI_2018_NassIn_CorrectYears <- within(Grant_Irrigated_EVI_2018_NassIn_CorrectYears, 
                                                         remove(geo, max_Doy,  max_value))
Grant_Irrigated_EVI_2018_NassIn_CorrectYears <- unique(Grant_Irrigated_EVI_2018_NassIn_CorrectYears)

Grant_Irrigated_EVI_2018_NassIn_CorrectYears_2peaks <- Grant_Irrigated_EVI_2018_NassIn_CorrectYears %>%
                                                       filter(max_count == 2) %>%
                                                       data.table()
sum(Grant_Irrigated_EVI_2018_NassIn_CorrectYears_2peaks$ExctAcr)

Grant_Irrigated_EVI_2018_NassIn_CorrectYears_2morepeaks <- Grant_Irrigated_EVI_2018_NassIn_CorrectYears %>%
                                                           filter(max_count >= 2) %>%
                                                           data.table()
sum(Grant_Irrigated_EVI_2018_NassIn_CorrectYears_2morepeaks$ExctAcr)

#######
Grant_Irrigated_EVI_2018_NassIn_CorrectYears_doubleNotes <- filter_double_by_Notes(Grant_Irrigated_EVI_2018_NassIn_CorrectYears)

Grant_Irrigated_EVI_2018_NassIn_CorrectYears_2peaks <- Grant_Irrigated_EVI_2018_NassIn_CorrectYears_doubleNotes %>%
                                                       filter(max_count == 2) %>%
                                                       data.table()
sum(Grant_Irrigated_EVI_2018_NassIn_CorrectYears_2peaks$ExctAcr)

Grant_Irrigated_EVI_2018_NassIn_CorrectYears_2morepeaks <- Grant_Irrigated_EVI_2018_NassIn_CorrectYears_doubleNotes %>%
                                                           filter(max_count >= 2) %>%
                                                           data.table()
sum(Grant_Irrigated_EVI_2018_NassIn_CorrectYears_2morepeaks$ExctAcr)






