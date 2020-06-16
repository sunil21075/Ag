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

data_dir <- paste0("/Users/hn/Documents/01_research_data/remote_sensing/01_Data_part_not_filtered/")

##############################################################################

# WSDA_2018 <- readOGR(paste0(data_dir, "WSDACrop_2018/WSDACrop_2018.shp"),
#                          layer = "WSDACrop_2018", 
#                          GDAL1_integer64_policy = TRUE)

WSDA_2017 <- read.csv(paste0(data_dir, "WSDA_DataTable_2017.csv"))

WSDA_2017$CropType <- tolower(WSDA_2017$CropType)

# Eastern_2017 <- pick_eastern_counties(WSDA_2017)

Grant_2017 <- WSDA_2017 %>% 
              filter(county == "Grant") %>% 
              data.table()
dim(Grant_2017)
sum(Grant_2017$ExactAcres)

Grant_2017_surveyed2017 <- Grant_2017[grepl(as.character(2017), Grant_2017$LastSurvey), ]
dim(Grant_2017_surveyed2017) # 17413
sum(Grant_2017_surveyed2017$ExactAcres) # 790386.06


# Grant_2017_double_by_notes <- filter_double_by_Notes(Grant_2017)
# dim(Grant_2017_double_by_notes)
# sum(Grant_2017_double_by_notes$ExactAcres)


# Grant_2017_double_by_notes_surveyed2017 <- Grant_2017_double_by_notes[grepl(as.character(2017), Grant_2017_double_by_notes$LastSurvey), ]
# dim(Grant_2017_double_by_notes_surveyed2017)
# sum(Grant_2017_double_by_notes_surveyed2017$ExactAcres)

########
######## Filter out non-irrigated
########

# for sanity again
Grant_2017 <- WSDA_2017 %>% 
              filter(county == "Grant") %>% 
              data.table()
dim(Grant_2017)

Grant_2017_Irrigated <- filter_out_non_irrigated_datatable(Grant_2017)
dim(Grant_2017_Irrigated) # 13793
sum(Grant_2017_Irrigated$ExactAcres) # 521766.7

Grant2017_Irrigated_surveyed2017 <- Grant_2017_Irrigated[grepl(as.character(2017), Grant_2017_Irrigated$LastSurvey), ]
dim(Grant2017_Irrigated_surveyed2017)
sum(Grant2017_Irrigated_surveyed2017$ExactAcres)

Grant_2017_Irrigated_doubleNotes <- filter_double_by_Notes(Grant_2017_Irrigated)
dim(Grant_2017_Irrigated_doubleNotes)
sum(Grant_2017_Irrigated_doubleNotes$ExactAcres)


Grant2017_Irrigated_surveyed2017_doubleNotes <- filter_double_by_Notes(Grant2017_Irrigated_surveyed2017)
dim(Grant2017_Irrigated_surveyed2017_doubleNotes)
sum(Grant2017_Irrigated_surveyed2017_doubleNotes$ExactAcres)

########
######## Perennials subseciton
########
param_dir <- "/Users/hn/Documents/00_GitHub/Ag/remote_sensing/parameters/"
double_crop_pots <- read.csv(paste0(param_dir, "double_crop_potential_plants.csv"))

Grant_2017_Irrigated_poten <- Grant_2017_Irrigated %>% 
                              filter(CropType %in% double_crop_pots$Crop_Type)

dim(Grant_2017_Irrigated_poten) # 4575
sum(Grant_2017_Irrigated_poten$ExactAcres) # 256314.4

Grant_2017_Irrigated_poten_surveyed_2017 <- Grant_2017_Irrigated_poten[grepl(as.character(2017), Grant_2017_Irrigated_poten$LastSurvey), ]
dim(Grant_2017_Irrigated_poten_surveyed_2017) # 4575

Grant_2017_Irrigated_poten_surveyed_2017_double <- filter_double_by_Notes(Grant_2017_Irrigated_poten_surveyed_2017)
dim(Grant_2017_Irrigated_poten_surveyed_2017_double) # 241
sum(Grant_2017_Irrigated_poten_surveyed_2017_double$ExactAcres) # 16411.14

########
########   Toss out NASS and repeat everything again
########
Grant_2017 <- WSDA_2017 %>% 
              filter(county == "Grant") %>% 
              data.table()

Grant_2017 <- Grant_2017 %>%
              filter( DataSource != "NASS") %>%
              data.table()

dim(Grant_2017) # 9781
sum(Grant_2017$ExactAcres) # 309449.6

Grant_2017_surveyed2017 <- Grant_2017[grepl(as.character(2017), Grant_2017$LastSurvey), ]
dim(Grant_2017_surveyed2017) # 9514
sum(Grant_2017_surveyed2017$ExactAcres) # 305797.06


Grant_2017_Irrigated <- filter_out_non_irrigated_datatable(Grant_2017)
dim(Grant_2017_Irrigated) # 9200
sum(Grant_2017_Irrigated$ExactAcres) # 296994.6



Grant2017_Irrigated_surveyed2017 <- Grant_2017_Irrigated[grepl(as.character(2017), Grant_2017_Irrigated$LastSurvey), ]
dim(Grant2017_Irrigated_surveyed2017) # 9175
sum(Grant2017_Irrigated_surveyed2017$ExactAcres) # 296713.7


Grant_2017_Irrigated_doubleNotes <- filter_double_by_Notes(Grant_2017_Irrigated)
dim(Grant_2017_Irrigated_doubleNotes) # 115
sum(Grant_2017_Irrigated_doubleNotes$ExactAcres) # 9370.81388


Grant2017_Irrigated_surveyed2017_doubleNotes <- filter_double_by_Notes(Grant2017_Irrigated_surveyed2017)
dim(Grant2017_Irrigated_surveyed2017_doubleNotes)
sum(Grant2017_Irrigated_surveyed2017_doubleNotes$ExactAcres)


Grant_2017_Irrigated_poten <- Grant_2017_Irrigated %>% 
                              filter(CropType %in% double_crop_pots$Crop_Type)

dim(Grant_2017_Irrigated_poten) # 2248
sum(Grant_2017_Irrigated_poten$ExactAcres) # 


Grant_2017_Irrigated_poten_surveyed_2017 <- Grant_2017_Irrigated_poten[grepl(as.character(2017), Grant_2017_Irrigated_poten$LastSurvey), ]
dim(Grant_2017_Irrigated_poten_surveyed_2017) # 2248


Grant_2017_Irrigated_poten_surveyed_2017_double <- filter_double_by_Notes(Grant_2017_Irrigated_poten_surveyed_2017)
dim(Grant_2017_Irrigated_poten_surveyed_2017_double) # 76
sum(Grant_2017_Irrigated_poten_surveyed_2017_double$ExactAcres) # 7028.4



