rm(list=ls())
library(data.table)
library(rgdal)
library(dplyr)
library(sp)
library(sf)
library(foreign)

source_1 = "/Users/hn/Documents/00_GitHub/Ag/remote_sensing/R/remote_core.R"
source(source_1)
options(digits=9)
options(digit=9)


##########
########## Directories
##########

data_dir <- paste0("/Users/hn/Documents/01_research_data/remote_sensing/", 
                   "00_shapeFiles/00_WSDA_original_weird_files/WSDACrop2017/")

WWA <- readOGR(paste0(data_dir, "/WWA.shp"),
               layer = "WWA", 
               GDAL1_integer64_policy = TRUE)

Palouse <- readOGR(paste0(data_dir, "/Palouse.shp"),
               layer = "Palouse", 
               GDAL1_integer64_policy = TRUE)

NE <- readOGR(paste0(data_dir, "/NE.shp"),
               layer = "NE", 
               GDAL1_integer64_policy = TRUE)

ColumbiaBasin <- readOGR(paste0(data_dir, "/ColumbiaBasin.shp"),
               layer = "ColumbiaBasin", 
               GDAL1_integer64_policy = TRUE)

Central <- readOGR(paste0(data_dir, "/Central.shp"),
               layer = "Central", 
               GDAL1_integer64_policy = TRUE)

WSDA_2017 <- raster::bind(WWA, Palouse, NE, ColumbiaBasin, Central)
########################################################################################
######
######     add the goddamn numeric identifier.
######
########################################################################################

WSDA_2017 <- add_identifier(dt_df=WSDA_2017, year="2017")

########################################################################################
setnames(WSDA_2017@data, old=c("SHAPE_Area", "SHAPE_Leng", "County", "Acres", "Irrigation"), 
                         new=c("Shap_Ar", "Shp_Lng", "county", "TotalAcres", "Irrigtn"))

WSDA_2017@data$year <- paste0("2017_shapeFile")

WSDA_2017 <- transfer_projection_to_lat_long(WSDA_2017) # 206277 rows
WSDA_2017 <- filter_out_non_irrigated_shapefile(WSDA_2017) # 81682 rows
##########
########## write TRUE shapefiles
##########

write_dir <- paste0("/Users/hn/Documents/01_research_data/remote_sensing/", 
                    "00_shapeFiles/0002_final_shapeFiles/0001_irrigated/")
if (dir.exists(file.path(write_dir)) == F){
  dir.create(path=file.path(write_dir), recursive=T)
}

writeOGR(obj = WSDA_2017, 
         dsn = paste0(write_dir, "/WSDA_2017_irrigated/"), 
         layer="WSDA_2017_irrigated", 
         driver="ESRI Shapefile")

############################################################################
############################################################################
############################################################################

############################################################################
#######
#######         Counties of interest
#######
coi = c("Okanogan", "Chelan", "Kittitas", "Yakima", "Klickitat",
        "Douglas", "Grant", "Benton", "Ferry", "Lincoln", "Adams",
        "Franklin", "Walla Walla", "Pend Oreille", "Stevens", "Spokane",
        "Whitman", "Garfield", "Columbia", "Asotin")


Okanogan <- WSDA_2017[grepl('Okanogan', WSDA_2017$county), ]
Chelan <- WSDA_2017[grepl('Chelan', WSDA_2017$county), ]

Kittitas <- WSDA_2017[grepl('Kittitas', WSDA_2017$county), ]
Yakima <- WSDA_2017[grepl('Yakima', WSDA_2017$county), ]

Klickitat <- WSDA_2017[grepl('Klickitat', WSDA_2017$county), ]
Douglas <- WSDA_2017[grepl('Douglas', WSDA_2017$county), ]

Grant <- WSDA_2017[grepl('Grant', WSDA_2017$county), ]
Benton <- WSDA_2017[grepl('Benton', WSDA_2017$county), ]

Ferry <- WSDA_2017[grepl('Ferry', WSDA_2017$county), ]
Lincoln <- WSDA_2017[grepl('Lincoln', WSDA_2017$county), ]

Adams <- WSDA_2017[grepl('Adams', WSDA_2017$county), ]
Franklin <- WSDA_2017[grepl('Franklin', WSDA_2017$county), ]

Walla_Walla <- WSDA_2017[grepl('Walla Walla', WSDA_2017$county), ]
Pend_Oreille <- WSDA_2017[grepl('Pend Oreille', WSDA_2017$county), ]

Stevens <- WSDA_2017[grepl('Stevens', WSDA_2017$county), ]
Spokane <- WSDA_2017[grepl('Spokane', WSDA_2017$county), ]

Whitman <- WSDA_2017[grepl('Whitman', WSDA_2017$county), ]
Garfield <- WSDA_2017[grepl('Garfield', WSDA_2017$county), ]

Columbia <- WSDA_2017[grepl('Columbia', WSDA_2017$county), ]
Asotin <- WSDA_2017[grepl('Asotin', WSDA_2017$county), ]

# 60271 rows
WSDA_2017 <- raster::bind(Okanogan, Chelan, Kittitas, Yakima, Klickitat, 
                          Douglas, Benton, Ferry, Lincoln, Adams, Franklin,
                          Walla_Walla, Pend_Oreille, Stevens, Spokane, Whitman, Garfield, 
                          Columbia, Asotin)

nrow(WSDA_2017)

write_dir <- paste0("/Users/hn/Documents/01_research_data/remote_sensing/", 
                    "00_shapeFiles/0002_final_shapeFiles/0002_irrigated_eastern/")
if (dir.exists(file.path(write_dir)) == F){
  dir.create(path=file.path(write_dir), recursive=T)
}

writeOGR(obj = WSDA_2017, 
         dsn = paste0(write_dir, "/WSDA_2017_irrigated_eastern_noGrant/"), 
         layer="WSDA_2017_irrigated_eastern_noGrant", 
         driver="ESRI Shapefile")



# 74064 rows
WSDA_2017 <- raster::bind(WSDA_2017, Grant)


write_dir <- paste0("/Users/hn/Documents/01_research_data/remote_sensing/", 
                    "00_shapeFiles/0002_final_shapeFiles/0002_irrigated_eastern/")
if (dir.exists(file.path(write_dir)) == F){
  dir.create(path=file.path(write_dir), recursive=T)
}

writeOGR(obj = WSDA_2017, 
         dsn = paste0(write_dir, "/WSDA_2017_irrigated_eastern/"), 
         layer="WSDA_2017_irrigated_eastern", 
         driver="ESRI Shapefile")


################################
write_dir <- paste0("/Users/hn/Documents/01_research_data/remote_sensing/", 
                    "00_shapeFiles/0002_final_shapeFiles/0003_Grant_irrigated/")
if (dir.exists(file.path(write_dir)) == F){
  dir.create(path=file.path(write_dir), recursive=T)
}

writeOGR(obj = Grant, 
         dsn = paste0(write_dir, "/Grant_2017_irrigated/"), 
         layer="Grant_2017_irrigated", 
         driver="ESRI Shapefile")



