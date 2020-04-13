
library(data.table)
library(rgdal)
library(dplyr)
library(sp)
############################################
############################################
############################################
###########
###########     Weird ShapeFiles section
###########
############################################
############################################

add_identifier <- function(dt_df, year){
  dt_df@data <- tibble::rowid_to_column(dt_df@data, "ID")
  dt_df@data$ID <- paste0(dt_df@data$ID, "_WSDA_SF_", year)
  return(dt_df)
}

transfer_projection_to_lat_long <- function(shape_file){
  crs <- CRS("+proj=lcc 
             +lat_1=45.83333333333334 
             +lat_2=47.33333333333334 
             +lat_0=45.33333333333334 
             +lon_0=-120.5 +datum=WGS84")
  shape_file <- spTransform(shape_file, 
                            CRS("+proj=longlat +datum=WGS84"))
  return(shape_file)
}

pick_correct_year <- function(a_shape_file, year){
  year_chr <- as.character(year)
  a_shape_file <- a_shape_file[grepl(year_chr, a_shape_file$LstSrvD), ]
  a_shape_file$year <- year
  return(a_shape_file)
}

pick_proper_cols_w_notes <- function(a_shape_file){
  cols_to_keep <- c("LstSrvD", "CropGrp", "CropTyp", "ExctAcr", "county",
                    "Irrigtn", "TRS", "RtCrpTy", "Notes", "year")
  a_shape_file <- a_shape_file[, (names(a_shape_file) %in% cols_to_keep)]
  return(a_shape_file)
}

# pick_proper_cols_no_notes <- function(a_shape_file){
#   cols_to_keep <- c("LstSrvD", "CropGrp", "CropTyp", "ExctAcr", "county",
#                     "Irrigtn", "TRS", "RtCrpTy")
#   a_shape_file <- a_shape_file[, (names(a_shape_file) %in% cols_to_keep)]
#   return(a_shape_file)
# }



