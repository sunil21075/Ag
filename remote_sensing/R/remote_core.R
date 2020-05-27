
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
toss_Nass <- function(sfe){
  sfe <- sfe[sfe@data$DataSrc != "NASS", ]
  return(sfe)
}

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

filter_lastSrvyDate <- function(dt, year){
  dt_LstSrvD <- dt[grepl(as.character(year), dt$LstSrvD), ]
  return(dt_LstSrvD)
}



filter_double_by_Notes <- function(dt){
  dt$Notes <- tolower(dt$Notes)
  dt_doube_by_notes <- dt[grepl('double', dt$Notes), ]
  dt_dbl_by_notes <- dt[grepl('dbl', dt$Notes), ]

  return(rbind(dt_doube_by_notes, dt_dbl_by_notes))
}

filter_out_non_irrigated_datatable <- function(dt){
  dt <- data.table(dt)
  dt <- dt %>% 
        filter(Irrigtn != "Unknown") %>% 
        data.table()
  
}

filter_out_non_irrigated_shapefile <- function (dt){
  dt <- dt[!grepl('None', dt$Irrigtn), ] # toss out those with None in irrigation
  dt <- dt[!grepl('Unknown', dt$Irrigtn), ] # toss out Unknown
  return(dt)

}


pick_eastern_counties <- function(sff){
  Okanogan <- sff[grepl('Okanogan', sff$county), ]
  Chelan <- sff[grepl('Chelan', sff$county), ]

  Kittitas <- sff[grepl('Kittitas', sff$county), ]
  Yakima <- sff[grepl('Yakima', sff$county), ]

  Klickitat <- sff[grepl('Klickitat', sff$county), ]
  Douglas <- sff[grepl('Douglas', sff$county), ]

  Grant <- sff[grepl('Grant', sff$county), ]
  Benton <- sff[grepl('Benton', sff$county), ]

  Ferry <- sff[grepl('Ferry', sff$county), ]
  Lincoln <- sff[grepl('Lincoln', sff$county), ]

  Adams <- sff[grepl('Adams', sff$county), ]
  Franklin <- sff[grepl('Franklin', sff$county), ]

  Walla_Walla <- sff[grepl('Walla Walla', sff$county), ]
  Pend_Oreille <- sff[grepl('Pend Oreille', sff$county), ]

  Stevens <- sff[grepl('Stevens', sff$county), ]
  Spokane <- sff[grepl('Spokane', sff$county), ]

  Whitman <- sff[grepl('Whitman', sff$county), ]
  Garfield <- sff[grepl('Garfield', sff$county), ]

  Columbia <- sff[grepl('Columbia', sff$county), ]
  Asotin <- sff[grepl('Asotin', sff$county), ]

  sff <- raster::bind(Okanogan, Chelan, Kittitas, Yakima, Klickitat, 
                      Douglas, Benton, Ferry, Lincoln, Adams, Franklin,
                      Walla_Walla, Pend_Oreille, Stevens, Spokane, Whitman, Garfield, 
                      Columbia, Asotin, Grant)
  return(sff)

}

pick_eastern_counties_noGrant <- function(sff){
  Okanogan <- sff[grepl('Okanogan', sff$county), ]
  Chelan <- sff[grepl('Chelan', sff$county), ]

  Kittitas <- sff[grepl('Kittitas', sff$county), ]
  Yakima <- sff[grepl('Yakima', sff$county), ]

  Klickitat <- sff[grepl('Klickitat', sff$county), ]
  Douglas <- sff[grepl('Douglas', sff$county), ]

  Benton <- sff[grepl('Benton', sff$county), ]

  Ferry <- sff[grepl('Ferry', sff$county), ]
  Lincoln <- sff[grepl('Lincoln', sff$county), ]

  Adams <- sff[grepl('Adams', sff$county), ]
  Franklin <- sff[grepl('Franklin', sff$county), ]

  Walla_Walla <- sff[grepl('Walla Walla', sff$county), ]
  Pend_Oreille <- sff[grepl('Pend Oreille', sff$county), ]

  Stevens <- sff[grepl('Stevens', sff$county), ]
  Spokane <- sff[grepl('Spokane', sff$county), ]

  Whitman <- sff[grepl('Whitman', sff$county), ]
  Garfield <- sff[grepl('Garfield', sff$county), ]

  Columbia <- sff[grepl('Columbia', sff$county), ]
  Asotin <- sff[grepl('Asotin', sff$county), ]

  sff <- raster::bind(Okanogan, Chelan, Kittitas, Yakima, Klickitat, 
                      Douglas, Benton, Ferry, Lincoln, Adams, Franklin,
                      Walla_Walla, Pend_Oreille, Stevens, Spokane, Whitman, Garfield, 
                      Columbia, Asotin)
  return(sff)

}



