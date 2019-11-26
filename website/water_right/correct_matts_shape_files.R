crs <- CRS("+proj=lcc 
           +lat_1=45.83333333333334 
           +lat_2=47.33333333333334 
           +lat_0=45.33333333333334 
           +lon_0=-120.5 +datum=WGS84")


################################
########
########
########
all_basins_sp <- spTransform(all_basins_sp, 
                             CRS("+proj=longlat +datum=WGS84")
                             )
writeOGR(obj = all_basins_sp, 
         dsn = paste0(shapefile_dir, "all_basins/"), 
         layer="all_basins", 
         driver="ESRI Shapefile")

################################
########
########
########
all_streams_sp <- spTransform(all_streams_sp, 
                             CRS("+proj=longlat +datum=WGS84")
                             )

writeOGR(obj = all_streams_sp, 
         dsn = paste0(shapefile_dir, "all_streams/"), 
         layer="all_streams", 
         driver="ESRI Shapefile")


################################
########
########
########
all_subbasins_sp <- spTransform(all_subbasins_sp, 
                             CRS("+proj=longlat +datum=WGS84")
                             )

writeOGR(obj = all_subbasins_sp, 
         dsn = paste0(shapefile_dir, "all_subbasins/"), 
         layer="all_subbasins", 
         driver="ESRI Shapefile")


