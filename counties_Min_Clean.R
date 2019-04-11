


library(foreign)

counties <- read.dbf("/Users/hn/Documents/GitHub/Kirti/codling_moth/code/parameters/vic_grid_cover_conus/VICID_CO.DBF")
setnames(counties, old= colnames(counties), new= tolower(colnames(counties)))

write.csv(counties, 
          file = "/Users/hn/Documents/GitHub/Kirti/codling_moth/code/parameters/us_county_lat_long.csv", 
          row.names=FALSE)

counties_filter <- subset(counties, select= c(fips, state, county, vicclat, vicclon))
counties_filter$county = gsub("County", "", counties_filter$county)

counties_filter$st_county = paste0(counties_filter$state, "_", counties_filter$county)
counties_filter$location = paste0(counties_filter$vicclat, "_", counties_filter$vicclon)

counties_filter <- subset(counties_filter, select = c(fips, st_county, location))

write.csv(counties_filter, 
          file = "/Users/hn/Documents/GitHub/Kirti/analogy/parameters/us_fips_st_county_lat_long.csv",
          row.names=FALSE)