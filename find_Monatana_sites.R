
library(foreign)

Min_counties <- read.dbf("/Users/hn/Documents/GitHub/Kirti/codling_moth/code/parameters/vic_grid_cover_conus/VICID_CO.DBF")

setnames(Min_counties, old= colnames(Min_counties), new= tolower(colnames(Min_counties)))
setnames(Min_counties, old=c("vicclat", "vicclon"), new=c("lat", "long"))

counties_filter <- subset(Min_counties, select= c(vicid, state, lat, long))



