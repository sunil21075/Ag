####
#### Find Areas within FPU stuff
####
####

rm(list=ls())
library(ggmap)
library(rgdal)
library(tmap)
library(magrittr)
library(sf)
library(raster)
library(dplyr)

library(tigris)
library(ggplot2)
library(ggthemes)

library(choroplethr)
library(choroplethrMaps)
######################
###################### This is the list of counties in shapefile format I found off the web.
######################
ds = "/Users/hn/Documents/GitHub/Kirti/merge_irrigation_Monday/UScounties/"
ly = "UScounties"
US_cnt <- read_sf(dsn=path.expand(ds), layer = ly, quiet = TRUE)

####### US census shapefile which is the same as the file above.
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
# census_ds = "/Users/hn/Documents/GitHub/Kirti/merge_irrigation_Monday/cb_2017_us_county_500k/"
# census_layer="cb_2017_us_county_500k"
# census_cnt  <- read_sf(dsn=path.expand(ds), layer = ly, quiet = TRUE)
######################
###################### Read the FUP (shapefile) Data off the disk
######################
data_dir = "/Users/hn/Documents/GitHub/Kirti/merge_irrigation_Monday/IMPACT_FPU_Map/"
layer_name = "fpu2015_polygons_v3_multipart_polygons"
FPU_data <- read_sf(dsn=path.expand(data_dir), layer = layer_name, quiet = TRUE)

## extract US part of the data from whole world
tofind <- c("_USA")
FPU_data <- FPU_data[grep(paste(tofind, collapse = "|"), FPU_data$FPU2015), ]
FPU_data <- within(FPU_data, remove(FPU2015))


######################
###################### Vector of the 16 FPU areas
######################
USA_FPU_names = c("Alaska", "Arkansas", "California", "Columbia",
	              "Colorado", "GreatBasin", "GreatLakes", "Hawaii",
	              "Mississippi", "Missouri", "Ohio", "RioGrande",
	              "RedWinnipeg", "Southeast", "Northeast", "WesternGulfMexico")

#####################
##################### Mask (filter/extract) each FPU area from the whole data set.
#####################
Alaska    <- subset(FPU_data, USAFPU == USA_FPU_names[1])
Arkansas  <- subset(FPU_data, USAFPU == USA_FPU_names[2])
California<- subset(FPU_data, USAFPU == USA_FPU_names[3])
Colombia  <- subset(FPU_data, USAFPU == USA_FPU_names[4])

Colorado  <- subset(FPU_data, USAFPU == USA_FPU_names[5])
GreatBasin<- subset(FPU_data, USAFPU == USA_FPU_names[6])
GreatLakes<- subset(FPU_data, USAFPU == USA_FPU_names[7])
Hawaii    <- subset(FPU_data, USAFPU == USA_FPU_names[8])

Mississippi <- subset(FPU_data, USAFPU == USA_FPU_names[9])
Missouri  <- subset(FPU_data, USAFPU == USA_FPU_names[10])
Ohio <- subset(FPU_data, USAFPU == USA_FPU_names[11])
RioGrande <- subset(FPU_data, USAFPU == USA_FPU_names[12])

RedWinnipeg <- subset(FPU_data, USAFPU == USA_FPU_names[13])
Southeast <- subset(FPU_data, USAFPU == USA_FPU_names[14])
Northeast <- subset(FPU_data, USAFPU == USA_FPU_names[15])
WesternGulfMexico <- subset(FPU_data, USAFPU == USA_FPU_names[16])

#####################
##################### find intersection of each FPU area/boundary with the US counties.
#####################
Alask_int_cnt <- st_intersection(Alaska, US_cnt)
Arkan_int_cnt <- st_intersection(Arkansas, US_cnt)
Calif_int_cnt <- st_intersection(California, US_cnt)
Colum_int_cnt <- st_intersection(Colombia, US_cnt)

Color_int_cnt <- st_intersection(Colorado, US_cnt)
GrtBa_int_cnt <- st_intersection(GreatBasin, US_cnt)
GrtLa_int_cnt <- st_intersection(GreatLakes, US_cnt)
Hawai_int_cnt <- st_intersection(Hawaii, US_cnt)

Missi_int_cnt <- st_intersection(Mississippi, US_cnt)
Misso_int_cnt <- st_intersection(Missouri, US_cnt)
Ohio_int_cnt  <- st_intersection(Ohio, US_cnt)
RioGr_int_cnt <- st_intersection(RioGrande, US_cnt)

RedWi_int_cnt <- st_intersection(RedWinnipeg, US_cnt)
South_int_cnt <- st_intersection(Southeast, US_cnt)
North_int_cnt <- st_intersection(Northeast, US_cnt)
WstGu_int_cnt <- st_intersection(WesternGulfMexico, US_cnt)

#####################
#####################  plotting part
#####################

# the following three lines are 
# for when rstudio complains about 
# the size of image being big
graphics.off() 
par("mar")
par(mar=c(1,1,1,1))

# some colors : "grey70", "dodgerblue", "olivedrab4", "red"

plot(US_cnt$geometry, lwd=.1)
plot(colombia$geometry, axes=F, add=T, col ="red", lwd=.1)
plot(Colum_int_cnt$geometry, add =T, col="olivedrab4", density=c(1), lwd=.1)

#####################

#####################
##################### compute Area of each FPU area!
#####################
Alask_area <- Alask_int_cnt %>% mutate(area= st_area(.) %>% as.numeric())
Arkan_area <- Arkan_int_cnt %>% mutate(area= st_area(.) %>% as.numeric())
Calif_area <- Calif_int_cnt %>% mutate(area= st_area(.) %>% as.numeric())
Colum_area <- Colum_int_cnt %>% mutate(area= st_area(.) %>% as.numeric())

Color_area <- Color_int_cnt %>% mutate(area= st_area(.) %>% as.numeric())
GrtBa_area <- GrtBa_int_cnt %>% mutate(area= st_area(.) %>% as.numeric())
GrtLa_area <- GrtLa_int_cnt %>% mutate(area= st_area(.) %>% as.numeric())
Hawai_area <- Hawai_int_cnt %>% mutate(area= st_area(.) %>% as.numeric())

Missi_area <- Missi_int_cnt %>% mutate(area= st_area(.) %>% as.numeric())
Misso_area <- Misso_int_cnt %>% mutate(area= st_area(.) %>% as.numeric())
Ohio_area  <- Ohio_int_cnt  %>% mutate(area= st_area(.) %>% as.numeric())
RioGr_area <- RioGr_int_cnt %>% mutate(area= st_area(.) %>% as.numeric())
RedWi_area <- RedWi_int_cnt %>% mutate(area= st_area(.) %>% as.numeric())
South_area <- South_int_cnt %>% mutate(area= st_area(.) %>% as.numeric())
North_area <- North_int_cnt %>% mutate(area= st_area(.) %>% as.numeric())
WstGu_area <- WstGu_int_cnt %>% mutate(area= st_area(.) %>% as.numeric())

#####################
##################### Group by (Do we want to do by Name? or FIPS?)
#####################
Alask_area_gN <- Alask_area %>% as_tibble() %>% group_by(NAME, USAFPU) %>% summarize(area = sum(area))
Arkan_area_gN <- Arkan_area %>% as_tibble() %>% group_by(NAME, USAFPU) %>% summarize(area = sum(area))
Calif_area_gN <- Calif_area %>% as_tibble() %>% group_by(NAME, USAFPU) %>% summarize(area = sum(area))
Colum_area_gN <- Colum_area %>% as_tibble() %>% group_by(NAME, USAFPU) %>% summarize(area = sum(area))

Color_area_gN <- Color_area %>% as_tibble() %>% group_by(NAME, USAFPU) %>% summarize(area = sum(area))
GrtBa_area_gN <- GrtBa_area %>% as_tibble() %>% group_by(NAME, USAFPU) %>% summarize(area = sum(area))
GrtLa_area_gN <- GrtLa_area %>% as_tibble() %>% group_by(NAME, USAFPU) %>% summarize(area = sum(area))
Hawai_area_gN <- Hawai_area %>% as_tibble() %>% group_by(NAME, USAFPU) %>% summarize(area = sum(area))
            
Missi_area_gN <- Missi_area %>% as_tibble() %>% group_by(NAME, USAFPU) %>% summarize(area = sum(area))
Misso_area_gN <- Misso_area %>% as_tibble() %>% group_by(NAME, USAFPU) %>% summarize(area = sum(area))
Ohio_area_gN  <- Ohio_area  %>% as_tibble() %>% group_by(NAME, USAFPU) %>% summarize(area = sum(area))
RioGr_area_gN <- RioGr_area %>% as_tibble() %>% group_by(NAME, USAFPU) %>% summarize(area = sum(area))

RedWi_area_gN <- RedWi_area %>% as_tibble() %>% group_by(NAME, USAFPU) %>% summarize(area = sum(area))
South_area_gN <- South_area %>% as_tibble() %>% group_by(NAME, USAFPU) %>% summarize(area = sum(area))
North_area_gN <- North_area %>% as_tibble() %>% group_by(NAME, USAFPU) %>% summarize(area = sum(area))
WstGu_area_gN <- WstGu_area %>% as_tibble() %>% group_by(NAME, USAFPU) %>% summarize(area = sum(area))
#####################
##################### Group by FIPS?
#####################
Alask_area_gF <- Alask_area %>% as_tibble() %>% group_by(FIPS, USAFPU) %>% summarize(area = sum(area))
Arkan_area_gF <- Arkan_area %>% as_tibble() %>% group_by(FIPS, USAFPU) %>% summarize(area = sum(area))
Calif_area_gF <- Calif_area %>% as_tibble() %>% group_by(FIPS, USAFPU) %>% summarize(area = sum(area))
Colum_area_gF <- Colum_area %>% as_tibble() %>% group_by(FIPS, USAFPU) %>% summarize(area = sum(area))

Color_area_gF <- Color_area %>% as_tibble() %>% group_by(FIPS, USAFPU) %>% summarize(area = sum(area))
GrtBa_area_gF <- GrtBa_area %>% as_tibble() %>% group_by(FIPS, USAFPU) %>% summarize(area = sum(area))
GrtLa_area_gF <- GrtLa_area %>% as_tibble() %>% group_by(FIPS, USAFPU) %>% summarize(area = sum(area))
Hawai_area_gF <- Hawai_area %>% as_tibble() %>% group_by(NAME, USAFPU) %>% summarize(area = sum(area))
            
Missi_area_gF <- Missi_area %>% as_tibble() %>% group_by(FIPS, USAFPU) %>% summarize(area = sum(area))
Misso_area_gF <- Misso_area %>% as_tibble() %>% group_by(FIPS, USAFPU) %>% summarize(area = sum(area))
Ohio_area_gF <- Ohio_area  %>% as_tibble() %>% group_by(FIPS, USAFPU) %>% summarize(area = sum(area))
RioGr_area_gF <- RioGr_area %>% as_tibble() %>% group_by(FIPS, USAFPU) %>% summarize(area = sum(area))

RedWi_area_gF <- RedWi_area %>% as_tibble() %>% group_by(FIPS, USAFPU) %>% summarize(area = sum(area))
South_area_gF <- South_area %>% as_tibble() %>% group_by(FIPS, USAFPU) %>% summarize(area = sum(area))
North_area_gF <- North_area %>% as_tibble() %>% group_by(FIPS, USAFPU) %>% summarize(area = sum(area))
WstGu_area_gF <- WstGu_area %>% as_tibble() %>% group_by(FIPS, USAFPU) %>% summarize(area = sum(area))

#####################
##################### Area of counties (To check whether we have done a good job or not)
##################### by looling at boundary counties that are in two different FPU areas
#####################

## e.g.
nevada_counties <- subset(US_cnt, STATE_NAME=="Nevada")
nevada_counties_area <- nevada_counties %>% mutate(area = st_area(.)  %>% as.numeric() )
just_elko = subset(attArea, NAME == "Elko")
countyTotalArea <- US_cnt %>% mutate(area = st_area(.)  %>% as.numeric() )

################
url_dir = "/Users/hn/Documents/GitHub/Kirti/merge_irrigation_Monday/"
file_name = "NASS_county.csv"
file = paste0(url_dir, file_name)
#url_info <- read.table(file, header=T, sep = ",")

url_info <- data.table(readRDS(paste0(url_dir, "NASS_county.rds")))

# url_info <- url_info[url_info$county != "000"]
# saveRDS(url_info, paste0(url_dir, "NASS_county.rds"))
# write.csv(url_info, file = paste0(url_dir, "NASS_county.csv" ), row.names=FALSE)

# url_info_historical <- url_info[url_info$history == 2]
url_df <- url_info[url_info$history == 1]


#####################
##################### Area of counties (To check whether we have done a good job or not)
##################### by looling at boundary counties that are in two different FPU areas
#####################

joint_cnt_crd = merge(US_cnt, url_df, by= "FIPS" )
plot(joint_cnt_crd$geometry)


# convert sp to spatial
# CRD_spatial <- as(joint_cnt_crd, 'Spatial')
CRD <- unionSpatialPolygons(CRD_spatial, CRD_spatial$CDR_FIPS)


