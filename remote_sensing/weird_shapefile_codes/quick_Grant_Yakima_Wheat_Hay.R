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

data_dir <- paste0("/Users/hn/Documents/01_research_data/", 
                    "Ag_check_point/remote_sensing/", 
                    "02_correct_years_separate/lat_long_projections/")

year <- 2015
WSDACrop <- readOGR(paste0(data_dir, 
                           paste0("WSDACrop_", as.character(year), "/WSDACrop_", as.character(year), ".shp")),
                    layer = paste0("WSDACrop_", as.character(year)),
                    GDAL1_integer64_policy = TRUE)

Grant <- WSDACrop[grepl('Grant', WSDACrop$county), ]

print (sort(unique(Grant@data$Irrigtn)))
print(dim(Grant))

Grant_irrigated <- Grant[!(Grant@data$Irrigtn %in% c("None", "Unknown")), ]
Grant_non_irrigated <- Grant[(Grant@data$Irrigtn %in% c("None")), ] # "Unknown"
# Grant_unknown_irrigated <- Grant[(Grant@data$Irrigtn %in% c("Unknown")), ] # "Unknown"

# this will be less that dim(Grant) since some Irrigtn are unknown
print(dim(Grant_irrigated)[1] + dim(Grant_non_irrigated)[1])

# could have done this all at once (whean and irrigated!)
Grant_wheat_irrigated <- Grant_irrigated[grepl('Wheat', Grant_irrigated$CropTyp), ]
Grant_wheat_non_irrigated <- Grant_non_irrigated[grepl('Wheat', Grant_non_irrigated$CropTyp), ]

Grant_hay_irrigated <- Grant_irrigated[grepl('Hay', Grant_irrigated$CropTyp), ]
Grant_hay_non_irrigated <- Grant_non_irrigated[grepl('Hay', Grant_non_irrigated$CropTyp), ]

print(paste0("Grant_wheat_irrigated area is ", sum(Grant_wheat_irrigated@data$ExctAcr)))
print(paste0("Grant_wheat_non_irrigated area is ", sum(Grant_wheat_non_irrigated@data$ExctAcr)))
print(paste0("Grant_hay_irrigated area is ", sum(Grant_hay_irrigated@data$ExctAcr)))
print(paste0("Grant_hay_non_irrigated area is ", sum(Grant_hay_non_irrigated@data$ExctAcr)))



Yakima <- WSDACrop[grepl('Yakima', WSDACrop$county), ]

print (sort(unique(Yakima@data$Irrigtn)))
print(dim(Yakima))

Yakima_irrigated <- Yakima[!(Yakima@data$Irrigtn %in% c("None", "Unknown")), ]
Yakima_non_irrigated <- Yakima[(Yakima@data$Irrigtn %in% c("None")), ] # "Unknown"

# this will be less that dim(Yakima) since some Irrigtn are unknown
print(dim(Yakima_irrigated)[1] + dim(Yakima_non_irrigated)[1])

# could have done this all at once (whean and irrigated!)
Yakima_wheat_irrigated <- Yakima_irrigated[grepl('Wheat', Yakima_irrigated$CropTyp), ]
Yakima_wheat_non_irrigated <- Yakima_non_irrigated[grepl('Wheat', Yakima_non_irrigated$CropTyp), ]

Yakima_hay_irrigated <- Yakima_irrigated[grepl('Hay', Yakima_irrigated$CropTyp), ]
Yakima_hay_non_irrigated <- Yakima_non_irrigated[grepl('Hay', Yakima_non_irrigated$CropTyp), ]

print(paste0("Yakima_wheat_irrigated area is ", sum(Yakima_wheat_irrigated@data$ExctAcr)))
print(paste0("Yakima_wheat_non_irrigated area is ", sum(Yakima_wheat_non_irrigated@data$ExctAcr)))
print(paste0("Yakima_hay_irrigated area is ", sum(Yakima_hay_irrigated@data$ExctAcr)))
print(paste0("Yakima_hay_non_irrigated area is ", sum(Yakima_hay_non_irrigated@data$ExctAcr)))

c(sum(Grant_wheat_irrigated@data$ExctAcr), 
  sum(Grant_wheat_non_irrigated@data$ExctAcr), 
  sum(Grant_hay_irrigated@data$ExctAcr), 
  sum(Grant_hay_non_irrigated@data$ExctAcr),
  sum(Yakima_wheat_irrigated@data$ExctAcr), 
  sum(Yakima_wheat_non_irrigated@data$ExctAcr),
  sum(Yakima_hay_irrigated@data$ExctAcr),
  sum(Yakima_hay_non_irrigated@data$ExctAcr))


c(sum(Grant_wheat_irrigated@data$TtlAcrs), 
  sum(Grant_wheat_non_irrigated@data$TtlAcrs), 
  sum(Grant_hay_irrigated@data$TtlAcrs), 
  sum(Grant_hay_non_irrigated@data$TtlAcrs),
  sum(Yakima_wheat_irrigated@data$TtlAcrs), 
  sum(Yakima_wheat_non_irrigated@data$TtlAcrs),
  sum(Yakima_hay_irrigated@data$TtlAcrs),
  sum(Yakima_hay_non_irrigated@data$TtlAcrs))



c(sum(Grant_wheat_irrigated@data$Acres), 
  sum(Grant_wheat_non_irrigated@data$Acres), 
  sum(Grant_hay_irrigated@data$Acres), 
  sum(Grant_hay_non_irrigated@data$Acres),
  sum(Yakima_wheat_irrigated@data$Acres), 
  sum(Yakima_wheat_non_irrigated@data$Acres),
  sum(Yakima_hay_irrigated@data$Acres),
  sum(Yakima_hay_non_irrigated@data$Acres))