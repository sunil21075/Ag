#!/share/apps/R-3.2.2_gcc/bin/Rscript
#library(chron)
library(data.table)

#data_dir = getwd()
data_dir = "/data/hydro/users/giridhar/giridhar/codmoth_pop"
filename <- paste0(data_dir, "/allData_grouped_counties.rds")
data <- data.table(readRDS(filename))
#data <- data[data[, month==8 & day==23]]
data <- data[data[, month==11 & day==5]]
data$NumAdultGens <- data$PercAdultGen1 + data$PercAdultGen2 + data$PercAdultGen3 + data$PercAdultGen4
data$NumLarvaGens <- data$PercLarvaGen1 + data$PercLarvaGen2 + data$PercLarvaGen3 + data$PercLarvaGen4
#saveRDS(data, paste0(data_dir, "/", "generations.rds"))
saveRDS(data, paste0(data_dir, "/", "generations1.rds"))

filename <- paste0(data_dir, "/allData_grouped_counties_rcp45.rds")
data <- data.table(readRDS(filename))
#data <- data[data[, month==8 & day==23]]
data <- data[data[, month==11 & day==5]]
data$NumAdultGens <- data$PercAdultGen1 + data$PercAdultGen2 + data$PercAdultGen3 + data$PercAdultGen4
data$NumLarvaGens <- data$PercLarvaGen1 + data$PercLarvaGen2 + data$PercLarvaGen3 + data$PercLarvaGen4
#saveRDS(data, paste0(data_dir, "/", "generations_rcp45.rds"))
saveRDS(data, paste0(data_dir, "/", "generations1_rcp45.rds"))
