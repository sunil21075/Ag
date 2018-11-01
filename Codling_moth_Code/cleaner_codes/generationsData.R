#!/share/apps/R-3.2.2_gcc/bin/Rscript
#library(chron)
library(data.table)
library(ggplot2)

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
########################################################################################################################
# Things above this line are in core.R and is called generations_func
########################################################################################################################
#data = data[, .(PercAdultGen1 = median(PercAdultGen1), 
                                        # PercAdultGen2 = median(PercAdultGen2), 
                                        # PercAdultGen3 = median(PercAdultGen3), 
                                        # PercAdultGen4 = median(PercAdultGen4), 
                                        # PercLarvaGen1 = median(PercLarvaGen1), 
                                        # PercLarvaGen2 = median(PercLarvaGen2), 
                                        # PercLarvaGen3 = median(PercLarvaGen3), 
                                        # PercLarvaGen4 = median(PercLarvaGen4), 
                                        # CumDDinC = median(CumDDinC), 
                                        # CumDDinF = median(CumDDinF)), 
                                        # by = c("ClimateGroup", "year", "month", "day", "dayofyear")]

#data <- subset(data, select = c("ClimateGroup", "month", 
#		"PercAdultGen1", "PercAdultGen2", "PercAdultGen3", "PercAdultGen4", 
#		"PercLarvaGen1", "PercLarvaGen2", "PercLarvaGen3", "PercLarvaGen4"))

#data_melted = melt(data, c("ClimateGroup", "month"), 
#                   c("PercAdultGen1", "PercAdultGen2", "PercAdultGen3", "PercAdultGen4", 
#                     "PercLarvaGen1", "PercLarvaGen2", "PercLarvaGen3", "PercLarvaGen4"), 
#                   variable.name = "Generations")

#p = ggplot(data = data_melted, aes(x = Generations, y = value, fill = ClimateGroup)) +
#  geom_boxplot() + coord_flip() +
#  facet_wrap(~month)

#saveRDS(p, paste0(data_dir, "/", "popplot.rds"))

