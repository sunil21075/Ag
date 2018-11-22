library(chron)
library(data.table)
library(ggplot2)
library(reshape2)
library(dplyr)
library(foreach)
library(iterators)

source_path = "/home/hnoorazar/cleaner_codes/core.R"
source(source_path)



plot_some_plot <- function(input_dir, file_name, version="rcp45", plot_path, output_name){
	output_name = paste0(output_name, "_", version, ".png")
	file_name <- paste0(input_dir, file_name, version, ".rds")
	data <- data.table(readRDS(file_name))
	data = data[, .(PercAdultGen1 = median(PercAdultGen1), 
                    PercAdultGen2 = median(PercAdultGen2), 
                    PercAdultGen3 = median(PercAdultGen3), 
                    PercAdultGen4 = median(PercAdultGen4), 
                    PercLarvaGen1 = median(PercLarvaGen1), 
                    PercLarvaGen2 = median(PercLarvaGen2), 
                    PercLarvaGen3 = median(PercLarvaGen3), 
                    PercLarvaGen4 = median(PercLarvaGen4), 
                    CumDDinC = median(CumDDinC), 
                    CumDDinF = median(CumDDinF)), 
                    by = c("ClimateGroup", "year", "month", "day", "dayofyear")]
    data <- subset(data, select = c("ClimateGroup", "month", 
                                    "PercAdultGen1", "PercAdultGen2", "PercAdultGen3", "PercAdultGen4", 
                                    "PercLarvaGen1", "PercLarvaGen2", "PercLarvaGen3", "PercLarvaGen4"))

    data_melted = melt(data, c("ClimateGroup", "month"), 
                             c("PercAdultGen1", "PercAdultGen2", "PercAdultGen3", "PercAdultGen4", 
                               "PercLarvaGen1", "PercLarvaGen2", "PercLarvaGen3", "PercLarvaGen4"), 
                             variable.name = "Generations")

    p = ggplot(data = data_melted, aes(x = Generations, y = value, fill = ClimateGroup)) +
        geom_boxplot() + coord_flip() +
        facet_wrap(~month)
    ggsave(output_name, p, path=plot_path)

    # saveRDS(p, paste0(data_dir, "/", "popplot.rds"))
    # saveRDS(data, paste0(data_dir, "/", "subData.rds"))
}




#data = data[, .(PercAdultGen1 = median(PercAdultGen1), 
#                PercAdultGen2 = median(PercAdultGen2), 
#                PercAdultGen3 = median(PercAdultGen3), 
#                PercAdultGen4 = median(PercAdultGen4), 
#                PercLarvaGen1 = median(PercLarvaGen1), 
#                PercLarvaGen2 = median(PercLarvaGen2), 
#                PercLarvaGen3 = median(PercLarvaGen3), 
#                PercLarvaGen4 = median(PercLarvaGen4), 
#                CumDDinC = median(CumDDinC), 
#                CumDDinF = median(CumDDinF)), 
#                by = c("ClimateGroup", "year", "month", "day", "dayofyear")]
#data <- subset(data, select = c("ClimateGroup", "month", 
#		                         "PercAdultGen1", "PercAdultGen2", "PercAdultGen3", "PercAdultGen4", 
#		                         "PercLarvaGen1", "PercLarvaGen2", "PercLarvaGen3", "PercLarvaGen4"))

#data_melted = melt(data, c("ClimateGroup", "month"), 
#                         c("PercAdultGen1", "PercAdultGen2", "PercAdultGen3", "PercAdultGen4", 
#                           "PercLarvaGen1", "PercLarvaGen2", "PercLarvaGen3", "PercLarvaGen4"), 
#                         variable.name = "Generations")

#p = ggplot(data = data_melted, aes(x = Generations, y = value, fill = ClimateGroup)) +
#  geom_boxplot() + coord_flip() +
#  facet_wrap(~month)

#saveRDS(p, paste0(data_dir, "/", "popplot.rds"))
# saveRDS(data, paste0(data_dir, "/", "subData.rds"))
