data_path = "/Users/hn/Desktop/Kirti/check_point/my_aeolus_2015/all_local/pest_control/"
file_name = "pest_quantile_rcp45.rds"

curr_data = readRDS(paste0(data_path, file_name))


color_ord = c("grey70", "dodgerblue", "olivedrab4", "red")


if (stage == "adult"){
    data <- subset(data, select = c("PercAdultGen1", "PercAdultGen2", "PercAdultGen3", "PercAdultGen4",
                                    "ClimateGroup", "CountyGroup",
                                    "CumDDinF", "dayofyear"))
    L = c('PercAdultGen1','PercAdultGen2', 'PercAdultGen3','PercAdultGen4')
  } else{
    data <- subset(data, select = c("PercLarvaGen1", "PercLarvaGen2", 
    	                            "PercLarvaGen3", "PercLarvaGen4",
                                    "ClimateGroup", "CountyGroup",
                                    "CumDDinF", "dayofyear"))
    L = c('PercLarvaGen1', 'PercLarvaGen2',  'PercLarvaGen3', 'PercLarvaGen4')
  }