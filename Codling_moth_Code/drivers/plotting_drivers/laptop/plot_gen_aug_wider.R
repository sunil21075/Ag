
master_path = "/Users/hn/Desktop/Kirti/check_point/my_aeolus_2015/sensitivity_wider/one_loc/"
aug_file_name = "generations_Aug_combined_CMPOP_rcp45.rds"

twenty_gen_aug_45 <- readRDS(paste0(master_path, "20/", aug_file_name))
zero_gen_aug_45   <- readRDS(paste0(master_path, "0/", aug_file_name))


var = "NumLarvaGens"
# var = "NumAdultGens"

data_0 = data.table(zero_gen_aug_45)
data_0$CountyGroup = as.character(data_0$CountyGroup)
data_0[CountyGroup == 1]$CountyGroup = 'Cooler Areas'
data_0[CountyGroup == 2]$CountyGroup = 'Warmer Areas'


data_0 <- subset(data_0, select = c("ClimateGroup", "CountyGroup", var))

df_0 <- data.frame(data_0)
df_0 <- (df_0 %>% group_by(CountyGroup, ClimateGroup))
medians_0 <- (df_0 %>% summarise(med = median(!!sym(var))))

data_20 = data.table(twenty_gen_aug_45)
data_20$CountyGroup = as.character(data_20$CountyGroup)
data_20[CountyGroup == 1]$CountyGroup = 'Cooler Areas'
data_20[CountyGroup == 2]$CountyGroup = 'Warmer Areas'

data_20 <- subset(data_20, select = c("ClimateGroup", "CountyGroup", var))
df_20 <- data.frame(data_20)
df_20 <- (df_20 %>% group_by(CountyGroup, ClimateGroup))
medians_20 <- (df_20 %>% summarise(med = median(!!sym(var))))

 
twenty_gen_aug_45$PercAdultGen1 - zero_gen_aug_45$PercAdultGen1