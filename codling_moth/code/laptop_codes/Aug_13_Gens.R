### Generations by August
master_dir = "/Users/hn/Desktop/Kirti/check_point/my_aeolus_2015/sensitivity_wider/"
## original
####### Adult
data <- readRDS(paste0(master_dir, "0/generations_Aug_combined_CMPOP_rcp85.rds"))
var = "NumAdultGens"
data <- subset(data, select = c("ClimateGroup", "CountyGroup", var))
df <- data.frame(data)
df <- (df %>% group_by(CountyGroup, ClimateGroup))
medians <- (df %>% summarise(med = median(!!sym(var))))

####### Larva
data <- readRDS(paste0(master_dir, "0/generations_Aug_combined_CMPOP_rcp85.rds"))
var = "NumLarvaGens"
data <- subset(data, select = c("ClimateGroup", "CountyGroup", var))
df <- data.frame(data)
df <- (df %>% group_by(CountyGroup, ClimateGroup))
medians <- (df %>% summarise(med = median(!!sym(var))))

## shift 5
####### Adult
data <- readRDS(paste0(master_dir, "5/generations_Aug_combined_CMPOP_rcp85.rds"))
var = "NumAdultGens"
data <- subset(data, select = c("ClimateGroup", "CountyGroup", var))
df <- data.frame(data)
df <- (df %>% group_by(CountyGroup, ClimateGroup))
medians <- (df %>% summarise(med = median(!!sym(var))))

####### Larva
data <- readRDS(paste0(master_dir, "5/generations_Aug_combined_CMPOP_rcp85.rds"))
var = "NumLarvaGens"
data <- subset(data, select = c("ClimateGroup", "CountyGroup", var))
df <- data.frame(data)
df <- (df %>% group_by(CountyGroup, ClimateGroup))
medians <- (df %>% summarise(med = median(!!sym(var))))

## shift 10
####### Adult
data <- readRDS(paste0(master_dir, "10/generations_Aug_combined_CMPOP_rcp85.rds"))
var = "NumAdultGens"
data <- subset(data, select = c("ClimateGroup", "CountyGroup", var))
df <- data.frame(data)
df <- (df %>% group_by(CountyGroup, ClimateGroup))
medians <- (df %>% summarise(med = median(!!sym(var))))

####### Larva
data <- readRDS(paste0(master_dir, "10/generations_Aug_combined_CMPOP_rcp85.rds"))
var = "NumLarvaGens"
data <- subset(data, select = c("ClimateGroup", "CountyGroup", var))
df <- data.frame(data)
df <- (df %>% group_by(CountyGroup, ClimateGroup))
medians <- (df %>% summarise(med = median(!!sym(var))))

## shift 15
####### Adult
data <- readRDS(paste0(master_dir, "15/generations_Aug_combined_CMPOP_rcp85.rds"))
var = "NumAdultGens"
data <- subset(data, select = c("ClimateGroup", "CountyGroup", var))
df <- data.frame(data)
df <- (df %>% group_by(CountyGroup, ClimateGroup))
medians <- (df %>% summarise(med = median(!!sym(var))))

####### Larva
data <- readRDS( paste0(master_dir, "15/generations_Aug_combined_CMPOP_rcp85.rds"))
var = "NumLarvaGens"
data <- subset(data, select = c("ClimateGroup", "CountyGroup", var))
df <- data.frame(data)
df <- (df %>% group_by(CountyGroup, ClimateGroup))
medians <- (df %>% summarise(med = median(!!sym(var))))

## shift 20
####### Adult
data <- readRDS(paste0(master_dir, "20/generations_Aug_combined_CMPOP_rcp85.rds"))
var = "NumAdultGens"
data <- subset(data, select = c("ClimateGroup", "CountyGroup", var))
df <- data.frame(data)
df <- (df %>% group_by(CountyGroup, ClimateGroup))
medians <- (df %>% summarise(med = median(!!sym(var))))

####### Larva
data <- readRDS(paste0(master_dir, "20/generations_Aug_combined_CMPOP_rcp85.rds"))
var = "NumLarvaGens"
data <- subset(data, select = c("ClimateGroup", "CountyGroup", var))
df <- data.frame(data)
df <- (df %>% group_by(CountyGroup, ClimateGroup))
medians <- (df %>% summarise(med = median(!!sym(var))))

