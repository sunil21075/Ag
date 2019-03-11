##
## The following two functions also includes absolute fraction which is too much.
## Hence, the two functions after that is written to just include relative population
## The following two functions also need to be updated so that exita columns are not
## saved and the needed info is created right in them, as opposed to having extra drivers
##

generate_diapause_map1_for_analog <- function(input_dir, file_name, param_dir, time_type, CodMothParams_name){
  CodMothParams <- read.table(paste0(param_dir, CodMothParams_name), header=TRUE, sep=",")
  sub1 = data.table(readRDS(paste0(input_dir, file_name)))
  group_vec = c("latitude", "longitude", "ClimateScenario", "year")

  sub2 = sub1[, .(RelPctDiap=(auc(CumulativeDDF, RelDiap)/auc(CumulativeDDF,RelLarvaPop))*100, 
                  RelPctNonDiap = (auc(CumulativeDDF, RelNonDiap)/auc(CumulativeDDF, RelLarvaPop))*100,
                  AbsPctDiap=(auc(CumulativeDDF,AbsDiap)/auc(CumulativeDDF,AbsLarvaPop))*100, 
                  AbsPctNonDiap=(auc(CumulativeDDF,AbsNonDiap)/auc(CumulativeDDF,AbsLarvaPop))*100), by=group_vec]
  
  sub2 = merge(sub2, sub1[CumulativeDDF >= CodMothParams[5,5] & CumulativeDDF < CodMothParams[5,6], .(RelPctDiapGen1 = (auc(CumulativeDDF, RelDiap)/auc(CumulativeDDF, RelLarvaPop))*100), by = group_vec], by = group_vec, all.x = TRUE)
  sub2 = merge(sub2, sub1[CumulativeDDF >= CodMothParams[6,5] & CumulativeDDF < CodMothParams[6,6], .(RelPctDiapGen2 = (auc(CumulativeDDF, RelDiap)/auc(CumulativeDDF, RelLarvaPop))*100), by = group_vec], by = group_vec, all.x = TRUE)
  sub2 = merge(sub2, sub1[CumulativeDDF >= CodMothParams[7,5] & CumulativeDDF < CodMothParams[7,6], .(RelPctDiapGen3 = (auc(CumulativeDDF, RelDiap)/auc(CumulativeDDF, RelLarvaPop))*100), by = group_vec], by = group_vec, all.x = TRUE)
  sub2 = merge(sub2, sub1[CumulativeDDF >= CodMothParams[8,5] & CumulativeDDF < CodMothParams[8,6], .(RelPctDiapGen4 = (auc(CumulativeDDF, RelDiap)/auc(CumulativeDDF, RelLarvaPop))*100), by = group_vec], by = group_vec, all.x = TRUE)
  sub2 = merge(sub2, sub1[CumulativeDDF >= CodMothParams[5,5] & CumulativeDDF < CodMothParams[5,6], .(RelPctNonDiapGen1 = (auc(CumulativeDDF, RelNonDiap)/auc(CumulativeDDF, RelLarvaPop))*100), by = group_vec], by = group_vec, all.x = TRUE)
  
  sub2 = merge(sub2, sub1[CumulativeDDF >= CodMothParams[6,5] & CumulativeDDF < CodMothParams[6,6], .(RelPctNonDiapGen2 = (auc(CumulativeDDF, RelNonDiap)/auc(CumulativeDDF, RelLarvaPop))*100), by = group_vec], by = group_vec, all.x = TRUE)
  sub2 = merge(sub2, sub1[CumulativeDDF >= CodMothParams[7,5] & CumulativeDDF < CodMothParams[7,6], .(RelPctNonDiapGen3 = (auc(CumulativeDDF, RelNonDiap)/auc(CumulativeDDF, RelLarvaPop))*100), by = group_vec], by = group_vec, all.x = TRUE)
  sub2 = merge(sub2, sub1[CumulativeDDF >= CodMothParams[8,5] & CumulativeDDF < CodMothParams[8,6], .(RelPctNonDiapGen4 = (auc(CumulativeDDF, RelNonDiap)/auc(CumulativeDDF, RelLarvaPop))*100), by = group_vec], by = group_vec, all.x = TRUE)
  sub2 = merge(sub2, sub1[CumulativeDDF >= CodMothParams[5,5] & CumulativeDDF < CodMothParams[5,6], .(AbsPctDiapGen1 = (auc(CumulativeDDF, AbsDiap)/auc(CumulativeDDF, AbsLarvaPop))*100), by = group_vec], by = group_vec, all.x = TRUE)
  
  sub2 = merge(sub2, sub1[CumulativeDDF >= CodMothParams[6,5] & CumulativeDDF < CodMothParams[6,6], .(AbsPctDiapGen2 = (auc(CumulativeDDF, AbsDiap)/auc(CumulativeDDF, AbsLarvaPop))*100), by = group_vec], by = group_vec, all.x = TRUE)
  sub2 = merge(sub2, sub1[CumulativeDDF >= CodMothParams[7,5] & CumulativeDDF < CodMothParams[7,6], .(AbsPctDiapGen3 = (auc(CumulativeDDF, AbsDiap)/auc(CumulativeDDF, AbsLarvaPop))*100), by = group_vec], by = group_vec, all.x = TRUE)
  sub2 = merge(sub2, sub1[CumulativeDDF >= CodMothParams[8,5] & CumulativeDDF < CodMothParams[8,6], .(AbsPctDiapGen4 = (auc(CumulativeDDF, AbsDiap)/auc(CumulativeDDF, AbsLarvaPop))*100), by = group_vec], by = group_vec, all.x = TRUE)
  
  sub2 = merge(sub2, sub1[CumulativeDDF >= CodMothParams[5,5] & CumulativeDDF < CodMothParams[5,6], .(AbsPctNonDiapGen1 = (auc(CumulativeDDF, AbsNonDiap)/auc(CumulativeDDF, AbsLarvaPop))*100), by = group_vec], by = group_vec, all.x = TRUE)
  sub2 = merge(sub2, sub1[CumulativeDDF >= CodMothParams[6,5] & CumulativeDDF < CodMothParams[6,6], .(AbsPctNonDiapGen2 = (auc(CumulativeDDF, AbsNonDiap)/auc(CumulativeDDF, AbsLarvaPop))*100), by = group_vec], by = group_vec, all.x = TRUE)
  sub2 = merge(sub2, sub1[CumulativeDDF >= CodMothParams[7,5] & CumulativeDDF < CodMothParams[7,6], .(AbsPctNonDiapGen3 = (auc(CumulativeDDF, AbsNonDiap)/auc(CumulativeDDF, AbsLarvaPop))*100), by = group_vec], by = group_vec, all.x = TRUE)
  sub2 = merge(sub2, sub1[CumulativeDDF >= CodMothParams[8,5] & CumulativeDDF < CodMothParams[8,6], .(AbsPctNonDiapGen4 = (auc(CumulativeDDF, AbsNonDiap)/auc(CumulativeDDF, AbsLarvaPop))*100), by = group_vec], by = group_vec, all.x = TRUE)
  return (sub2)
}

diapause_map1_prep_for_analog <- function(input_dir, file_name, param_dir, time_type){
    file_N = paste0(input_dir, file_name, ".rds")
    data <- data.table(readRDS(file_N))
    print (paste0("line 292"))
    # if (time_type == "future"){
    #     data <- data %>% filter(year >= 2025)
    #     } else if (time_type == "observed"){
    #         data <- data %>% filter(year <= 2015)
    # }
    theta = 0.2163108 + (2 * atan(0.9671396 * tan(0.00860 * (data$dayofyear - 186))))
    phi = asin(0.39795 * cos(theta))
    D = 24 - ((24/pi) * acos((sin(6 * pi / 180) + 
              (sin(data$latitude * pi / 180) * sin(phi)))/(cos(data$latitude * pi / 180) * cos(phi))))
    data$daylength = D

    data$diapause = 102.6077 * exp(-exp(-(-1.306483) * (data$daylength - 16.95815)))
    data$diapause1 = data$diapause
    data[diapause1 > 100, diapause1 := 100]
    data$enterDiap = (data$diapause1/100) * data$SumLarva
    data$escapeDiap = data$SumLarva - data$enterDiap

    sub = data
    rm(data)
    startingpopulationfortheyear <- 1000
  
    # Gen 1
    sub[, LarvaGen1RelFraction := LarvaGen1/sum(LarvaGen1), 
          by =list(year, ClimateScenario, latitude, longitude) ]
    
    sub$AbsPopLarvaGen1 <- sub$LarvaGen1RelFraction * startingpopulationfortheyear
    sub$AbsPopLarvaGen1Diap <- sub$AbsPopLarvaGen1 * sub$diapause1/100
    sub$AbsPopLarvaGen1NonDiap <- sub$AbsPopLarvaGen1 - sub$AbsPopLarvaGen1Diap
   
    # Gen 2
    sub[, LarvaGen2RelFraction := LarvaGen2/sum(LarvaGen2), 
          by = list(year, ClimateScenario, latitude, longitude)]
    sub[, AbsPopLarvaGen2 := LarvaGen2RelFraction * sum(AbsPopLarvaGen1NonDiap)*3.9, 
          by = list(year, ClimateScenario, latitude, longitude)]
    
    sub$AbsPopLarvaGen2Diap <- sub$AbsPopLarvaGen2 * sub$diapause1/100
    sub$AbsPopLarvaGen2NonDiap <- sub$AbsPopLarvaGen2 - sub$AbsPopLarvaGen2Diap
    
    # Gen 3
    sub[, LarvaGen3RelFraction := LarvaGen3/sum(LarvaGen3), 
          by =list(year,ClimateScenario, latitude, longitude)]
    
    sub[, AbsPopLarvaGen3 := LarvaGen3RelFraction * sum(AbsPopLarvaGen2NonDiap)*3.9, 
          by =list(year, ClimateScenario, latitude, longitude) ]
    
    sub$AbsPopLarvaGen3Diap <- sub$AbsPopLarvaGen3 * sub$diapause1/100
    sub$AbsPopLarvaGen3NonDiap <- sub$AbsPopLarvaGen3 - sub$AbsPopLarvaGen3Diap
    print (paste0("line 370 of core "))
    # Gen 4
    sub[, LarvaGen4RelFraction := LarvaGen4/sum(LarvaGen4), 
          by =list(year, ClimateScenario, latitude, longitude)]
    
    sub[, AbsPopLarvaGen4 := LarvaGen4RelFraction*sum(AbsPopLarvaGen3NonDiap)*3.9, 
          by =list(year, ClimateScenario, latitude,longitude)]
    
    sub$AbsPopLarvaGen4Diap <- sub$AbsPopLarvaGen4*sub$diapause1/100
    sub$AbsPopLarvaGen4NonDiap <- sub$AbsPopLarvaGen4 - sub$AbsPopLarvaGen4Diap

    ### get totals similar to Sum Larva column, but abs numbers

    sub$AbsPopTotal <- sub$AbsPopLarvaGen1 + sub$AbsPopLarvaGen2 + sub$AbsPopLarvaGen3 + sub$AbsPopLarvaGen4
    sub$AbsPopDiap <- sub$AbsPopLarvaGen1Diap + sub$AbsPopLarvaGen2Diap + sub$AbsPopLarvaGen3Diap + sub$AbsPopLarvaGen4Diap
    sub$AbsPopNonDiap <- sub$AbsPopLarvaGen1NonDiap + sub$AbsPopLarvaGen2NonDiap + 
                         sub$AbsPopLarvaGen3NonDiap + sub$AbsPopLarvaGen4NonDiap

    sub = subset(sub, select = c("latitude", "longitude", 
                                 "ClimateScenario",
                                 "year", "dayofyear", "CumDDinF", 
                                 "SumLarva", "enterDiap", 
                                 "escapeDiap", "AbsPopTotal",
                                 "AbsPopNonDiap","AbsPopDiap"))
    sub = sub[, .(RelLarvaPop = mean(SumLarva), RelDiap = mean(enterDiap), 
                                                RelNonDiap = mean(escapeDiap), 
                                                AbsLarvaPop = mean(AbsPopTotal), 
                                                AbsDiap = mean(AbsPopDiap), 
                                                AbsNonDiap = mean(AbsPopNonDiap), 
                                                CumulativeDDF = mean(CumDDinF)), 
                                                by = c("ClimateScenario",
                                                       "latitude", "longitude", 
                                                       "dayofyear", "year")]
    return (sub)
}