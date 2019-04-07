make_non_overlapping <- function(overlap_dt){
  old_time_periods <- c("2040's", "2060's", "2080's", "Historical")
  new_time_periods <- c("2026-2050", "2051-2075", "2076-2095", "Historical")

  ########## Historical
  hist <- overlap_dt %>% filter(ClimateGroup == "Historical")

  ########## 2040
  F1 <- overlap_dt %>% filter(ClimateGroup == "2040's")
  F1 <- F1 %>% filter(year>=2025 & year<=2050)
  F1$ClimateGroup <- new_time_periods[1]

  ########## 2060
  F2 <- overlap_dt %>% filter(ClimateGroup == "2060's")
  F2 <- F2 %>% filter(year>=2051 & year<=2075)
  F2$ClimateGroup <- new_time_periods[2]

  ########## 2080
  F3 <- overlap_dt %>% filter(ClimateGroup == "2080's")
  F3 <- F3 %>% filter(year>=2076)
  F3$ClimateGroup <- new_time_periods[3]

  non_overlap_dt <- rbind(hist, F1, F2, F3)
  non_overlap_dt$ClimateGroup <- factor(non_overlap_dt$ClimateGroup, levels=new_time_periods, order=T)

}