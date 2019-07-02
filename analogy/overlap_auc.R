library(dplyr)
library(data.table)

seperate_1D_similarities <- function(f_dt){
  ###########
  ###########
  future <- f_dt %>%
            filter(model != "observed") %>%
            data.table()
  
  hist <- f_dt %>%
          filter(model == "observed") %>%
          data.table()

  future_press <- future$CumDDinF_Aug23
  hist_press <- hist$CumDDinF_Aug23
  
  future_prcip <- future$yearly_precip
  hist_precip <- hist$yearly_precip

  ########## Pest Pressure Similarity Measurement
  press_simil <- measure_1D_similarities(a=future_press, b=hist_press)

  ########## Precipitation Similarity Measurement
  precip_simil <- measure_1D_similarities(a=future_prcip, b=hist_precip)

  return(list(press_simil, precip_simil))

}
