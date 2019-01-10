

library(data.table)
library(ggplot2)
library(dplyr)

excel_file <- "/Users/hn/Documents/GitHub/Kirti/hardiness/Model_for_distribution_beta_v2.7.xls"
param_dir <- "/Users/hn/Documents/GitHub/Kirti/hardiness/R_code/parameters/"

input_params<-data.frame("variety" = "Cabernet Sauvignon", 
	                     "Hc_initial" = -10.3, 
	                     "Hc_min" = -1.2,
	                     "Hc_max" = -25.1,
	                     "t_threshold_endo" = 13.0,
	                     "t_threshold_eco" = 5.0,
	                     "Ecodormancy_boundary" = -700,
	                     "acclimation_rate_endo" = 0.12,
	                     "acclimation_rate_eco" = 0.10,
	                     "deacclimation_rate_endo" = 0.08,
	                     "deacclimation_rate_eco" = 0.10,
	                     "theta" = 7)

write.csv(input_params, 
	      file = paste0(param_dir, "input_parameters.csv"), 
	      row.names=FALSE)



variety_parameters <- data.table(read_excel(excel_file, sheet = "variety_parameters"))
write.csv(variety_parameters, 
	      file = paste0(param_dir, "variety_parameters.csv"), 
	      row.names=FALSE)


input_temps <- data.table(read_excel(excel_file, sheet = "input_temps"))
input_temps <- within(input_temps, remove(X__1, X__2))
write.csv(input_temps, 
	      file = paste0(param_dir, "input_temps.csv"), 
	      row.names=FALSE)

Prosser_temps <- data.table(read_excel(excel_file, sheet = "Prosser_temps"))
write.csv(Prosser_temps, 
	      file = paste0(param_dir, "Prosser_temps.csv"),
	      row.names=FALSE)





















