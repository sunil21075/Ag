
generate_vertdd <- function(input_dir, file_name, 
	                        lower_temp = 4.5, upper_temp=24.28){
	file_name <- paste0(data_dir, file_name, ".rds")
    data <- data.table(readRDS(filename))
    #lower = 10 # 50 F
	#upper = 31.11 # 88 F

	twopi = 2 * pi
	pihlf = 0.5 * pi

	data$summ = data$tmin + data$tmax
	data$diff = data$tmax - data$tmin
	data$diffsq = data$diff * data$diff

	data$b = 2 * upper - data$summ
	data$bsq = data$b * data$b

	data$a = 2 * lower - data$summ
	data$asq = data$a * data$a
	data$th1 = atan(data$a / sqrt(data$diffsq - data$asq))
	data$th2 = atan(data$b / sqrt(data$diffsq - data$bsq))

	data[tmin >= lower & tmax > upper, vertdd := ((-diff * cos(th2) - a * (th2 + pihlf))/twopi)]

	data[tmin >= lower & tmax <= upper, vertdd := summ/2 - lower]

	data[tmin < lower & tmax <= upper, vertdd := (diff * cos(th1) - (a * (pihlf - th1)))/twopi]

	data[tmin < lower & tmax > upper, vertdd := (-diff * (cos(th2) - cos(th1))-(a * (th2 - th1)))/twopi]

	data[tmin > tmax | tmax <= lower | tmin >= upper, vertdd := 0]

	data$summ = NULL
	data$diff = NULL
	data$diffsq = NULL
	data$b = NULL
	data$bsq = NULL
	data$a = NULL
	data$asq = NULL
	data$th1 = NULL
	data$th2 = NULL

	data = data[, vert_Cum_dd := cumsum(vertdd), by=list(latitude, longitude, ClimateScenario, ClimateGroup, year)]
	data$vert_Cum_dd_F = data$vert_Cum_dd *1.8

	data$cripps_pink = pnorm(data$vert_Cum_dd_F, mean = 495.51, sd = 42.58, lower.tail = TRUE)
	data$gala = pnorm(data$vert_Cum_dd_F, mean = 528.56, sd = 41.95, lower.tail = TRUE)
	data$red_deli = pnorm(data$vert_Cum_dd_F, mean = 522.74, sd = 42.79, lower.tail = TRUE)
	return(data)
}

	