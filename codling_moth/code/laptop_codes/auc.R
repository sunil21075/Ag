# compute are under the curve

rm(list=ls())
library(MESS) # has the aux function in it.
library(data.table)

data_dir = "/Users/hn/Desktop/Kirti/check_point/my_aeolus_2015/all_local/diapause/"

file_list = list.files(path = data_dir, 
                       pattern = ".rds", 
                       all.files = FALSE, 
                       full.names = FALSE, 
                       recursive = FALSE)


for (file in file_list){	
    ######### initiate the output table
    pop_type = unlist(strsplit(file, "_"))[2]
    model_type = unlist(strsplit(unlist(strsplit(file, "_"))[3], ".rds"))
    gen_borders = c(213, 1153, 2313, 3443, 4453)
    
    ###########################################################################
    df_hist <- data.frame(matrix(ncol = 5, nrow = 4))
    col_names = c("CountyGroup", 
		          "hist_gen_1", "hist_gen_2", "hist_gen_3", "hist_gen_4")
    colnames(df_hist) <- col_names
    df_hist[1, 1] = "warmer_total"
    df_hist[2, 1] = "warmer_escape"
    df_hist[3, 1] = "colder_total"
    df_hist[4, 1] = "colder_escape"
    ###########################################################################
    df_2040 <- data.frame(matrix(ncol = 5, nrow = 4))
    col_names = c("CountyGroup", 
		          "2040_gen_1", "2040_gen_2", "2040_gen_3", "2040_gen_4")
    colnames(df_2040) <- col_names
    df_2040[1, 1] = "warmer_total"
    df_2040[2, 1] = "warmer_escape"
    df_2040[3, 1] = "colder_total"
    df_2040[4, 1] = "colder_escape"
    ###########################################################################
    df_2060 <- data.frame(matrix(ncol = 5, nrow = 4))
    col_names = c("CountyGroup", 
		          "2060_gen_1", "2060_gen_2", "2060_gen_3", "2060_gen_4")
    colnames(df_2060) <- col_names
    df_2060[1, 1] = "warmer_total"
    df_2060[2, 1] = "warmer_escape"
    df_2060[3, 1] = "colder_total"
    df_2060[4, 1] = "colder_escape"
    ###########################################################################x
    df_2080 <- data.frame(matrix(ncol = 5, nrow = 4))
    col_names = c("CountyGroup", 
		          "2080_gen_1", "2080_gen_2", "2080_gen_3", "2080_gen_4")
    colnames(df_2080) <- col_names
    df_2080[1, 1] = "warmer_total"
    df_2080[2, 1] = "warmer_escape"
    df_2080[3, 1] = "colder_total"
    df_2080[4, 1] = "colder_escape"
    
    ####### read data
    data <- data.table(readRDS(file))

    # subset the data to get just needed info.
    data <- subset(data, select=c("ClimateGroup", "CountyGroup", "CumulativeDDF", "variable", "value"))
    if (pop_type=="rel"){
    	data <- data[variable =="RelLarvaPop" | variable =="RelNonDiap"]
    } else {
    	data <- data[variable =="AbsLarvaPop" | variable =="AbsNonDiap"]
    }
    data$variable <- factor(data$variable)

    time_periods = c("Historical", "2040's", "2060's", "2080's")
    for (time_p in time_period){
    	# to prevent a warning 
    	if (time_p == "Historical"){
    		data_sub = data[data$ClimateGroup == "Historical"]
    	} else if (time_p == "2040's"){
    		data_sub = data[data$ClimateGroup == "2040's"]
    	} else if (time_p == "2060's"){
    		data_sub = data[data$ClimateGroup == "2060's"]
    	} else if (time_p == "2080's"){
    		data_sub = data[data$ClimateGroup == "2080's"]
    	}

    	data_sub_warm = data_sub[data_sub$CountyGroup==2]
    	data_sub_cold = data_sub[data_sub$CountyGroup==1]
    	if (pop_type=="rel"){
    		data_sub_warm_total = data_sub_warm[data_sub_warm$variable == "RelLarvaPop"]
    		data_sub_warm_escap = data_sub_warm[data_sub_warm$variable == "RelNonDiap"]

    		data_sub_cold_total = data_sub_cold[data_sub_cold$variable == "RelLarvaPop"]
    		data_sub_cold_escap = data_sub_cold[data_sub_cold$variable == "RelNonDiap"]

    	} else {
    		data_sub_warm_total = data_sub_warm[data_sub_warm$variable == "AbsLarvaPop"]
    		data_sub_warm_escap = data_sub_warm[data_sub_warm$variable == "AbsNonDiap"]

    		data_sub_cold_total = data_sub_cold[data_sub_cold$variable == "AbsLarvaPop"]
    		data_sub_cold_escap = data_sub_cold[data_sub_cold$variable == "AbsNonDiap"]
    	}

    	if (time_p == "Historical"){
    		##########################################
    		########################################## warmer total
    		# gen 1
    		data_gen = data_sub_warm_total[ data_sub_warm_total$CumulativeDDF>=gen_borders[1] & data_sub_warm_total$CumulativeDDF<gen_borders[2]]
    		df_hist[1, 2] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
    		
    		# gen 2
    		data_gen = data_sub_warm_total[ data_sub_warm_total$CumulativeDDF>=gen_borders[2] & data_sub_warm_total$CumulativeDDF<gen_borders[3]]
    		df_hist[1, 3] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
    		
    		# gen 3
    		data_gen = data_sub_warm_total[ data_sub_warm_total$CumulativeDDF>=gen_borders[3] & data_sub_warm_total$CumulativeDDF<gen_borders[4]]
    		df_hist[1, 4] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
    		
    		# gen 4
    		data_gen = data_sub_warm_total[ data_sub_warm_total$CumulativeDDF>=gen_borders[4] & data_sub_warm_total$CumulativeDDF<=gen_borders[5]]
    		df_hist[1, 5] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            ##########################################
    		########################################## warmer escape
    		# gen 1
    		data_gen = data_sub_warm_escap[data_sub_warm_escap$CumulativeDDF>=gen_borders[1] & data_sub_warm_escap$CumulativeDDF<gen_borders[2]]
    		df_hist[2, 2] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
    		
    		# gen 2
    		data_gen = data_sub_warm_escap[data_sub_warm_escap$CumulativeDDF>=gen_borders[2] & data_sub_warm_escap$CumulativeDDF<gen_borders[3]]
    		df_hist[2, 3] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
    		
    		# gen 3
    		data_gen = data_sub_warm_escap[data_sub_warm_escap$CumulativeDDF>=gen_borders[3] & data_sub_warm_escap$CumulativeDDF<gen_borders[4]]
    		df_hist[2, 4] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
    		
    		# gen 4
    		data_gen = data_sub_warm_escap[data_sub_warm_escap$CumulativeDDF>=gen_borders[4] & data_sub_warm_escap$CumulativeDDF<=gen_borders[5]]
    		df_hist[2, 5] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            ##########################################
    		########################################## colder total
    		# gen 1
    		data_gen = data_sub_cold_total[ data_sub_cold_total$CumulativeDDF>=gen_borders[1] & data_sub_cold_total$CumulativeDDF<gen_borders[2]]
    		df_hist[3, 2] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
    		
    		# gen 2
    		data_gen = data_sub_cold_total[ data_sub_cold_total$CumulativeDDF>=gen_borders[2] & data_sub_cold_total$CumulativeDDF<gen_borders[3]]
    		df_hist[3, 3] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
    		
    		# gen 3
    		data_gen = data_sub_cold_total[ data_sub_cold_total$CumulativeDDF>=gen_borders[3] & data_sub_cold_total$CumulativeDDF<gen_borders[4]]
    		df_hist[3, 4] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
    		
    		# gen 4
    		data_gen = data_sub_cold_total[ data_sub_cold_total$CumulativeDDF>=gen_borders[4] & data_sub_cold_total$CumulativeDDF<=gen_borders[5]]
    		df_hist[3, 5] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
    		##########################################
    		########################################## colder escape
    		# gen 1
            data_gen = data_sub_cold_escap[ data_sub_cold_escap$CumulativeDDF>=gen_borders[1] & data_sub_cold_escap$CumulativeDDF<gen_borders[2]]
            df_hist[4, 2] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 2
            data_gen = data_sub_cold_escap[ data_sub_cold_escap$CumulativeDDF>=gen_borders[2] & data_sub_cold_escap$CumulativeDDF<gen_borders[3]]
            df_hist[4, 3] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 3
            data_gen = data_sub_cold_escap[ data_sub_cold_escap$CumulativeDDF>=gen_borders[3] & data_sub_cold_escap$CumulativeDDF<gen_borders[4]]
            df_hist[4, 4] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 4
            data_gen = data_sub_cold_escap[ data_sub_cold_escap$CumulativeDDF>=gen_borders[4] & data_sub_cold_escap$CumulativeDDF<=gen_borders[5]]
            df_hist[4, 5] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)

    	} else if (time_p == "2040's"){
    		##########################################
    		########################################## warmer total
    		# gen 1
    		data_gen = data_sub_warm_total[ data_sub_warm_total$CumulativeDDF>=gen_borders[1] & data_sub_warm_total$CumulativeDDF<gen_borders[2]]
    		df_2040[1, 2] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
    		
    		# gen 2
    		data_gen = data_sub_warm_total[ data_sub_warm_total$CumulativeDDF>=gen_borders[2] & data_sub_warm_total$CumulativeDDF<gen_borders[3]]
    		df_2040[1, 3] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
    		
    		# gen 3
    		data_gen = data_sub_warm_total[ data_sub_warm_total$CumulativeDDF>=gen_borders[3] & data_sub_warm_total$CumulativeDDF<gen_borders[4]]
    		df_2040[1, 4] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
    		
    		# gen 4
    		data_gen = data_sub_warm_total[ data_sub_warm_total$CumulativeDDF>=gen_borders[4] & data_sub_warm_total$CumulativeDDF<=gen_borders[5]]
    		df_2040[1, 5] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            ##########################################
    		########################################## warmer escape
    		# gen 1
    		data_gen = data_sub_warm_escap[data_sub_warm_escap$CumulativeDDF>=gen_borders[1] & data_sub_warm_escap$CumulativeDDF<gen_borders[2]]
    		df_2040[2, 2] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
    		
    		# gen 2
    		data_gen = data_sub_warm_escap[data_sub_warm_escap$CumulativeDDF>=gen_borders[2] & data_sub_warm_escap$CumulativeDDF<gen_borders[3]]
    		df_2040[2, 3] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
    		
    		# gen 3
    		data_gen = data_sub_warm_escap[data_sub_warm_escap$CumulativeDDF>=gen_borders[3] & data_sub_warm_escap$CumulativeDDF<gen_borders[4]]
    		df_2040[2, 4] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
    		
    		# gen 4
    		data_gen = data_sub_warm_escap[data_sub_warm_escap$CumulativeDDF>=gen_borders[4] & data_sub_warm_escap$CumulativeDDF<=gen_borders[5]]
    		df_2040[2, 5] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            ##########################################
    		########################################## colder total
    		# gen 1
    		data_gen = data_sub_cold_total[ data_sub_cold_total$CumulativeDDF>=gen_borders[1] & data_sub_cold_total$CumulativeDDF<gen_borders[2]]
    		df_2040[3, 2] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
    		
    		# gen 2
    		data_gen = data_sub_cold_total[ data_sub_cold_total$CumulativeDDF>=gen_borders[2] & data_sub_cold_total$CumulativeDDF<gen_borders[3]]
    		df_2040[3, 3] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
    		
    		# gen 3
    		data_gen = data_sub_cold_total[ data_sub_cold_total$CumulativeDDF>=gen_borders[3] & data_sub_cold_total$CumulativeDDF<gen_borders[4]]
    		df_2040[3, 4] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
    		
    		# gen 4
    		data_gen = data_sub_cold_total[ data_sub_cold_total$CumulativeDDF>=gen_borders[4] & data_sub_cold_total$CumulativeDDF<=gen_borders[5]]
    		df_2040[3, 5] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
    		##########################################
    		########################################## colder escape
    		# gen 1
            data_gen = data_sub_cold_escap[ data_sub_cold_escap$CumulativeDDF>=gen_borders[1] & data_sub_cold_escap$CumulativeDDF<gen_borders[2]]
            df_2040[4, 2] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 2
            data_gen = data_sub_cold_escap[ data_sub_cold_escap$CumulativeDDF>=gen_borders[2] & data_sub_cold_escap$CumulativeDDF<gen_borders[3]]
            df_2040[4, 3] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 3
            data_gen = data_sub_cold_escap[ data_sub_cold_escap$CumulativeDDF>=gen_borders[3] & data_sub_cold_escap$CumulativeDDF<gen_borders[4]]
            df_2040[4, 4] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 4
            data_gen = data_sub_cold_escap[ data_sub_cold_escap$CumulativeDDF>=gen_borders[4] & data_sub_cold_escap$CumulativeDDF<=gen_borders[5]]
            df_2040[4, 5] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)

    	} else if (time_p == "2060's"){
    		##########################################
            ########################################## warmer total
            # gen 1
            data_gen = data_sub_warm_total[ data_sub_warm_total$CumulativeDDF>=gen_borders[1] & data_sub_warm_total$CumulativeDDF<gen_borders[2]]
            df_2060[1, 2] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 2
            data_gen = data_sub_warm_total[ data_sub_warm_total$CumulativeDDF>=gen_borders[2] & data_sub_warm_total$CumulativeDDF<gen_borders[3]]
            df_2060[1, 3] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 3
            data_gen = data_sub_warm_total[ data_sub_warm_total$CumulativeDDF>=gen_borders[3] & data_sub_warm_total$CumulativeDDF<gen_borders[4]]
            df_2060[1, 4] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 4
            data_gen = data_sub_warm_total[ data_sub_warm_total$CumulativeDDF>=gen_borders[4] & data_sub_warm_total$CumulativeDDF<=gen_borders[5]]
            df_2060[1, 5] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            ##########################################
            ########################################## warmer escape
            # gen 1
            data_gen = data_sub_warm_escap[data_sub_warm_escap$CumulativeDDF>=gen_borders[1] & data_sub_warm_escap$CumulativeDDF<gen_borders[2]]
            df_2060[2, 2] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 2
            data_gen = data_sub_warm_escap[data_sub_warm_escap$CumulativeDDF>=gen_borders[2] & data_sub_warm_escap$CumulativeDDF<gen_borders[3]]
            df_2060[2, 3] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 3
            data_gen = data_sub_warm_escap[data_sub_warm_escap$CumulativeDDF>=gen_borders[3] & data_sub_warm_escap$CumulativeDDF<gen_borders[4]]
            df_2060[2, 4] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 4
            data_gen = data_sub_warm_escap[data_sub_warm_escap$CumulativeDDF>=gen_borders[4] & data_sub_warm_escap$CumulativeDDF<=gen_borders[5]]
            df_2060[2, 5] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            ##########################################
            ########################################## colder total
            # gen 1
            data_gen = data_sub_cold_total[ data_sub_cold_total$CumulativeDDF>=gen_borders[1] & data_sub_cold_total$CumulativeDDF<gen_borders[2]]
            df_2060[3, 2] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 2
            data_gen = data_sub_cold_total[ data_sub_cold_total$CumulativeDDF>=gen_borders[2] & data_sub_cold_total$CumulativeDDF<gen_borders[3]]
            df_2060[3, 3] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 3
            data_gen = data_sub_cold_total[ data_sub_cold_total$CumulativeDDF>=gen_borders[3] & data_sub_cold_total$CumulativeDDF<gen_borders[4]]
            df_2060[3, 4] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 4
            data_gen = data_sub_cold_total[ data_sub_cold_total$CumulativeDDF>=gen_borders[4] & data_sub_cold_total$CumulativeDDF<=gen_borders[5]]
            df_2060[3, 5] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            ##########################################
            ########################################## colder escape
            # gen 1
            data_gen = data_sub_cold_escap[ data_sub_cold_escap$CumulativeDDF>=gen_borders[1] & data_sub_cold_escap$CumulativeDDF<gen_borders[2]]
            df_2060[4, 2] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 2
            data_gen = data_sub_cold_escap[ data_sub_cold_escap$CumulativeDDF>=gen_borders[2] & data_sub_cold_escap$CumulativeDDF<gen_borders[3]]
            df_2060[4, 3] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 3
            data_gen = data_sub_cold_escap[ data_sub_cold_escap$CumulativeDDF>=gen_borders[3] & data_sub_cold_escap$CumulativeDDF<gen_borders[4]]
            df_2060[4, 4] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 4
            data_gen = data_sub_cold_escap[ data_sub_cold_escap$CumulativeDDF>=gen_borders[4] & data_sub_cold_escap$CumulativeDDF<=gen_borders[5]]
            df_2060[4, 5] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)

    	} else if (time_p == "2080's"){
    		##########################################
            ########################################## warmer total
            # gen 1
            data_gen = data_sub_warm_total[ data_sub_warm_total$CumulativeDDF>=gen_borders[1] & data_sub_warm_total$CumulativeDDF<gen_borders[2]]
            df_2080[1, 2] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 2
            data_gen = data_sub_warm_total[ data_sub_warm_total$CumulativeDDF>=gen_borders[2] & data_sub_warm_total$CumulativeDDF<gen_borders[3]]
            df_2080[1, 3] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 3
            data_gen = data_sub_warm_total[ data_sub_warm_total$CumulativeDDF>=gen_borders[3] & data_sub_warm_total$CumulativeDDF<gen_borders[4]]
            df_2080[1, 4] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 4
            data_gen = data_sub_warm_total[ data_sub_warm_total$CumulativeDDF>=gen_borders[4] & data_sub_warm_total$CumulativeDDF<=gen_borders[5]]
            df_2080[1, 5] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            ##########################################
            ########################################## warmer escape
            # gen 1
            data_gen = data_sub_warm_escap[data_sub_warm_escap$CumulativeDDF>=gen_borders[1] & data_sub_warm_escap$CumulativeDDF<gen_borders[2]]
            df_2080[2, 2] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 2
            data_gen = data_sub_warm_escap[data_sub_warm_escap$CumulativeDDF>=gen_borders[2] & data_sub_warm_escap$CumulativeDDF<gen_borders[3]]
            df_2080[2, 3] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 3
            data_gen = data_sub_warm_escap[data_sub_warm_escap$CumulativeDDF>=gen_borders[3] & data_sub_warm_escap$CumulativeDDF<gen_borders[4]]
            df_2080[2, 4] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 4
            data_gen = data_sub_warm_escap[data_sub_warm_escap$CumulativeDDF>=gen_borders[4] & data_sub_warm_escap$CumulativeDDF<=gen_borders[5]]
            df_2080[2, 5] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            ##########################################
            ########################################## colder total
            # gen 1
            data_gen = data_sub_cold_total[ data_sub_cold_total$CumulativeDDF>=gen_borders[1] & data_sub_cold_total$CumulativeDDF<gen_borders[2]]
            df_2080[3, 2] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 2
            data_gen = data_sub_cold_total[ data_sub_cold_total$CumulativeDDF>=gen_borders[2] & data_sub_cold_total$CumulativeDDF<gen_borders[3]]
            df_2080[3, 3] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 3
            data_gen = data_sub_cold_total[ data_sub_cold_total$CumulativeDDF>=gen_borders[3] & data_sub_cold_total$CumulativeDDF<gen_borders[4]]
            df_2080[3, 4] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 4
            data_gen = data_sub_cold_total[ data_sub_cold_total$CumulativeDDF>=gen_borders[4] & data_sub_cold_total$CumulativeDDF<=gen_borders[5]]
            df_2080[3, 5] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            ##########################################
            ########################################## colder escape
            # gen 1
            data_gen = data_sub_cold_escap[ data_sub_cold_escap$CumulativeDDF>=gen_borders[1] & data_sub_cold_escap$CumulativeDDF<gen_borders[2]]
            df_2080[4, 2] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 2
            data_gen = data_sub_cold_escap[ data_sub_cold_escap$CumulativeDDF>=gen_borders[2] & data_sub_cold_escap$CumulativeDDF<gen_borders[3]]
            df_2080[4, 3] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 3
            data_gen = data_sub_cold_escap[ data_sub_cold_escap$CumulativeDDF>=gen_borders[3] & data_sub_cold_escap$CumulativeDDF<gen_borders[4]]
            df_2080[4, 4] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
            
            # gen 4
            data_gen = data_sub_cold_escap[ data_sub_cold_escap$CumulativeDDF>=gen_borders[4] & data_sub_cold_escap$CumulativeDDF<=gen_borders[5]]
            df_2080[4, 5] = auc(x=data_gen$CumulativeDDF, y=data_gen$value)
    	}
    }

}









