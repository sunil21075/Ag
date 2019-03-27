####################################################################
##                                                                ##
##            Read the data off Jenny's data                      ##
##            and get the percipitation.                          ##
##                                                                ##
####################################################################
##    The modeled historical is in 
##               /data/hydro/jennylabcommon2/metdata/maca_v2_vic_binary/
##    modeled historical is equivalent to 
##    having 4 variables, and years 1950-2005
##
##    The observed historical is in 
##    /data/hydro/jennylabcommon2/metdata/historical/UI_historical/VIC_Binary_CONUS_to_2016
##    observed historical is equivalent to 
##    having 8 variables, and years 1979-2016
##
options(digits=9)
options(digit=9)
####################################################################
##                                                                ##
##                                                                ##
##                         find analogs                           ##
##                                                                ##
##                                                                ##
####################################################################

########################################################################
#
#                                 Mahony Style
# 
########################################################################
find_NN_info_W4G_ICV <- function(ICV, historical_dt, future_dt, n_neighbors){
  # This is modification of find_NN_info_W4G
  # where we add ICV matrix which in our case is 
  # the same as historical_dt, however, when we do averages,
  # then SD's will be NA for just one input!

  # remove extra columns
  if ("treatment" %in% colnames(historical_dt)) {historical_dt <- within(historical_dt, remove(treatment))}
  if ("treatment" %in% colnames(future_dt)) {future_dt <- within(future_dt, remove(treatment))}
  if ("treatment" %in% colnames(ICV)) {ICV <- within(ICV, remove(treatment))}

  # sort the columns of data tables so they both have the same order, if they do not.
  columns_ord <- c("year", "location", "ClimateScenario",
                   "medianDoY", "NumLarvaGens_Aug", 
                   "mean_escaped_Gen1", "mean_escaped_Gen2",
                   "mean_escaped_Gen3",
                   "mean_gdd", "mean_precip")# , "mean_escaped_Gen4",
  setcolorder(historical_dt, columns_ord)
  setcolorder(future_dt, columns_ord)
  setcolorder(ICV, columns_ord)

  all_us_locs <- unique(historical_dt$location)
  
  # 9 local locations are not in the all_us data!!!
  local_locations <- unique(future_dt$location)
  local_locations <- local_locations[which(local_locations %in% all_us_locs)] 
  future_dt <- future_dt %>% filter(location %in% local_locations)

  future_years <- unique(future_dt$year)
  trunc.SDs <- 0.1 # Principal component truncation rule

  A <- as.data.frame(historical_dt)
  B <- as.data.frame(future_dt)
  C <- as.data.frame(ICV)
  # rm (historical_dt, future_dt, ICV)
  numeric_cols <- c("medianDoY", "NumLarvaGens_Aug", "mean_escaped_Gen1", 
                    "mean_escaped_Gen2", "mean_escaped_Gen3", 
                    "mean_gdd", "mean_precip") # , "mean_escaped_Gen4", 

  non_numeric_cols <- c("year", "location", "ClimateScenario")

  NN_dist_tb <- data.table()
  NNs_loc_year_tb <- data.table()
  NN_sigma_tb <- data.table()

  for (loc in local_locations){
    # loc = local_locations[1]
    Bj <- B %>% filter(location==loc)
    Cj <- C %>% filter(location==loc)

    # standard deviation of 1951-1990 interannual variability in each climate 
    # variable, ignoring missing years
    Cj.sd <- apply(Cj[, numeric_cols], MARGIN=2, FUN = sd, na.rm = T)
    Cj.sd[Cj.sd<(10^-10)] = 1

    A_prime <- A
    A_prime[, numeric_cols] <- sweep(A_prime[, numeric_cols], MARGIN = 2, STATS = Cj.sd, FUN = `/`) 
    
    # standardize the analog pool
    Bj_prime <- Bj
    Bj_prime[, numeric_cols] <- sweep(Bj_prime[, numeric_cols], MARGIN = 2, STATS = Cj.sd, FUN = `/`)

    # standardize the reference ICV
    Cj_prime <- Cj
    Cj_prime[, numeric_cols] <- sweep(Cj_prime[, numeric_cols], MARGIN = 2, STATS = Cj.sd, FUN = `/`)

    ## Step 2: Extract the principal components (PCs) of 
    ##         the reference period ICV and project all data onto these PCs

    # Principal components analysis. The !is.na(apply(...)) 
    # term is there simply to select all years with complete observations in all variables. 
    #  ZZ[!is.na(apply(ZZ, 1, mean)) ,] selects the rows whose mean is not NaN
    PCA <- prcomp(Cj_prime[, numeric_cols][!is.na(apply(Cj_prime[, numeric_cols], 1, mean)) ,])

    # find the number of PCs to retain using the PC truncation
    # rule of eigenvector stdev > the truncation threshold
    PCs <- max(which(unlist(summary(PCA)[1])>trunc.SDs))

    # project the reference ICV onto the PCs which is the same as analog pool:
    X <- as.data.frame(predict(PCA, A_prime))
    X <- cbind(A_prime[, non_numeric_cols], X)

    # project the projected future conditions onto the PCs
    Yj <- as.data.frame(predict(PCA, Bj_prime[, numeric_cols]))
    Yj <- cbind(Bj_prime[, non_numeric_cols], Yj)

    Zj <- as.data.frame(predict(PCA, Cj_prime[, numeric_cols]))
    Zj <- cbind(Cj_prime[, non_numeric_cols], Zj)

    ## Step 3a: express PC scores as standardized
    #           anomalies of reference interannual variability
      
    # standard deviation of 1951-1990 interannual 
    # variability in each principal component, ignoring missing years
    Zj_sd <- apply(Zj[, 4:(PCs + 3)], MARGIN = 2, FUN = sd, na.rm=T)

    # standardize the analog pool   
    X_prime <- sweep(X[, 4:(PCs + 3)], MARGIN=2, Zj_sd, FUN = `/`)
    X_prime <- cbind(X[, non_numeric_cols], X_prime)

    # standardize the projected conditions
    Yj_prime <- sweep(Yj[, 4:(PCs + 3)], MARGIN=2, Zj_sd, FUN = `/`)
    Yj_prime <- cbind(Yj[, non_numeric_cols], Yj_prime)
    
    NN_list <- get.knnx(data = X_prime[, 4:(PCs+3)], 
                        query= Yj_prime[, 4:(PCs+3)], 
                        k=n_neighbors, algorithm="brute")
    #
    # Step 3: find sigma dissimilarities
    ############################################################
    #
    # This is for one location, and different years. 
    # So, the location column and ClimateScenario can be dropped.
    # We already know what they are from loc and the input file.
    #
    ############################################################
    # percentile of the nearest neighbour distance on the chi distribution with
    # degrees of freedom equaling the dimensionality of the distance measurement (PCs)
    NN_chi <- pchi(as.vector(NN_list$nn.dist), PCs)
    
    # values of the chi percentiles on a standard half-normal distribution
    # (chi distribution with one degree of freedom)
    # NN.sigma[which(proxy==j)] <- qchi(NN.chi, 1)

    NN_sigma <- qchi(NN_chi, 1)

    ########################################################################
    #
    # extract the rows corresponding to nearest neighbors indices
    # 
    ########################################################################
    NN_idx <- NN_list$nn.index
    NN_dist <- NN_list$nn.dist %>% data.table()
    
    NNs_loc_year <- X_prime[as.vector(NN_idx), c('year', 'location')]
    #
    # reshape the long list to wide data table
    # currenty the data are in vector form, by reshaping
    # we will have one row of NNs per sample point
    #
    NNs_loc_year <- Reduce(cbind, 
                           split(NNs_loc_year, 
                                 rep(1:n_neighbors, each=(nrow(NNs_loc_year)/n_neighbors)))) %>% 
                    data.table()

    NN_sigma <- Reduce(cbind, 
                       split(NN_sigma, 
                             rep(1:n_neighbors, each=(length(NN_sigma)/n_neighbors)))) %>% data.table()
    #
    # rename columns
    #
    names(NN_dist) <- paste0("NN_", c(1:n_neighbors))
    names(NNs_loc_year) <- paste0(names(NNs_loc_year), paste0("_NN_", rep(1:n_neighbors, each=2)))
    names(NN_sigma) <- paste0("sigma_NN_", c(1:n_neighbors))

    NN_dist <- cbind(Yj[, c("year", "location")], NN_dist)
    NNs_loc_year <- cbind(Yj[, c("year", "location")], NNs_loc_year)
    NN_sigma <- cbind(Yj[, c("year", "location")], NN_sigma)
    
    NN_dist_tb <- rbind(NN_dist_tb, NN_dist)
    NN_sigma_tb <- rbind(NN_sigma_tb, NN_sigma)
    NNs_loc_year_tb <- rbind(NNs_loc_year_tb, NNs_loc_year)
    
    rm(NN_dist, NNs_loc_year, NN_sigma)
  }
  ########################################################################
  # For some bizzare reason, order of columns are random
  # when I run it on Aeolus. We fix the order of columns here
  ########################################################################
  distance_col_names <- c("year", "location", paste0("NN_", c(1:n_neighbors)))
  LL = (ncol(NNs_loc_year_tb)-2)/2
  v = rep(c("year_NN_", "location_NN_"), LL); w = rep(1:LL, each = 2);
  loc_year_col_names <- c("year", "location", paste0(v, w))
  
  sigma_df_col_names <- c("year", "location", paste0("sigma_NN_", c(1:n_neighbors)))
  
  setcolorder(NN_dist_tb, distance_col_names)
  setcolorder(NNs_loc_year_tb, loc_year_col_names)
  setcolorder(NN_sigma_tb, sigma_df_col_names)
  #############################################
  return(list(NN_dist_tb, NNs_loc_year_tb, NN_sigma_tb, 
              colnames(NN_dist_tb), colnames(NNs_loc_year_tb), colnames(NN_sigma_tb)))
}
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################

find_NN_info_W4G_ICV_2Loops_STOPWROKING <- function(ICV, historical_dt, future_dt, n_neighbors=50){
  # This is modification of find_NN_info_W4G
  # where we add ICV matrix which in our case is 
  # the same as historical_dt, however, when we do averages,
  # then SD's will be NA for just one input!

  # remove extra columns
  if ("treatment" %in% colnames(historical_dt)) {historical_dt <- within(historical_dt, remove(treatment))}
  if ("treatment" %in% colnames(future_dt)) {future_dt <- within(future_dt, remove(treatment))}
  if ("treatment" %in% colnames(ICV)) {ICV <- within(ICV, remove(treatment))}

  # sort the columns of data tables so they both have the same order, if they do not.
  columns_ord <- c("year", "location", "ClimateScenario",
                   "medianDoY", "NumLarvaGens_Aug", 
                   "mean_escaped_Gen1", "mean_escaped_Gen2",
                   "mean_escaped_Gen3",
                   "mean_gdd", "mean_precip") #  "mean_escaped_Gen4",
  setcolorder(historical_dt, columns_ord)
  setcolorder(future_dt, columns_ord)
  setcolorder(ICV, columns_ord)

  all_us_locs <- unique(historical_dt$location)
  
  # 9 local locations are not in the all_us data!!!
  local_locations <- unique(future_dt$location)
  local_locations <- local_locations[which(local_locations %in% all_us_locs)] 
  future_dt <- future_dt %>% filter(location %in% local_locations)

  future_years <- unique(future_dt$year)

  trunc.SDs <- 0.1 # Principal component truncation rule

  A <- as.data.frame(historical_dt)
  B <- as.data.frame(future_dt)
  C <- as.data.frame(ICV)
  # rm (historical_dt, future_dt, ICV)
  numeric_cols <- c("medianDoY", "NumLarvaGens_Aug", "mean_escaped_Gen1", 
                    "mean_escaped_Gen2", "mean_escaped_Gen3",
                    "mean_gdd", "mean_precip") #  "mean_escaped_Gen4",

  non_numeric_cols <- c("year", "location", "ClimateScenario")

  NN_dist_tb <- data.table()
  NNs_loc_year_tb <- data.table()
  NN_sigma_tb <- data.table()

  for (loc in local_locations){
    Bj <- B %>% filter(location==loc)
    Cj <- C %>% filter(location==loc)

    # standard deviation of 1951-1990 interannual variability in each climate 
    # variable, ignoring missing years
    Cj.sd <- apply(Cj[, numeric_cols], MARGIN=2, FUN = sd, na.rm = T)
    Cj.sd[Cj.sd<(10^-10)] = 1

    A_prime <- A
    A_prime[, numeric_cols] <- sweep(A_prime[, numeric_cols], MARGIN = 2, STATS = Cj.sd, FUN = `/`) 
    
    # standardize the analog pool
    Bj_prime <- Bj
    Bj_prime[, numeric_cols] <- sweep(Bj_prime[, numeric_cols], MARGIN = 2, STATS = Cj.sd, FUN = `/`)

    # standardize the reference ICV
    Cj_prime <- Cj
    Cj_prime[, numeric_cols] <- sweep(Cj_prime[, numeric_cols], MARGIN = 2, STATS = Cj.sd, FUN = `/`)

    ## Step 2: Extract the principal components (PCs) of 
    ##         the reference period ICV and project all data onto these PCs

    # Principal components analysis. The !is.na(apply(...)) 
    # term is there simply to select all years with complete observations in all variables. 
    #  ZZ[!is.na(apply(ZZ, 1, mean)) ,] selects the rows whose mean is not NaN
    PCA <- prcomp(Cj_prime[, numeric_cols][!is.na(apply(Cj_prime[, numeric_cols], 1, mean)) ,])

    # find the number of PCs to retain using the PC truncation
    # rule of eigenvector stdev > the truncation threshold
    PCs <- max(which(unlist(summary(PCA)[1])>trunc.SDs))

    # project the reference ICV onto the PCs which is the same as analog pool:
    X <- as.data.frame(predict(PCA, A_prime))
    X <- cbind(A_prime[, non_numeric_cols], X)

    # project the projected future conditions onto the PCs
    Yj <- as.data.frame(predict(PCA, Bj_prime[, numeric_cols]))
    Yj <- cbind(Bj_prime[, non_numeric_cols], Yj)

    Zj <- as.data.frame(predict(PCA, Cj_prime[, numeric_cols]))
    Zj <- cbind(Cj_prime[, non_numeric_cols], Zj)

    ## Step 3a: express PC scores as standardized
    #           anomalies of reference interannual variability
      
    # standard deviation of 1951-1990 interannual 
    # variability in each principal component, ignoring missing years
    Zj_sd <- apply(Zj[, 4:(PCs + 3)], MARGIN = 2, FUN = sd, na.rm=T)

    # standardize the analog pool   
    X_prime <- sweep(X[, 4:(PCs + 3)], MARGIN=2, Zj_sd, FUN = `/`)
    X_prime <- cbind(X[, non_numeric_cols], X_prime)

    # standardize the projected conditions
    Yj_prime <- sweep(Yj[, 4:(PCs + 3)], MARGIN=2, Zj_sd, FUN = `/`)
    Yj_prime <- cbind(Yj[, non_numeric_cols], Yj_prime)

    for (yr in future_years){
      qry <- Yj_prime %>% filter(year == yr)

      NN_list <- get.knnx(data = X_prime[, 4:(PCs+3)], 
                          query= qry[, 4:(PCs+3)], 
                          k=n_neighbors, algorithm="brute")

      # Step 3: find sigma dissimilarities
      # percentile of the nearest neighbour distance on the chi distribution with
      # degrees of freedom equaling the dimensionality of the distance measurement (PCs)
      NN_chi <- pchi(as.vector(NN_list$nn.dist), PCs)
      
      # values of the chi percentiles on a standard half-normal distribution
      # (chi distribution with one degree of freedom)
      # NN.sigma[which(proxy==j)] <- qchi(NN.chi, 1)
      NN_sigma <- qchi(NN_chi, 1)

     ########################################################################
     #
     # extract the rows corresponding to nearest neighbors indices
     # 
     ########################################################################
     NN_idx <- NN_list$nn.index
     NN_dist <- NN_list$nn.dist %>% data.table()
    
     NNs_loc_year <- X_prime[as.vector(NN_idx), c('year', 'location')]
     #
     # reshape the long list to wide data table
     #
     NNs_loc_year <- Reduce(cbind, split(NNs_loc_year,
                                         rep(1:n_neighbors, 
                                         each=(nrow(NNs_loc_year)/n_neighbors)))) %>% data.table()

     NN_sigma <- Reduce(cbind, split(NN_sigma, 
                                    rep(1:n_neighbors, 
                                    each=(length(NN_sigma)/n_neighbors)))) %>% data.table()
     #
     # rename columns
     #
     names(NN_dist) <- paste0("NN_", c(1:n_neighbors))
     names(NNs_loc_year) <- paste0(names(NNs_loc_year), paste0("_NN_", rep(1:n_neighbors, each=2)))
     names(NN_sigma) <- paste0("sigma_NN_", c(1:n_neighbors))

     NN_dist <- cbind(yr, loc, NN_dist)
     NNs_loc_year<- cbind(yr, loc, NNs_loc_year)
     NN_sigma <- cbind(yr, loc, NN_sigma)

     setnames(NN_dist,      old=c("yr","loc"), new=c("year", "location"))
     setnames(NNs_loc_year, old=c("yr","loc"), new=c("year", "location"))
     setnames(NN_sigma,  old=c("yr","loc"), new=c("year", "location"))
    
     NN_dist_tb <- rbind(NN_dist_tb, NN_dist)
     NNs_loc_year_tb <- rbind(NNs_loc_year_tb, NNs_loc_year)
     NN_sigma_tb <- rbind(NN_sigma_tb, NN_sigma)
     rm(NN_dist, NNs_loc_year, NN_sigma)
    }
  }
  ########################################################################
  # For some bizzare reason, order of columns are random
  # when I run it on Aeolus. We fix the order of columns here
  ########################################################################
  
  # distance_col_names <- c("year", "location", paste0("NN_", c(1:n_neighbors)))
  
  # LL = (ncol(NNs_loc_year_tb)-2)/2
  # v = rep(c("year_NN_", "location_NN_"), LL); w = rep(1:LL, each = 2);
  # loc_year_col_names <- c("year", "location", paste0(v, w))
  
  # sigma_df_col_names <- c("year", "location", paste0("sigma_NN_", c(1:n_neighbors)))
  
  # setcolorder(NN_dist_tb, distance_col_names)
  # setcolorder(NNs_loc_year_tb, loc_year_col_names)
  # setcolorder(NN_sigma_tb, sigma_df_col_names)
  #############################################
  return(list(NN_dist_tb, NNs_loc_year_tb, NN_sigma_tb, 
              colnames(NN_dist_tb), colnames(NNs_loc_year_tb), colnames(NN_sigma_tb)))
}
########################################################################
#
#                                 MatchIt
# 
########################################################################
sort_matchit_out <- function(base, usa, m_method, m_distance, m_ratio, precip){
  if (dim(base)[2] != dim(usa)[2]){
    print ("The number of columns do not agree!")
    stopifnot(dim(base)[2] == dim(usa)[2])
   } else {
    binded_dt <- rbind(usa, base)
  }
  # add row numbers to data, so we can check
  # the order of output we get is correct
  # binded_dt$row_ID = seq(1, dim(binded_dt)[1], 1)

  m_out <- list_by_dist_1_to_all(binded_dt, m_method, m_distance, m_ratio, precip)
  
  if (m_method=="nearest"){
    matched_rows <- as.integer(m_out$match.matrix)
    matched_frame <- binded_dt[matched_rows, ]
    matched_frame <- rbind(matched_frame, base)
  }
  return(matched_frame)
}

list_by_dist_1_to_all <- function(binded, 
                                  m_method, 
                                  m_distance = "default", 
                                  m_ratio = 500, 
                                  precip = FALSE){
  # List the historical locations
  # according to increasing distance 
  # from a given locaion data in a certain year: base
  #
  # inputs: 
  #          base: a row vector of size 1-by-n_vars of future data, one location, one year
  #          usa: matrix of all observed data across USA.
  #          m_method: Matching method: could be exact, subclass, nearest ~= optimal, full
  #                    suggestion: NN, full, optimal
  #          m_option: When you choose nearest then option can be set to subclass.
  #          m_distance: distance metric:logit
  #          precipitation: TRUE or FALSE (include it or exclude it)
  #
  # output:  a matchit object
  if (m_method == "nearest"){
    if (precip == TRUE){
      if (m_distance != "default"){
        match_out <- matchit(formula = treatment ~ medianDoY + NumLarvaGens_Aug + 
                                                   mean_escaped_Gen1 + mean_escaped_Gen2 + 
                                                   mean_escaped_Gen3 + mean_escaped_Gen4 +
                                                   mean_gdd + mean_precip,
                              method=m_method, 
                              ratio = m_ratio, 
                              data = binded, 
                              distance = m_distance)
        
        } else {
          match_out <- matchit(formula = treatment ~ medianDoY + NumLarvaGens_Aug + 
                                                     mean_escaped_Gen1 + mean_escaped_Gen2 + 
                                                     mean_escaped_Gen3 + mean_escaped_Gen4 +
                                                     mean_gdd + mean_precip,
                              method=m_method, 
                              ratio = m_ratio, 
                              data = binded)
      }
     } else {
      if (m_distance != "default"){
        match_out <- matchit(formula = treatment ~ medianDoY + NumLarvaGens_Aug + 
                                                   mean_escaped_Gen1 + mean_escaped_Gen2 + 
                                                   mean_escaped_Gen3 + mean_escaped_Gen4 +
                                                   mean_gdd,
                              method=m_method, 
                              ratio = m_ratio, 
                              data = binded,
                              distance = m_distance)
        
        } else {
          match_out <- matchit(formula = treatment ~ medianDoY + NumLarvaGens_Aug + 
                                                     mean_escaped_Gen1 + mean_escaped_Gen2 + 
                                                     mean_escaped_Gen3 + mean_escaped_Gen4 +
                                                     mean_gdd,
                              method=m_method, 
                              ratio = m_ratio, 
                              data = binded)
      }
    }
  }

  if (m_method == "full"){
    if (precip == TRUE){
      if (m_distance != "default"){
        match_out <- matchit(formula = treatment ~ medianDoY + NumLarvaGens_Aug + 
                                                   mean_escaped_Gen1 + mean_escaped_Gen2 + 
                                                   mean_escaped_Gen3 + mean_escaped_Gen4 +
                                                   mean_gdd + mean_precip,
                              method=m_method, 
                              min.controls = m_ratio, 
                              data = binded, 
                              distance = m_distance)
        
        } else {
          match_out <- matchit(formula = treatment ~ medianDoY + NumLarvaGens_Aug + 
                                                     mean_escaped_Gen1 + mean_escaped_Gen2 + 
                                                     mean_escaped_Gen3 + mean_escaped_Gen4 +
                                                     mean_gdd + mean_precip,
                              method=m_method, 
                              min.controls = m_ratio, 
                              data = binded)
      }
     } else {
      if (m_distance != "default"){
        match_out <- matchit(formula = treatment ~ medianDoY + NumLarvaGens_Aug + 
                                                   mean_escaped_Gen1 + mean_escaped_Gen2 + 
                                                   mean_escaped_Gen3 + mean_escaped_Gen4 +
                                                   mean_gdd,
                              method=m_method, 
                              min.controls = m_ratio, 
                              data = binded,
                              distance = m_distance)
        
        } else {
          match_out <- matchit(formula = treatment ~ medianDoY + NumLarvaGens_Aug + 
                                                     mean_escaped_Gen1 + mean_escaped_Gen2 + 
                                                     mean_escaped_Gen3 + mean_escaped_Gen4 +
                                                     mean_gdd,
                              method=m_method, 
                              min.controls = m_ratio, 
                              data = binded)
      }
    }
  }
  return(match_out)
}

####################################################################
##                                                                ##
##            Read the data off the laptop                        ##
##            and clean them so they are ready                    ##
##            to be used to produce features.                     ##
##                                                                ##
####################################################################

# create number of generations by Aug 23, and Nov 5th
# the data is on the computer for this.
generate_no_generations <- function(input_dir, file_name, stage="Larva", dead_line="Aug", version){
  file_name <- paste0(input_dir, file_name)
  data <- data.table(readRDS(file_name))
  if (stage == "Larva"){
      var = "NumLarvaGens"
     } else if (stage == "Adult"){
      var = "NumAdultGens"
  }
  ############################################
  ##
  ## Clean data, we just need future data 
  ## for local stuff.
  ##
  ############################################
  # some how there is no 2025 in the data!
  data <- clean_gens_files(data, var)
  colnames(data)[colnames(data) == var] <- paste0(var, "_", dead_line)
  return(data)
}

clean_gens_files <- function(data, var){
  # remove historical data
  data <- data %>% filter(year >= 2025 | year <= 2015)
  
  # remove historical level stuck in ClimateScenario
  data$ClimateScenario = factor(data$ClimateScenario)
    
  data$location = paste0(data$latitude, "_", data$longitude)
    
  data <- subset(data, select = c("year", "location", var, "ClimateScenario"))
  data = unique(data)
  return(data)
}
######################################################################
##                                                                    ##
##                 Generate the following features:                   ##
##                     1. Median DoY (First Flight)                   ##
##                     2. Pest risk for Gen 3/4, for 25/75 %          ##
##                                  (not any more)                    ##
##                     3. Fraction of escaped diapause for each Gen.  ##
##                     4. No. Generations.                            ##
##                     5. GDD accumulation.                           ##
##                     6. Precipitation.                              ##
######################################################################

#######
####### First Flight
#######
generate_mDoY_FF <- function(dt, meann = TRUE){  
  mDoY_FF <- clean_4_FF(dt)
  # mDoY_FF <- within(mDoY_FF, remove(ClimateScenario))
  if (meann ==TRUE) {
  	mDoY_FF = mDoY_FF[, .(medianDoY = as.integer(median(emergence))),
                          by = c("year", "location")]
   } else {
   	mDoY_FF = mDoY_FF[, .(medianDoY = as.integer(median(emergence))),
                        by = c("year", "location", "ClimateScenario")]
  }
  return(mDoY_FF)
}

clean_4_FF <- function(dt){
  if ("location" %in% colnames(dt)){
    print ("Hello there! from line 131 core of analog")
    } else {
      dt$location <- paste0(dt$latitude, "_", dt$longitude)
  }

  need_cols <- c("year", "location", "ClimateScenario", "Emergence")
  sub_Emerg <- subset(dt, !is.na("Emergence"), select = need_cols)
  sub_Emerg$ClimateScenario <- factor(sub_Emerg$ClimateScenario)
  colnames(sub_Emerg)[colnames(sub_Emerg) == "Emergence"] <- paste0("emergence")
  return(sub_Emerg)
}

###############################################################
######                                                   ######
######      Generate data for escaped diapause stuff     ######
######                                                   ######
###############################################################
gen_diap_map1_4_analog_Rel <- function(sub1, param_dir, time_type, CodMothParams_name){
  CodMothParams <- read.table(paste0(param_dir, CodMothParams_name), header=TRUE, sep=",")
  group_vec = c("latitude", "longitude", "ClimateScenario", "year")

  sub2 = sub1[, .(RelPctDiap=(auc(CumulativeDDF, RelDiap)/auc(CumulativeDDF,RelLarvaPop))*100, 
                  RelPctNonDiap = (auc(CumulativeDDF, RelNonDiap)/auc(CumulativeDDF, RelLarvaPop))*100), 
                  by=group_vec]

  sub2=merge(sub2, sub1[CumulativeDDF>=CodMothParams[5,5]&CumulativeDDF<CodMothParams[5,6],.(RelPctDiapGen1=(auc(CumulativeDDF,RelDiap)/auc(CumulativeDDF,RelLarvaPop))*100),by=group_vec],by=group_vec,all.x=T)
  sub2=merge(sub2, sub1[CumulativeDDF>=CodMothParams[6,5]&CumulativeDDF<CodMothParams[6,6],.(RelPctDiapGen2=(auc(CumulativeDDF,RelDiap)/auc(CumulativeDDF,RelLarvaPop))*100),by=group_vec],by=group_vec,all.x=T)
  sub2=merge(sub2, sub1[CumulativeDDF>=CodMothParams[7,5]&CumulativeDDF<CodMothParams[7,6],.(RelPctDiapGen3=(auc(CumulativeDDF,RelDiap)/auc(CumulativeDDF,RelLarvaPop))*100),by=group_vec],by=group_vec,all.x=T)
  sub2=merge(sub2, sub1[CumulativeDDF>=CodMothParams[8,5]&CumulativeDDF<CodMothParams[8,6],.(RelPctDiapGen4=(auc(CumulativeDDF,RelDiap)/auc(CumulativeDDF,RelLarvaPop))*100),by=group_vec],by=group_vec,all.x=T)
  sub2=merge(sub2, sub1[CumulativeDDF>=CodMothParams[5,5]&CumulativeDDF<CodMothParams[5,6],.(RelPctNonDiapGen1=(auc(CumulativeDDF,RelNonDiap)/auc(CumulativeDDF,RelLarvaPop))*100),by=group_vec],by=group_vec,all.x=T)
  sub2=merge(sub2, sub1[CumulativeDDF>=CodMothParams[6,5]&CumulativeDDF<CodMothParams[6,6],.(RelPctNonDiapGen2=(auc(CumulativeDDF,RelNonDiap)/auc(CumulativeDDF,RelLarvaPop))*100),by=group_vec],by=group_vec,all.x=T)
  sub2=merge(sub2, sub1[CumulativeDDF>=CodMothParams[7,5]&CumulativeDDF<CodMothParams[7,6],.(RelPctNonDiapGen3=(auc(CumulativeDDF,RelNonDiap)/auc(CumulativeDDF,RelLarvaPop))*100),by=group_vec],by=group_vec,all.x=T)
  sub2=merge(sub2, sub1[CumulativeDDF>=CodMothParams[8,5]&CumulativeDDF<CodMothParams[8,6],.(RelPctNonDiapGen4=(auc(CumulativeDDF,RelNonDiap)/auc(CumulativeDDF,RelLarvaPop))*100),by=group_vec],by=group_vec,all.x=T)
  
  # double check, but with high certaintly we do not need the first four lines above!
  sub2 <- within(sub2, remove("RelPctDiapGen1", "RelPctDiapGen2", "RelPctDiapGen3", "RelPctDiapGen4"))
  sub2$location <- paste0(sub2$latitude, "_", sub2$longitude)
  sub2 <- within(sub2, remove("latitude", "longitude"))

  sub2 <- subset(sub2, select=c("location", "year", 
                                "RelPctNonDiapGen1", "RelPctNonDiapGen2",
                                "RelPctNonDiapGen3", "RelPctNonDiapGen4"))
  return (sub2)
}

diap_map1_prep_4_analog_Rel <- function(input_dir, file_name, param_dir,  time_type){
  file_N = paste0(input_dir, file_name, ".rds")
  data <- data.table(readRDS(file_N))
  print (paste0("line 624"))
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
 
  # Gen 2
  sub[, LarvaGen2RelFraction := LarvaGen2/sum(LarvaGen2), 
        by = list(year, ClimateScenario, latitude, longitude)]

  # Gen 3
  sub[, LarvaGen3RelFraction := LarvaGen3/sum(LarvaGen3), 
        by =list(year,ClimateScenario, latitude, longitude)]

  # Gen 4
  sub[, LarvaGen4RelFraction := LarvaGen4/sum(LarvaGen4), 
        by =list(year, ClimateScenario, latitude, longitude)]

  sub = subset(sub, select = c("latitude", "longitude", 
                               "ClimateScenario",
                               "year", "dayofyear", "CumDDinF", 
                               "SumLarva", "enterDiap", 
                               "escapeDiap"))

  sub = sub[, .(RelLarvaPop = mean(SumLarva), 
  	            RelDiap = mean(enterDiap), 
                RelNonDiap = mean(escapeDiap), 
                CumulativeDDF = mean(CumDDinF)), 
             by = c("ClimateScenario",
                    "latitude", "longitude", 
                    "dayofyear", "year")]

  return (sub)
}
################################################################################
extract_gdd <- function(in_dir, file_name){
  dt <- data.table(readRDS(paste0(in_dir, file_name)))
  print ("line 247 of core, in extract_gdd functions")
  print (colnames(dt))
  if ("location" %in% colnames(dt)){
    print ("Hello there")
    } else {
    dt$location = paste0(dt$latitude, "_", dt$longitude)
  }

  # extract locations and years and the last day of the year
  # which has the last GDD
  dt <- dt %>% 
        group_by(location, year, ClimateScenario) %>%
        filter(month==12 & day==31) %>%
        data.table()
  dt <- subset(dt, select=c("year", "location", "CumDDinF", "ClimateScenario"))
  dt$ClimateScenario <- factor(dt$ClimateScenario) # get rid of historical level!
  return(dt)
}

###############################################################
###############################################################
######                                                   ######
######               combine precip data                 ######
######                                                   ######
###############################################################
merge_precip <- function(main_in_dir, location_type){
  if (location_type == "local"){
    models = c("bcc-csm1-1-m", "BNU-ESM", "CanESM2", "CNRM-CM5", "GFDL-ESM2G", "GFDL-ESM2M")
    carbon_types = c("rcp45", "rcp85")
    all_data_45 <- data.table()
    all_data_85 <- data.table()
    for (carbon in carbon_types){
      all_data <- data.table()
      for (model in models){
        curr_path = paste0(main_in_dir, model, "/", carbon, "/")
        # list of files in current folder
        files = list.files(path = curr_path, pattern = "data_")
        for (file in files){
          dt <- data.table(readRDS(paste0(curr_path, file)))
          dt <- dt %>% filter(year >= 2026 & year <= 2095)
          location = gsub("data_", "", file)
          location = gsub(".rds", "", location)
          dt$location <- location
          dt$ClimateScenario <- model
          all_data = rbind(all_data, dt)
        }
      }
    if (carbon == "rcp45"){ all_data_45 <- all_data } else if (carbon == "rcp85") { all_data_85 <- all_data}
    }
    return(list(all_data_45, all_data_85))
   } else if (location_type == "usa"){
    
    all_data = data.table()
    # list of files in current folder
    files = list.files(path = main_in_dir, pattern = "data_")
    
    for (file in files){
      dt <- data.table(readRDS(paste0(main_in_dir, file)))
      location = gsub("data_", "", file)
      location = gsub(".rds", "", location)
      dt$location <- location
      all_data = rbind(all_data, dt)
    }
    all_data$ClimateScenario = "observed"
    return (all_data)
  }
}

###############################################################
######                                                   ######
######                                                   ######
######                                                   ######
###############################################################
################################################################################
make_unique <- function(input_dir, param_dir, location_group_name){
    # This fukcing function is created because neither unique, nor !duplicate,
    # could work! So, we first separate the fucking data
    # then bind it together with another function.
    # then compute diapause stuff
    #
    # loc_grp = data.table(read.csv(paste0(param_dir, location_group_name)))
    # loc_grp$location = paste0(loc_grp$latitude, "_", loc_grp$longitude)
    # loc_grp = within(loc_grp, remove(latitude, longitude))
    
    ########## RCP45 - 2040

    file_n = "combined_CMPOP_rcp45.rds"
    file_name = paste0(input_dir, file_n)
    data <- data.table(readRDS(file_name))
    data = data %>% filter(ClimateGroup == "2040's")
    data <- within(data, remove(CountyGroup, County, 
                                tmax, tmin, DailyDD, 
                                AdultGen1, AdultGen2, AdultGen3, AdultGen4,
                                PercAdult, PercAdultGen1, PercAdultGen2,
                                PercAdultGen3, PercAdultGen4, PercEgg,
                                PercLarva, PercLarvaGen1, PercLarvaGen2, 
                                PercLarvaGen3, PercLarvaGen4, PercPupa,
                                SumAdult, SumEgg, SumPupa, CumDDinC
                                ))

    data$location = paste0(data$latitude, "_", data$longitude)
    # data <- data %>% filter(location %in% loc_grp$location)
    data$latitude = as.numeric(data$latitude)
    data$longitude = as.numeric(data$longitude)
    data = within(data, remove(ClimateGroup))
    out_dir = "/data/hydro/users/Hossein/analog/local/data_bases/"
    saveRDS(data, paste0(out_dir, "CMPOP_2040_rcp45.rds"))

    ########## RCP45 - 2060
    file_n = "combined_CMPOP_rcp45.rds"
    file_name = paste0(input_dir, file_n)
    data <- data.table(readRDS(file_name))
    data <- data %>% filter(ClimateGroup == "2060's")
    data <- data %>% filter(year >= 2056 & year <= 2065)

    data <- within(data, remove(CountyGroup, County, 
                                tmax, tmin, DailyDD, 
                                AdultGen1, AdultGen2, AdultGen3, AdultGen4,
                                PercAdult, PercAdultGen1, PercAdultGen2,
                                PercAdultGen3, PercAdultGen4, PercEgg,
                                PercLarva, PercLarvaGen1, PercLarvaGen2, 
                                PercLarvaGen3, PercLarvaGen4, PercPupa,
                                SumAdult, SumEgg, SumPupa, CumDDinC
                                ))

    data$location = paste0(data$latitude, "_", data$longitude)
    # data <- data %>% filter(location %in% loc_grp$location)
    data$latitude = as.numeric(data$latitude)
    data$longitude = as.numeric(data$longitude)

    data = within(data, remove(ClimateGroup))
    out_dir = "/data/hydro/users/Hossein/analog/local/data_bases/"
    saveRDS(data, paste0(out_dir, "CMPOP_2060_rcp45.rds"))

    ########## RCP45 - 2080
    file_n = "combined_CMPOP_rcp45.rds"
    file_name = paste0(input_dir, file_n)
    data <- data.table(readRDS(file_name))
    data <- data %>% filter(ClimateGroup == "2080's")
    data <- within(data, remove(CountyGroup, County, 
                                tmax, tmin, DailyDD, 
                                AdultGen1, AdultGen2, AdultGen3, AdultGen4,
                                PercAdult, PercAdultGen1, PercAdultGen2,
                                PercAdultGen3, PercAdultGen4, PercEgg,
                                PercLarva, PercLarvaGen1, PercLarvaGen2, 
                                PercLarvaGen3, PercLarvaGen4, PercPupa,
                                SumAdult, SumEgg, SumPupa, CumDDinC
                                ))
    data = within(data, remove(ClimateGroup))

    data$location = paste0(data$latitude, "_", data$longitude)
    # data <- data %>% filter(location %in% loc_grp$location)
    data$latitude = as.numeric(data$latitude)
    data$longitude = as.numeric(data$longitude)
    out_dir = "/data/hydro/users/Hossein/analog/local/data_bases/"
    saveRDS(data, paste0(out_dir, "CMPOP_2080_rcp45.rds"))
    
    ##########
    ########## rcp85
    ##########

    ########## rcp85 - 2040

    file_n = "combined_CMPOP_rcp85.rds"
    file_name = paste0(input_dir, file_n)
    data <- data.table(readRDS(file_name))
    data = data %>% filter(ClimateGroup == "2040's")
    data <- within(data, remove(CountyGroup, County, 
                                tmax, tmin, DailyDD, 
                                AdultGen1, AdultGen2, AdultGen3, AdultGen4,
                                PercAdult, PercAdultGen1, PercAdultGen2,
                                PercAdultGen3, PercAdultGen4, PercEgg,
                                PercLarva, PercLarvaGen1, PercLarvaGen2, 
                                PercLarvaGen3, PercLarvaGen4, PercPupa,
                                SumAdult, SumEgg, SumPupa, CumDDinC
                                ))

    data$location = paste0(data$latitude, "_", data$longitude)
    # data <- data %>% filter(location %in% loc_grp$location)
    data$latitude = as.numeric(data$latitude)
    data$longitude = as.numeric(data$longitude)
    data = within(data, remove(ClimateGroup))
    out_dir = "/data/hydro/users/Hossein/analog/local/data_bases/"
    saveRDS(data, paste0(out_dir, "CMPOP_2040_rcp85.rds"))

    ########## rcp85 - 2060
    print (paste0("line 125 of core ", dim(data)))
    file_n = "combined_CMPOP_rcp85.rds"
    file_name = paste0(input_dir, file_n)
    data <- data.table(readRDS(file_name))
    data <- data %>% filter(ClimateGroup == "2060's")
    data <- data %>% filter(year >= 2056 & year <= 2065)

    data <- within(data, remove(CountyGroup, County, 
                                tmax, tmin, DailyDD,
                                AdultGen1, AdultGen2, AdultGen3, AdultGen4,
                                PercAdult, PercAdultGen1, PercAdultGen2,
                                PercAdultGen3, PercAdultGen4, PercEgg,
                                PercLarva, PercLarvaGen1, PercLarvaGen2, 
                                PercLarvaGen3, PercLarvaGen4, PercPupa,
                                SumAdult, SumEgg, SumPupa, CumDDinC
                                ))

    data$location = paste0(data$latitude, "_", data$longitude)
    # data <- data %>% filter(location %in% loc_grp$location)
    data$latitude = as.numeric(data$latitude)
    data$longitude = as.numeric(data$longitude)
    
    data = within(data, remove(ClimateGroup))
    out_dir = "/data/hydro/users/Hossein/analog/local/data_bases/"
    saveRDS(data, paste0(out_dir, "CMPOP_2060_rcp85.rds"))

    ########## rcp85 - 2080
    file_n = "combined_CMPOP_rcp85.rds"
    file_name = paste0(input_dir, file_n)
    data <- data.table(readRDS(file_name))
    data <- data %>% filter(ClimateGroup == "2080's")
    data <- within(data, remove(CountyGroup, County, 
                                tmax, tmin, DailyDD, 
                                AdultGen1, AdultGen2, AdultGen3, AdultGen4,
                                PercAdult, PercAdultGen1, PercAdultGen2,
                                PercAdultGen3, PercAdultGen4, PercEgg,
                                PercLarva, PercLarvaGen1, PercLarvaGen2, 
                                PercLarvaGen3, PercLarvaGen4, PercPupa,
                                SumAdult, SumEgg, SumPupa, CumDDinC
                                ))
    data = within(data, remove(ClimateGroup))

    data$location = paste0(data$latitude, "_", data$longitude)
    # data <- data %>% filter(location %in% loc_grp$location)
    data$latitude = as.numeric(data$latitude)
    data$longitude = as.numeric(data$longitude)
    # data$latitude = as.numeric(substr(x = data$location, start = 1, stop = 8))
    # data$latitude = as.numeric(sapply(strsplit(data$location, "_"), function(x) x[1]))
    # data$longitude= as.numeric(sapply(strsplit(data$location, "_"), function(x) x[2]))
    
    out_dir = "/data/hydro/users/Hossein/analog/local/data_bases/"
    saveRDS(data, paste0(out_dir, "CMPOP_2080_rcp85.rds"))
}

generate_short_CM_files <- function(in_dir, out_dir){
  ## 
  ## This function just sebsets the useful columns.
  ##
  # in_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/my_aeolus_2015/all_local/analog/"
  # out_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/my_aeolus_2015/all_local/analog/"
  file_names = c("combined_CM_rcp45.rds", "combined_CM_rcp85.rds")
  for (file in file_names){
      data <- data.table(readRDS(paste0(in_dir, file)))
      data <- data.table(readRDS(paste0(in_dir, file)))
      data <- subset(data, select = c("year", "location", "ClimateScenario",
                                      "Emergence",
                                      "LGen1_0.25", "LGen1_0.5", "LGen1_0.75",
                                      "LGen2_0.25", "LGen2_0.5", "LGen2_0.75",
                                      "LGen3_0.25", "LGen3_0.5", "LGen3_0.75",
                                      "LGen4_0.25", "LGen4_0.5", "LGen4_0.75",
                                      "AGen1_0.25", "AGen1_0.5", "AGen1_0.75",
                                      "AGen2_0.25", "AGen2_0.5", "AGen2_0.75",
                                      "AGen3_0.25", "AGen3_0.5", "AGen3_0.75",
                                      "AGen4_0.25", "AGen4_0.5", "AGen4_0.75"))
      
      saveRDS(data, paste0(out_dir, "short_", file))
      rm(data)
  }
}

generate_CM_files <- function(in_dir, out_dir){
  # in_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/my_aeolus_2015/all_local/"
  # out_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/my_aeolus_2015/all_local/analog/"
  file_names = c("combined_CM_rcp45.rds", "combined_CM_rcp85.rds")
  for (file in file_names){
    data <- data.table(readRDS(paste0(in_dir, file)))
    data <- data %>% filter(year >= 2025)
    data$location = paste0(data$latitude, "_", data$longitude)
    data <- within(data, remove(longitude, latitude))
    data <- within(data, remove(CountyGroup, County, ClimateGroup))
    data = unique(data)
    saveRDS(data, paste0(out_dir, file))
    rm(data)
  }
}
###############################################################
######                                                   ######
######               Read the binary data                ######
######                                                   ######
###############################################################

read_binary <- function(file_path, hist, no_vars){
  if (hist) {
    if (no_vars==4){
      start_year <- 1950
      end_year <- 2005
      } else {
        start_year <- 1979
        end_year <- 2015
      }
  } else{
    start_year <- 2006
    end_year <- 2099
  }
  ymd_file <- create_ymdvalues(start_year, end_year)
  data <- read_binary_addmdy(file_path, ymd_file, no_vars)
  return(data)
}

read_binary_addmdy <- function(filename, ymd, no_vars){
  if (no_vars==4){
      return(read_binary_addmdy_4var(filename, ymd))
  } else {return(read_binary_addmdy_8var(filename, ymd))}
}

read_binary_addmdy_8var <- function(filename, ymd){
  Nofvariables <- 8 # number of variables or column in the forcing data file
  Nrecords <- nrow(ymd)
  ind <- seq(1, Nrecords * Nofvariables, Nofvariables)
  fileCon  <-  file(filename, "rb")
  temp <- readBin(fileCon, integer(), size = 2, n = Nrecords * Nofvariables,
                  endian = "little")
  dataM <- matrix(0, Nrecords, 8)
  k <- 1
  dataM[1:Nrecords, 1] <- temp[ind] / 40.00         # precip data
  dataM[1:Nrecords, 2] <- temp[ind + 1] / 100.00    # Max temperature data
  dataM[1:Nrecords, 3] <- temp[ind + 2] / 100.00    # Min temperature data
  dataM[1:Nrecords, 4] <- temp[ind + 3] / 100.00    # Wind speed data
  dataM[1:Nrecords, 5] <- temp[ind + 4] / 10000.00  # SPH
  dataM[1:Nrecords, 6] <- temp[ind + 5] / 40.00     # SRAD
  dataM[1:Nrecords, 7] <- temp[ind + 6] / 100.00    # Rmax
  dataM[1:Nrecords, 8] <- temp[ind + 7] / 100.00    # RMin
  AllData <- cbind(ymd, dataM)
  # calculate daily GDD  ...what? There doesn't appear to be any GDD work?
  colnames(AllData) <- c("year", "month", "day", "precip", "tmax", "tmin",
                         "windspeed", "SPH", "SRAD", "Rmax", "Rmin")
  close(fileCon)
  return(AllData)
}

read_binary_addmdy_4var <- function(filename, ymd) {
  Nofvariables <- 4 # number of variables or column in the forcing data file
  Nrecords <- nrow(ymd)
  ind <- seq(1, Nrecords * Nofvariables, Nofvariables)
  fileCon <-  file(filename, "rb")
  temp <- readBin(fileCon, integer(), size = 2, n = Nrecords * Nofvariables,
                  endian="little")
  dataM <- matrix(0, Nrecords, 4)
  k <- 1
  dataM[1:Nrecords, 1] <- temp[ind] / 40.00       # precip data
  dataM[1:Nrecords, 2] <- temp[ind + 1] / 100.00  # Max temperature data
  dataM[1:Nrecords, 3] <- temp[ind + 2] / 100.00  # Min temperature data
  dataM[1:Nrecords, 4] <- temp[ind + 3] / 100.00  # Wind speed data

  AllData <- cbind(ymd, dataM)
  # calculate daily GDD  ...what? There doesn't appear to be any GDD work?
  colnames(AllData) <- c("year", "month", "day", "precip", "tmax", "tmin",
                         "windspeed")
  close(fileCon)
  return(AllData)
}

create_ymdvalues <- function(data_start_year, data_end_year){
  Years <- seq(data_start_year, data_end_year)
  nYears <- length(Years)
  daycount_in_year <- 0
  moncount_in_year <- 0
  yearrep_in_year <- 0
    
  for (i in 1:nYears){
    ly <- leap_year(Years[i])
    if (ly == TRUE){
        days_in_mon <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
       } else {
        days_in_mon <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    }

    for (j in 1:12){
      daycount_in_year <- c(daycount_in_year, seq(1, days_in_mon[j]))
      moncount_in_year <- c(moncount_in_year, rep(j, days_in_mon[j]))
      yearrep_in_year <- c(yearrep_in_year, rep(Years[i], days_in_mon[j]))
    }
  }

  daycount_in_year <- daycount_in_year[-1] #delete the leading 0
  moncount_in_year <- moncount_in_year[-1]
  yearrep_in_year <- yearrep_in_year[-1]
  ymd <- cbind(yearrep_in_year, moncount_in_year, daycount_in_year)
  colnames(ymd) <- c("year", "month", "day")
  return(ymd)
}

