find_NN_info_W4G_ICV_stop_working <- function(ICV, historical_dt, future_dt, n_neighbors){
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
    NN_chi <- EnvStats::pchi(as.vector(NN_list$nn.dist), PCs)
    
    # values of the chi percentiles on a standard half-normal distribution
    # (chi distribution with one degree of freedom)
    # NN.sigma[which(proxy==j)] <- qchi(NN.chi, 1)

    NN_sigma <- EnvStats::qchi(NN_chi, 1)

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




find_NN_info_W4G_ICV_2Loops <- function(ICV, historical_dt, future_dt, n_neighbors=50){
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
      NN_chi <- EnvStats::pchi(as.vector(NN_list$nn.dist), PCs)
      
      # values of the chi percentiles on a standard half-normal distribution
      # (chi distribution with one degree of freedom)
      # NN.sigma[which(proxy==j)] <- qchi(NN.chi, 1)
      NN_sigma <- EnvStats::qchi(NN_chi, 1)

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