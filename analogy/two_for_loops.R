

find_NN_info_W4G_ICV_two_for_loops <- function(ICV, historical_dt, future_dt, n_neighbors){
  # remove extra columns
  if ("treatment" %in% colnames(historical_dt)) {historical_dt <- within(historical_dt, remove(treatment))}
  if ("treatment" %in% colnames(future_dt)) {future_dt <- within(future_dt, remove(treatment))}
  if ("treatment" %in% colnames(ICV)) {ICV <- within(ICV, remove(treatment))}

  # sort the columns of data tables so they both have the same order, if theydo not.
  columns_ord <- c("year", "location", "ClimateScenario",
                   "medianDoY", "NumLarvaGens_Aug", 
                   "mean_escaped_Gen1", "mean_escaped_Gen2",
                   "mean_escaped_Gen3", "mean_escaped_Gen4",
                   "mean_gdd", "mean_precip")

  setcolorder(historical_dt, columns_ord)
  setcolorder(future_dt, columns_ord)
  setcolorder(ICV, columns_ord)

  all_us_locs <- unique(historical_dt$location)

  # 9 local locations are not in the all_us data!!!
  local_locations <- unique(future_dt$location)
  local_locations <- local_locations[which(local_locations %in% all_us_locs)]
  future_dt <- future_dt %>% filter(location %in% local_locations)

  future_years <- unique(future_dt$year)
  
  trunc.SDs <- 0.1
  
  historical_dt$yr_loc <- paste0(historical_dt$year, historical_dt$location)
  future_dt$yr_loc <- paste0(future_dt$year, future_dt$location)
  ICV$yr_loc <- paste0(ICV$year, ICV$location)

  historical_dt <- within(historical_dt, remove(year, location))
  future_dt <- within(future_dt, remove(year, location))
  ICV <- within(ICV, remove(year, location))

  A_new <- as.data.frame(historical_dt)
  B_new <- as.data.frame(future_dt)
  C_new <- as.data.frame(ICV)

  numeric_cols <- c("medianDoY", "NumLarvaGens_Aug", "mean_escaped_Gen1", 
                    "mean_escaped_Gen2", "mean_escaped_Gen3", "mean_escaped_Gen4", 
                    "mean_gdd", "mean_precip")

  non_numeric_cols = c("yr_loc", "ClimateScenario")

  NN_dist_tb_new = data.table()
  NNs_loc_year_tb_new = data.table()
  NN_sigma_tb_new = data.table()

  for (row in nrow(future_dt)){
    Bj_new <- B_new[row, ]
    Cj_new <- C_new[row, ]

    # standard deviation of 1951-1990 interannual variability in each climate 
    # variable, ignoring missing years
    Cj.sd_new <- apply(Cj_new[, numeric_cols], MARGIN=2, FUN = sd, na.rm = T)
    Cj.sd_new[Cj.sd_new<(10^-10)] = 1

    A_prime_new <- A_new
    A_prime_new[, numeric_cols] <- sweep(A_prime_new[, numeric_cols], MARGIN = 2, STATS = Cj.sd_new, FUN = `/`) 
    
    # standardize the analog pool
    Bj_prime_new <- Bj_new
    Bj_prime_new[, numeric_cols] <- sweep(Bj_new[, numeric_cols], MARGIN = 2, STATS = Cj.sd_new, FUN = `/`)

    # standardize the reference ICV
    Cj_prime_new <- Cj_new
    Cj_prime_new[, numeric_cols] <- sweep(Cj_new[, numeric_cols], MARGIN = 2, STATS = Cj.sd_new, FUN = `/`)

    ## Step 2: Extract the principal components (PCs) of 
    ##         the reference period ICV and project all data onto these PCs

    # Principal components analysis. The !is.na(apply(...)) 
    # term is there simply to select all years with complete observations in all variables. 
    PCA_new <- prcomp(Cj_prime_new[, numeric_cols][!is.na(apply(Cj_prime_new[, numeric_cols], 1, mean)) ,])

    # find the number of PCs to retain using the PC truncation
    # rule of eigenvector stdev > the truncation threshold
    PCs_new <- max(which(unlist(summary(PCA_new)[1])>trunc.SDs))

    # project the reference ICV onto the PCs which is the same as analog pool:
    X_new <- as.data.frame(predict(PCA_new, A_prime_new))
    X_new <- cbind(A_prime_new[, 1:3], X_new)

    # project the projected future conditions onto the PCs
    Yj_new <- as.data.frame(predict(PCA_new, Bj_prime_new[, numeric_cols]))
    Yj_new = cbind(Bj_prime_new[, 1:3], Yj_new)

    Zj_new <- as.data.frame(predict(PCA_new, Cj_prime_new[, numeric_cols]))
    Zj_new <- cbind(Cj_prime_new[, 1:3], Zj_new)

    ## Step 3a: express PC scores as standardized
    #           anomalies of reference interannual variability
      
    # standard deviation of 1951-1990 interannual 
    # variability in each principal component, ignoring missing years
    Zj_sd_new <- apply(Zj_new[, 4:(PCs_new + 3)], MARGIN = 2, FUN = sd, na.rm=T)

    # standardize the analog pool   
    X_prime_new <- sweep(X_new[, 4:(PCs_new + 3)], MARGIN=2, Zj_sd_new, FUN = `/`)
    X_prime_new <- cbind(X_new[, 1:3], X_prime_new)

    # standardize the projected conditions
    Yj_prime_new <- sweep(Yj_new[, 4:(PCs_new + 3)], MARGIN=2, Zj_sd_new, FUN = `/`)
    Yj_prime_new <- cbind(Yj_new[, 1:3], Yj_prime_new)
    
    NN_list_new <- get.knnx(data = X_prime_new[, 4:(PCs_new+3)], 
                            query = Yj_prime_new[, 4:(PCs_new+3)], k=n_neighbors, algorithm="brute")

    NN_idx_new <- NN_list_new$nn.index
    NN_dist_new <- NN_list_new$nn.dist

    # extract the rows corresponding to nearest neighbors indices
    NNs_loc_year_new <- X_prime_new[as.vector(NN_idx_new), c('year', 'location')]

    # reshape the long list to wide data table
    NNs_loc_year_new <- Reduce(cbind, 
                        split(NNs_loc_year_new, 
                              rep(1:n_neighbors, each=(nrow(NNs_loc_year_new)/n_neighbors)))) %>%
                        data.table()
    # rename columns
    NN_dist_new <- NN_dist_new %>% data.table()
    names(NN_dist_new) <- paste0("NN_", c(1:n_neighbors))
    names(NNs_loc_year_new) <- paste0(names(NNs_loc_year_new), paste0("_NN_", rep(1:n_neighbors, each=2)))

    NN_dist_new <- cbind(Yj_new[, c("year", "location")], NN_dist_new)
    NNs_loc_year_new <- cbind(Yj_new[, c("year", "location")], NNs_loc_year_new)

    # percentile of the nearest neighbour distance on the chi distribution with
    # degrees of freedom equaling the dimensionality of the distance measurement (PCs)
    NN_chi_new <- pchi(as.vector(NN_list_new$nn.dist), PCs_new)
    # values of the chi percentiles on a 
    # standard half-normal distribution 
    # (chi distribution with one degree of freedom)
    # NN.sigma[which(proxy==j)] <- qchi(NN.chi, 1)
    NN_sigma_new <- qchi(NN_chi_new, 1)
    NN_sigma_df_new <- Reduce(cbind, 
                              split(NN_sigma_new, 
                              rep(1:n_neighbors, each=(length(NN_sigma_new)/n_neighbors)))) %>%
                        data.table()
    names(NN_sigma_df_new) <- paste0("sigma_NN_", c(1:n_neighbors))
    NN_sigma_df_new <- cbind(Yj_new[, c("year", "location")], NN_sigma_df_new)
    
    NN_dist_tb_new <- rbind(NN_dist_tb_new, NN_dist_new)
    NNs_loc_year_tb_new <- rbind(NNs_loc_year_tb_new, NNs_loc_year_new)
    NN_sigma_tb_new <- rbind(NN_sigma_tb_new, NN_sigma_df_new)
    rm(NN_dist_new, NNs_loc_year_new, NN_sigma_df_new)
  }
  
  distance_col_names <- c("year", "location", paste0("NN_", c(1:n_neighbors)))
  
  LL = (ncol(NNs_loc_year_tb_new)-2)/2
  v = rep(c("year_NN_", "location_NN_"), LL); w = rep(1:LL, each = 2);
  loc_year_col_names <- c("year", "location", paste0(v, w))
  
  sigma_df_col_names <- c("year", "location", paste0("sigma_NN_", c(1:n_neighbors)))
  
  setcolorder(NN_dist_tb_new, distance_col_names)
  setcolorder(NNs_loc_year_tb_new, loc_year_col_names)
  setcolorder(NN_sigma_tb_new, sigma_df_col_names)
  #############################################
  return(list(NN_dist_tb_new, NNs_loc_year_tb_new, NN_sigma_tb_new, 
              colnames(NN_dist_tb_new), colnames(NNs_loc_year_tb_new), colnames(NN_sigma_tb_new)))
}