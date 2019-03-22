##############################################################################################
# MAHONY Stye Below:

library(data.table)
library(dplyr)
library(raster)
library(FNN)
library(RColorBrewer)
library(colorRamps)
library(adehabitatLT) # provides the chi distribution

###############################################################################################
# https://scikit-learn.org/stable/auto_examples/covariance/plot_mahalanobis_distances.html
# https://scikit-learn.org/stable/modules/generated/sklearn.covariance.EmpiricalCovariance.html
#
#
################################################################################################

main_dir <- "/data/hydro/users/Hossein/analog/"

main_us_dir <- file.path(main_dir, "usa/ready_features/")
main_local_dir <- file.path(main_dir, "local/ready_features/one_file_4_all_locations/")
output_dir <- file.path(main_dir, "z_R_results/sigma/")


all_dt_usa <- data.table(readRDS(paste0(main_us_dir, "all_data_usa.rds")))
all_dt_usa <- within(all_dt_usa, remove(treatment))
# file name is place holder 
# later to use in parallel qsub
local_dt <- data.table(readRDS(paste0(main_local_dir, file_name)))
local_dt <- within(local_dt, remove(treatment))

###########################################################################
# for laptop

all_dt_usa <- data.table(readRDS("/Users/hn/Desktop/Desktop/Kirti/check_point/analogs/all_data_usa.rds"))
# file name is place holder 
# later to use in parallel qsub
local_dt <- data.table(readRDS("/Users/hn/Desktop/Desktop/Kirti/check_point/analogs/feat_bcc-csm1-1-m_rcp45.rds"))

all_dt_usa <- within(all_dt_usa, remove(treatment))
local_dt <- within(local_dt, remove(treatment))

###########################################################################
# sort the columns of data tables so they both have 
# the same order, if they do not.
columns_ord <- c("year", "location","ClimateScenario",
                 "medianDoY", "NumLarvaGens_Aug", 
                 "mean_escaped_Gen1", "mean_escaped_Gen2",
                 "mean_escaped_Gen3", "mean_escaped_Gen4",
                 "mean_gdd", "mean_precip")

setcolorder(all_dt_usa, columns_ord)
setcolorder(local_dt, columns_ord)

min_max <- FALSE
# min_max scaling of historical data and future data
# This will put the range of the data in [0, 1]
# which takes care of different scales, however,
# it changes the correlations. Do we want that?
# why that happens? What are consequences?
if (min_max == TRUE){
    minimums <- apply(all_dt_usa[, 4:11], MARGIN=2, FUN=min)
    maximums <- apply(all_dt_usa[, 4:11], MARGIN=2, FUN=max)
    range_col <- maximums - minimums

    all_dt_usa[, 4:11] <- (all_dt_usa[, 4:11] - minimums) / range_col
    local_dt[, 4:11] <- (local_dt[, 4:11]- minimums) / range_col
}

all_us_locs <- unique(all_dt_usa$location)

# 9 local locations are not in the all_us data!!!
local_locations <- unique(local_dt$location)
local_locations <- local_locations[which(local_locations %in% all_us_locs)]
local_dt <- local_dt %>% filter(location %in% local_locations)

future_years <- unique(local_dt$year)

# Principal component truncation rule
trunc.SDs <- 0.1 # truncation 

# initiate the data frame to store the projected sigma 
# dissimilarity of best analogs for each grid cell. 
NN.sigma <- rep(NA, length(???))

A <- all_dt_usa
B <- local_dt
C <- all_dt_usa

for (loc in local_locations){
  all_distances_table <- setNames(data.table(matrix(nrow = length(future_years) * dim(all_dt_usa)[1], 
                                                    ncol = 5)), 
                                             c("future_loc", "future_year", 
                                             	"hist_loc", "hist_year", "dist"))
  # loc = local_locations[1]
  Bj <- B %>% filter(location==loc)
  Cj <- C %>% filter(location==loc)

  # standard deviation of 1951-1990 interannual variability in each climate 
  # variable, ignoring missing years
  Cj.sd <- apply(Cj[, 4:11], MARGIN=2, FUN=sd, na.rm=T)
  
  # lots of zeros, causing SD to be zero, causing NaNs in new data frame.
  Cj.sd['mean_escaped_Gen4'] <- 1 
  
  A.prime <- A
  A.prime[, 4:11] <- sweep(A[, 4:11], MARGIN=2, STATS = Cj.sd, FUN = `/`) # standardize
  sum(is.na(A.prime$mean_escaped_Gen4))
 
  # standardize the analog pool
  Bj.prime <- Bj
  Bj.prime[, 4:11] <- sweep(Bj[, 4:11], MARGIN=2, STATS = Cj.sd, FUN = `/`)
  sum(is.na(Bj.prime$mean_escaped_Gen4))

  # standardize the reference ICV
  Cj.prime <- Cj
  Cj.prime[, 4:11] <- sweep(Cj[, 4:11], MARGIN=2, STATS = Cj.sd, FUN = `/`) 
  sum(is.na(Cj.prime$mean_escaped_Gen4))

#################################################################################
#
# Thigns below were in the following for-loop
#
#################################################################################
  ## Step 2: Extract the principal components (PCs) of 
  ##         the reference period ICV and project all data onto these PCs
  ## 
    
  # Principal components analysis. The !is.na(apply(...)) 
  # term is there simply to select all years with complete observations in all variables. 
  PCA <- prcomp(Cj.prime[, 4:11][!is.na(apply(Cj.prime[, 4:11], 1, mean)) ,])

  # find the number of PCs to retain using the PC truncation
  # rule of eigenvector stdev > the truncation threshold
  PCs <- max(which(unlist(summary(PCA)[1])>trunc.SDs))

  # project the reference ICV onto the PCs which is the same as analog pool:
  X <- as.data.frame(predict(PCA, A.prime))
  X <- cbind(A.prime[, 1:3], X)

  Zj <- as.data.frame(predict(PCA, Cj.prime[, 4:11]))
  Zj <- cbind(Cj.prime[, 1:3], Zj)

  ## Step 3a: express PC scores as standardized
  #           anomalies of reference interannual variability
    
  # standard deviation of 1951-1990 interannual 
  # variability in each principal component, ignoring missing years
  Zj.sd <- apply(Zj[, 4:11], MARGIN = 2, FUN = sd, na.rm=T)

  # standardize the analog pool   
  X.prime <- sweep(X[, 4:11], MARGIN=2, Zj.sd, `/`)
  X.prime <- cbind(X[, 1:3], X.prime)

  for (yr in future_years){    
    Bj.prime_yr <- Bj.prime %>% filter(year==yr)
    # standardize the analog pool (did above)
    # Bj.prime <- sweep(Bj, MARGIN=2, STATS = Cj.sd, FUN = `/`)

    ## Step 2: Extract the principal components (PCs) of 
    ##         the reference period ICV and project all data onto these PCs
    ## 
    
    # project the projected future conditions onto the PCs
    Yj <- as.data.frame(predict(PCA, Bj.prime_yr[, 4:11]))
    Yj = cbind(Bj.prime_yr[, 1:3], Yj)

    ## Step 3a: express PC scores as standardized
    #           anomalies of reference interannual variability

    # standardize the projected conditions
    Yj.prime <- sweep(Yj[, 4:11], MARGIN=2, Zj.sd, `/`)
    Yj.prime = cbind(Yj[, 1:3], Yj.prime)

    ## Step 3b: find the sigma dissimilarity of each projected 
    #           condition with its best analog (Euclidean nearest neighbour) 
    #           in the observed analog pool. 

    # Euclidean nearest neighbour distance in the z-standardized 
    # PCs of interannual climatic variability, i.e. the Mahalanobian nearest neighbour. 
    NN.dist <- as.vector(get.knnx(data = X.prime[, (1+3):(PCs+3)], 
                                  query = Yj.prime[, (1+3):(PCs+3)], k=2, algorithm="brute")[[2]])
    
    # percentile of the nearest neighbour distance 
    # on the chi distribution with 
    # degrees of freedom equaling the dimensionality 
    # of the distance measurement (PCs)
    NN.chi <- pchi(NN.dist, PCs) 
    
    # values of the chi percentiles on a 
    # standard half-normal distribution 
    # (chi distribution with one degree of freedom)
    NN.sigma[which(proxy==j)] <- qchi(NN.chi, 1) 

  }
}

##############################################################################################
################                                                              ################
################                 No For Loop below                            ################
################                                                              ################
##############################################################################################
library(data.table)
library(dplyr)
library(raster)
library(FNN)
library(RColorBrewer)
library(colorRamps)
library(adehabitatLT) # provides the chi distribution

main_dir <- "/data/hydro/users/Hossein/analog/"

main_us_dir <- file.path(main_dir, "usa/ready_features/")
main_local_dir <- file.path(main_dir, "local/ready_features/one_file_4_all_locations/")
output_dir <- file.path(main_dir, "z_R_results/sigma/")

all_dt_usa <- data.table(readRDS(paste0(main_us_dir, "all_data_usa.rds")))
all_dt_usa <- within(all_dt_usa, remove(treatment))

# file name is place holder later to use in parallel qsub
local_dt <- data.table(readRDS(paste0(main_local_dir, file_name)))
local_dt <- within(local_dt, remove(treatment))
###########################################################################
# for laptop
all_dt_usa <- data.table(readRDS("/Users/hn/Desktop/Desktop/Kirti/check_point/analogs/all_data_usa.rds"))
# file name is place holder 
# later to use in parallel qsub
local_dt <- data.table(readRDS("/Users/hn/Desktop/Desktop/Kirti/check_point/analogs/feat_bcc-csm1-1-m_rcp45.rds"))
all_dt_usa <- within(all_dt_usa, remove(treatment))
local_dt <- within(local_dt, remove(treatment))
###########################################################################
# sort the columns of data tables so they both have the same order, if theydo not.
columns_ord <- c("year", "location","ClimateScenario",
                 "medianDoY", "NumLarvaGens_Aug", 
                 "mean_escaped_Gen1", "mean_escaped_Gen2",
                 "mean_escaped_Gen3", "mean_escaped_Gen4",
                 "mean_gdd", "mean_precip")

setcolorder(all_dt_usa, columns_ord)
setcolorder(local_dt, columns_ord)

min_max <- FALSE
# min_max scaling of historical data and future data
# This will put the range of the data in [0, 1]
# which takes care of different scales, however,
# it changes the correlations. Do we want that?
# why that happens? What are consequences?
if (min_max == TRUE){
  minimums <- apply(all_dt_usa[, 4:11], MARGIN=2, FUN=min)
  maximums <- apply(all_dt_usa[, 4:11], MARGIN=2, FUN=max)
  range_col <- maximums - minimums
  all_dt_usa[, 4:11] <- (all_dt_usa[, 4:11] - minimums) / range_col
  local_dt[, 4:11] <- (local_dt[, 4:11]- minimums) / range_col
}

all_us_locs <- unique(all_dt_usa$location)

# 9 local locations are not in the all_us data!!!
local_locations <- unique(local_dt$location)
local_locations <- local_locations[which(local_locations %in% all_us_locs)]
local_dt <- local_dt %>% filter(location %in% local_locations)

future_years <- unique(local_dt$year)

# Principal component truncation rule
trunc.SDs <- 0.1 # truncation 

# initiate the data frame to store the projected sigma 
# dissimilarity of best analogs for each grid cell. 
NN.sigma <- rep(NA, length(???))

A <- all_dt_usa
B <- local_dt
C <- all_dt_usa
for (loc in local_locations){
  all_distances_table <- setNames(data.table(matrix(nrow = length(future_years) * dim(all_dt_usa)[1], 
                                                    ncol = 5)), 
                                             c("future_loc", "future_year", 
                                             	"hist_loc", "hist_year", "dist"))
  # loc = local_locations[1]
  Bj <- B %>% filter(location==loc)
  Cj <- C %>% filter(location==loc)

  # standard deviation of 1951-1990 interannual variability in each climate 
  # variable, ignoring missing years
  Cj.sd <- apply(Cj[, 4:11], MARGIN=2, FUN=sd, na.rm=T)
  
  # lots of zeros, causing SD to be zero, causing NaNs in new data frame.
  Cj.sd['mean_escaped_Gen4'] <- 1 
  
  A.prime <- A
  A.prime[, 4:11] <- sweep(A[, 4:11], MARGIN=2, STATS = Cj.sd, FUN = `/`) # standardize
  sum(is.na(A.prime$mean_escaped_Gen4))
 
  # standardize the analog pool
  Bj.prime <- Bj
  Bj.prime[, 4:11] <- sweep(Bj[, 4:11], MARGIN=2, STATS = Cj.sd, FUN = `/`)
  sum(is.na(Bj.prime$mean_escaped_Gen4))

  # standardize the reference ICV
  Cj.prime <- Cj
  Cj.prime[, 4:11] <- sweep(Cj[, 4:11], MARGIN=2, STATS = Cj.sd, FUN = `/`) 
  sum(is.na(Cj.prime$mean_escaped_Gen4))

  #################################################################################
  #
  # Thigns below were in the following for-loop
  #
  #################################################################################
  ## Step 2: Extract the principal components (PCs) of 
  ##         the reference period ICV and project all data onto these PCs
  ## 
    
  # Principal components analysis. The !is.na(apply(...)) 
  # term is there simply to select all years with complete observations in all variables. 
  PCA <- prcomp(Cj.prime[, 4:11][!is.na(apply(Cj.prime[, 4:11], 1, mean)) ,])

  # find the number of PCs to retain using the PC truncation
  # rule of eigenvector stdev > the truncation threshold
  PCs <- max(which(unlist(summary(PCA)[1])>trunc.SDs))

  # project the reference ICV onto the PCs which is the same as analog pool:
  X <- as.data.frame(predict(PCA, A.prime))
  X <- cbind(A.prime[, 1:3], X)

  # project the projected future conditions onto the PCs
  Yj <- as.data.frame(predict(PCA, Bj.prime[, 4:11]))
  Yj = cbind(Bj.prime[, 1:3], Yj)

  Zj <- as.data.frame(predict(PCA, Cj.prime[, 4:11]))
  Zj <- cbind(Cj.prime[, 1:3], Zj)

  ## Step 3a: express PC scores as standardized
  #           anomalies of reference interannual variability
    
  # standard deviation of 1951-1990 interannual 
  # variability in each principal component, ignoring missing years
  Zj.sd <- apply(Zj[, 4:11], MARGIN = 2, FUN = sd, na.rm=T)

  # standardize the analog pool   
  X.prime <- sweep(X[, 4:11], MARGIN=2, Zj.sd, `/`)
  X.prime <- cbind(X[, 1:3], X.prime)

  # standardize the projected conditions
  Yj.prime <- sweep(Yj[, 4:11], MARGIN=2, Zj.sd, `/`)
  Yj.prime = cbind(Yj[, 1:3], Yj.prime)
  
  NN.dist <- as.vector(get.knnx(data = X.prime[, (1+3):(PCs+3)], 
                                query = Yj.prime[, (1+3):(PCs+3)], k=2, algorithm="brute")[[2]])
    
  # percentile of the nearest neighbour distance 
  # on the chi distribution with 
  # degrees of freedom equaling the dimensionality 
  # of the distance measurement (PCs)
  NN.chi <- pchi(NN.dist, PCs) 

  # values of the chi percentiles on a 
  # standard half-normal distribution 
  # (chi distribution with one degree of freedom)
  NN.sigma[which(proxy==j)] <- qchi(NN.chi, 1) 
}

##############################################################################################
################                                                              ################
################                 No For Loop, no Gen 4 below                  ################
################                                                              ################
##############################################################################################



# ^^^^^^^^^^^^^^                  MAHONY Stye in 3 steps!!!                     ^^^^^^^^^^^^^^
##############################################################################################

library(dplyr)
library(raster)
library(FNN)
library(RColorBrewer)
library(colorRamps)
library(adehabitatLT) # provides the chi distribution

###############################################################################################
# https://scikit-learn.org/stable/auto_examples/covariance/plot_mahalanobis_distances.html
# https://scikit-learn.org/stable/modules/generated/sklearn.covariance.EmpiricalCovariance.html
#
#
################################################################################################

main_dir <- "/data/hydro/users/Hossein/analog/"

main_us_dir <- file.path(main_dir, "usa/ready_features/")
main_local_dir <- file.path(main_dir, "local/ready_features/one_file_4_all_locations/")
output_dir <- file.path(main_dir, "z_R_results/sigma/")


all_dt_usa <- data.table(readRDS(paste0(main_us_dir, "all_data_usa.rds")))
all_dt_usa <- within(all_dt_usa, remove(treatment))
# file name is place holder 
# later to use in parallel qsub
local_dt <- data.table(readRDS(paste0(main_local_dir, file_name)))
local_dt <- within(local_dt, remove(treatment))

# sort the columns of data tables so they both have 
# the same order, if they do not.
columns_ord <- c("year", "location","ClimateScenario",
                 "medianDoY", "NumLarvaGens_Aug", 
                 "mean_escaped_Gen1", "mean_escaped_Gen2",
                 "mean_escaped_Gen3", "mean_escaped_Gen4",
                 "mean_gdd", "mean_precip")

setcolorder(all_dt_usa, columns_ord)
setcolorder(local_dt, columns_ord)

min_max <- FALSE
# min_max scaling of historical data and future data
# This will put the range of the data in [0, 1]
# which takes care of different scales, however,
# it changes the correlations. Do we want that?
# why that happens? What are consequences?
if (min_max == TRUE){
    minimums <- apply(all_dt_usa[, 4:11], MARGIN=2, FUN=min)
    maximums <- apply(all_dt_usa[, 4:11], MARGIN=2, FUN=max)
    range_col <- maximums - minimums

    all_dt_usa[, 4:11] <- (all_dt_usa[, 4:11] - minimums) / range_col
    local_dt[, 4:11] <- (local_dt[, 4:11]- minimums) / range_col
}

##################################################
#
# Directly use the Mahab. dist. in Eq. 2
#
##################################################
# 9 local locations are not in the all_us data!!!
all_locations = unique(all_dt_usa$location)

local_locations <- unique(local_dt$location)
local_locations = local_locations[which(local_locations %in% all_locations)]

future_years <- unique(local_dt$year)

numeric_cols <-c("medianDoY", "NumLarvaGens_Aug", 
                 "mean_escaped_Gen1", "mean_escaped_Gen2",
                 "mean_escaped_Gen3", "mean_escaped_Gen4",
                 "mean_gdd", "mean_precip")
for (loc in local_locations){
  all_distances_table <- setNames(data.table(matrix(nrow = length(future_years) * dim(all_dt_usa)[1], 
                                                    ncol = 5)), 
                                             c("future_loc", "future_year", 
                                             	"hist_loc", "hist_year", "dist"))

  # inverse of correlation matrix, if we want to use Mahab. directly
  # as opposed to three steps.
  Cj = all_dt_usa %>% filter(location==loc)
  Cj_numeric = Cj[, 4:11]
  cov_matrix = cov(Cj_numeric, method = c("pearson"))
  cov_matrix_inv = solve(cov_matrix) # data.matrix(df, rownames.force = NA) # matrix product: %*%

  curr_loc_dt = local_dt %>% filter(location==loc)
  for (yr in future_years){    
    curr_dt <- curr_loc_dt %>% filter(year==yr)
    durr_diff = curr_dt[, 4:11] - all_dt_usa[, 4:11]
  }

}



