###############################################################################################
#
# https://scikit-learn.org/stable/auto_examples/covariance/plot_mahalanobis_distances.html
# https://scikit-learn.org/stable/modules/generated/sklearn.covariance.EmpiricalCovariance.html
# https://scikit-learn.org/stable/modules/generated/sklearn.covariance.empirical_covariance.html
# https://scikit-learn.org/stable/modules/covariance.html
################################################################################################

# basic libraries
import numpy as np
import pandas as pd

from scipy.spatial import distance
import sklearn
from sklearn import preprocessing
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA

from scipy.spatial import distance
from sklearn.covariance import EmpiricalCovariance, MinCovDet
import sklearn.covariance

import warnings

main_dir = "/data/hydro/users/Hossein/analog/"

main_us_dir = main_dir + "usa/ready_features/"
main_local_dir = main_dir + "local/ready_features/one_file_4_all_locations/"
output_dir =  main_dir + "z_R_results/sigma/"

#
# read data
#
all_data_usa = pd.read_csv( main_us_dir + "all_data_usa.csv")

# file name is place holder 
# later to use in parallel qsub
local_dt = pd.read_csv(main_local_dir + file_name)
#
# drop the treatment column that was generated for matching package of R
#
all_data_usa = all_data_usa.drop(['treatment'], axis=1)
local_dt = local_dt.drop(['treatment'], axis=1)

# define new order for columns of data
columns_ord = ["year", "location","ClimateScenario",
               "medianDoY", "NumLarvaGens_Aug", 
               "mean_escaped_Gen1", "mean_escaped_Gen2",
               "mean_escaped_Gen3", "mean_escaped_Gen4",
               "mean_gdd", "mean_precip"]

numeric_feat = columns_ord[3:]
#
# reorder the columns of data frames so they have
# identical column orders
#
all_data_usa = all_data_usa[columns_ord]
local_dt = local_dt[columns_ord]

all_usa_numeric = all_data_usa.loc[:, numeric_feat].copy()


all_locations = pd.Series(all_data_usa.location.unique())
local_locations = pd.Series(local_dt.location.unique())

# convert them to Index type to find the intersecting entries
all_locations_idx = pd.Index(all_locations)
local_locations_idx = pd.Index(local_locations)
#
# local locations existing in the all_us_data
# 
local_locations = pd.Series(local_locations_idx.intersection(all_locations_idx))

##
##
##
future_years = local_dt.year.unique()



for loc in local_locations:
    col_names = ["future_loc", "future_year", "hist_loc", "hist_year", "dist"]
    all_distances_table = pd.DataFrame(0, index=np.arange(????), columns = col_names)

    # inverse of correlation matrix, if we want to use Mahab. directly
    # as opposed to three steps.
    """
    BIG QUESTION: Do we want to use covariance amtrix of all historical data
                  Or covariance of each location separately?
    """
    Cj = all_data_usa[all_data_usa['location'] == loc].copy()
    Cj_numeric = Cj.iloc[:, 3:]

    cov_matrix = sklearn.covariance.empirical_covariance(Cj_numeric, assume_centered=False)
    cov_matrix = pd.DataFrame(sklearn.covariance.empirical_covariance(Cj_numeric, assume_centered=False), 
                              columns=list(Cj_numeric))
    cov_matrix.head(2)

    cov_matrix_inv = pd.DataFrame(np.linalg.pinv(cov_matrix.values),
                             columns = cov_matrix.columns, index=cov_matrix.index)
    cov_matrix_inv.head(2)

    curr_loc_dt = local_dt[local_dt['location'] == loc].copy()
    for yr in future_years:
      curr_dt <- curr_loc_dt %>% filter(year==yr)
      durr_diff = curr_dt[, 4:11] - all_dt_usa[, 4:11]

  







