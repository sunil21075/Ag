####
#### July 3, 2020
####

"""
  Regularize the EVI and NDVI of fields in Grant, 2017.
"""

import csv
import numpy as np
import pandas as pd
# import geopandas as gpd
from IPython.display import Image
# from shapely.geometry import Point, Polygon
from math import factorial
import scipy
import scipy.signal
import os, os.path

from datetime import date
import datetime
import time

from statsmodels.sandbox.regression.predstd import wls_prediction_std
from sklearn.linear_model import LinearRegression
from patsy import cr

# from pprint import pprint
import matplotlib.pyplot as plt
import seaborn as sb

import sys
start_time = time.time()

# search path for modules
# look @ https://stackoverflow.com/questions/67631/how-to-import-a-module-given-the-full-path


####################################################################################
###
###                      Local
###
####################################################################################

###
### Core path
###

# sys.path.append('/Users/hn/Documents/00_GitHub/Ag/remote_sensing/python/')

###
### Directories
###

# param_dir = "/Users/hn/Documents/00_GitHub/Ag/remote_sensing/parameters/"
####################################################################################
###
###                      Aeolus Core path
###
####################################################################################

sys.path.append('/home/hnoorazar/remote_sensing_codes/')

####################################################################################
###
###                   Aeolus Directories
###
####################################################################################

data_dir = "/data/hydro/users/Hossein/remote_sensing/02_Eastern_WA_EE_TS/2Years/"
param_dir = "/home/hnoorazar/remote_sensing_codes/parameters/"

####################################################################################
###
###                   Import remote cores
###
####################################################################################

import remote_sensing_core as rc
import remote_sensing_core as rcp

####################################################################################
###
###      Parameters                   
###
####################################################################################

indeks = sys.argv[1]
county = "Grant"
SF_year = 2017
regular_window_size = 10
########################################################################################
###
###                   process data
###
########################################################################################

f_name = "Eastern_WA_" + str(SF_year) + "_70cloud_selectors.csv"
an_EE_TS = pd.read_csv(data_dir + f_name, low_memory=False)

########################################################################################

an_EE_TS = an_EE_TS[an_EE_TS['county'] == county] # Filter Grant
an_EE_TS['SF_year'] = SF_year

########################################################################################

output_dir = "/data/hydro/users/Hossein/remote_sensing/03_Regularized_TS/2Yrs/"
os.makedirs(output_dir, exist_ok=True)
########################################################################################

if (indeks == "EVI"):
    an_EE_TS = rc.initial_clean_EVI(an_EE_TS)
else:
    an_EE_TS = rc.initial_clean_NDVI(an_EE_TS)

an_EE_TS.head(2)

###
### List of unique polygons
###
polygon_list = an_EE_TS['ID'].unique()
print(len(polygon_list))

########################################################################################
###
###  initialize output data. all polygons in this case
###  will have the same length. 
###  9 steps in the first three months, followed by 36 points in the full year,
###  9 months in the last year
###
reg_cols = ['ID', 'Acres', 'county', 'CropGrp', 'CropTyp',
            'DataSrc', 'ExctAcr', 'IntlSrD', 'Irrigtn', 'LstSrvD', 'Notes',
            'RtCrpTy', 'Shap_Ar', 'Shp_Lng', 'TRS', 'image_year', 
            'SF_year', 'doy', indeks]

# We have 51 below, because in total there are 515 days in 17 months
# we have

no_days = 515
regular_window_size = 10
no_steps = int(no_days/regular_window_size)

nrows = no_steps * len(polygon_list)
output_df = pd.DataFrame(data = None,
                         index = np.arange(nrows), 
                         columns = reg_cols)
########################################################################################

counter = 0

for a_poly in polygon_list:
    if (counter % 300 == 0):
        print (counter)
    curr_field = an_EE_TS[an_EE_TS['ID']==a_poly].copy()
    ################################################################
    # Sort by DoY (sanitary check)
    curr_field.sort_values(by=['image_year', 'doy'], inplace=True)
    
    curr_field = rc.correct_timeColumns_dataTypes(curr_field)
    curr_field.reset_index(drop=True, inplace=True)
    
    # print ("print(curr_field.shape")
    # print(curr_field.shape)
    # print ("__________________________________________")
    ################################################################
    regularized_TS = rc.regularize_movingWindow_windowSteps_2Yrs(one_field_df = curr_field, \
                                                                 SF_yr = SF_year, \
                                                                 idks = indeks, \
                                                                 window_size = regular_window_size)
    # print(regularized_TS.shape)

    ################################################################
    row_pointer = no_steps * counter
    output_df[row_pointer: row_pointer + no_steps] = regularized_TS.values
    counter += 1


####################################################################################
###
###                   Write the outputs
###
####################################################################################

out_name = output_dir + "00_Regularized_" + county + "_SF_" + str(SF_year) + "_" + indeks + ".csv"
os.makedirs(output_dir, exist_ok=True)
output_df.to_csv(out_name, index = False)

end_time = time.time()
print(end_time - start_time)





