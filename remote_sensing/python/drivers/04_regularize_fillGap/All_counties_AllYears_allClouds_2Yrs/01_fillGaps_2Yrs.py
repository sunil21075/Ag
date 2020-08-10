####
#### July 3, 2020
####

"""
  This script was writte on Jul. 3 originally,
  On July 29 I am modifying it so in inclues also jump-removed data.
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

data_dir = "/data/hydro/users/Hossein/remote_sensing/03_Regularized_TS/70_cloud/2Yrs/"
output_dir = "/data/hydro/users/Hossein/remote_sensing/03_Regularized_TS/70_cloud/2Yrs/"
os.makedirs(output_dir, exist_ok=True)

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
jumps = sys.argv[2]
county = "Grant"
SF_year = 2017
regular_window_size = 10
########################################################################################
###
###                   updates based on wJumps or noJumps
###
########################################################################################
if jumps == "noJumps":
  data_dir = data_dir + "noJump_Regularized/"
  f_name = "00_noJumpsRegularized_" + county + "_SF_" + str(SF_year) + "_" + indeks + ".csv"
  output_dir = output_dir + "noJump_Regularized/"
  os.makedirs(output_dir, exist_ok=True)
else:
  f_name = "00_Regularized_" + county + "_SF_" + str(SF_year) + "_" + indeks + ".csv"

########################################################################################
###
###                   process data
###
########################################################################################

an_EE_TS = pd.read_csv(data_dir + f_name, low_memory=False)

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

reg_cols = an_EE_TS.columns

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
    
    curr_field.reset_index(drop=True, inplace=True)
    
    # print ("print(curr_field.shape)")
    # print(curr_field.shape)
    # print ("__________________________________________")
    ################################################################
    curr_field = rc.fill_theGap_linearLine(curr_field, V_idx = indeks, SF_year = 2017)

    ################################################################
    row_pointer = no_steps * counter
    output_df[row_pointer: row_pointer + no_steps] = curr_field.values
    counter += 1


output_df = rc.add_human_start_time_by_YearDoY(output_df)
rc.convert_human_system_start_time_to_systemStart_time(output_df)
####################################################################################
###
###                   Write the outputs
###
####################################################################################

out_name = output_dir + "01_Regular_filledGap_" + county + "_SF_" + str(SF_year) + "_" + indeks + ".csv"
os.makedirs(output_dir, exist_ok=True)
output_df.to_csv(out_name, index = False)

end_time = time.time()
print(end_time - start_time)





