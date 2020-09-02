  
####
#### Sept. 1
####

"""
Jupyter Notebook is called on iMac:
Acreages_ConfusionStyle_raw_n_regular_SOS
"""

import csv
import time
import scipy
import datetime
import itertools
import numpy as np
import os, os.path
import scipy.signal
import pandas as pd
from patsy import cr
from math import factorial
from IPython.display import Image
from sklearn.linear_model import LinearRegression
from statsmodels.sandbox.regression.predstd import wls_prediction_std

import matplotlib.pyplot as plt
import seaborn as sb

# import geopandas as gpd
# from shapely.geometry import Point, Polygon
# from pprint import pprint

import sys
start_time = time.time()

####################################################################################
###
###                      Aeolus Core path
###
####################################################################################

sys.path.append('/home/hnoorazar/remote_sensing_codes/')

import remote_sensing_core as rc
import remote_sensing_core as rcp

####################################################################################
###
###      Parameters                   
###
####################################################################################
SG_win_size = 5
SG_order = 3
delt = 0.4
do_plot  = 0
SF_year = 2017

given_county = sys.argv[1]
SF_year = int(sys.argv[2])
indekses = sys.argv[3]

print ("given_county is " + given_county.replace("_", " "))

regularized = True
raw_or_regular = "regular"

####################################################################################
###
###                   Aeolus Directories
###
####################################################################################
param_dir = "/home/hnoorazar/remote_sensing_codes/parameters/"
SF_data_dir = "/data/hydro/users/Hossein/remote_sensing/01_shapefiles_data_part_not_filtered/"

# the following data came from 70% cloud.
regular_in_dir = "/data/hydro/users/Hossein/remote_sensing/05_Regular_SOS_confusions/00_allYCtables_separate/"
regular_output_dir = "/data/hydro/users/Hossein/remote_sensing/05_Regular_SOS_confusions/01_allYCtables_merged/"

data_dir = regular_in_dir    
output_dir = regular_output_dir

os.makedirs(output_dir, exist_ok=True)

print ("_________________________________________________________")
print ("data dir is: " + data_dir)
print ("_________________________________________________________")
print ("output_dir is: " +  output_dir)
print ("_________________________________________________________")

####################################################################################
###
###                   Read parameters and data
###
####################################################################################


####
####  parameters
####

years = [2016, 2017, 2018]
indekses = ["EVI"]

perennials_out = [True]
NASS_out = [True]
non_Irr_out = [True]

double_by_Note = [False]

peak_count

all_patterns = ["2018_EVI_PereOut_NASSin_JustIrr_dblNotFiltered_confusion_Acr_exactly2Peaks_regular",
                "2017_EVI_PereOut_NASSin_JustIrr_dblNotFiltered_confusion_Acr_exactly2Peaks_regular",
                "2016_EVI_PereOut_NASSin_JustIrr_dblNotFiltered_confusion_Acr_exactly2Peaks_regular",

                "2018_EVI_PereOut_NASSin_BothIrr_dblNotFiltered_confusion_Acr_exactly2Peaks_regular",
                "2017_EVI_PereOut_NASSin_BothIrr_dblNotFiltered_confusion_Acr_exactly2Peaks_regular",
                "2016_EVI_PereOut_NASSin_BothIrr_dblNotFiltered_confusion_Acr_exactly2Peaks_regular",
            ]












