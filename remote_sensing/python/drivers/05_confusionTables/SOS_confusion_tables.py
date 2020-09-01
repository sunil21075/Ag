####
#### Sept. 1
####


import csv
import numpy as np
import pandas as pd
# import geopandas as gpd
from IPython.display import Image
# from shapely.geometry import Point, Polygon
from math import factorial
import datetime
import time
import scipy
import scipy.signal
import os, os.path

from statsmodels.sandbox.regression.predstd import wls_prediction_std
from sklearn.linear_model import LinearRegression
from patsy import cr

# from pprint import pprint
import matplotlib.pyplot as plt
import seaborn as sb


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
indeks = sys.argv[3]

print ("given_county is " + given_county.replace("_", " "))
print("SG_params is {}.".format(SG_params))
print("SG_win_size is {} and SG_order is {}.".format(SG_win_size, SG_order))

regularized = True
raw_or_regular = "regular"

####################################################################################
###
###                   Aeolus Directories
###
####################################################################################
param_dir = "/home/hnoorazar/remote_sensing_codes/parameters/"
SF_data_dir = "/data/hydro/users/Hossein/remote_sensing/01_shapefiles_data_part_not_filtered/"

regular_in_dir = "/data/hydro/users/Hossein/remote_sensing/04_noJump_Regularized_plt_tbl_SOSEOS/2Yrs_tables_regular/"
regular_output_dir = "/data/hydro/users/Hossein/remote_sensing/05_Regular_SOS_confusions/"

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
###                   Read data
###
####################################################################################
double_crop_potens = pd.read_csv(param_dir + "double_crop_potential_plants.csv")


WSDA_DataTable = pd.read_csv(SF_data_dir + "WSDA_DataTable_" + str(SF_year) + ".csv")
WSDA_DataTable = WSDA_DataTable[WSDA_DataTable.county == given_county]
WSDA_DataTable["DataSrc"] = WSDA_DataTable["DataSrc"].str.lower()
WSDA_DataTable["CropTyp"] = WSDA_DataTable["CropTyp"].str.lower()


f_name = given_county  + "_" + str(SF_year) + "_regular_" + indeks + \
         "_SG_win" + str(SG_win_size) + "_Order" + str(SG_order) + ".csv"

a_df = pd.read_csv(data_dir + f_name, low_memory=False)

"""
Generate different datafamres based on the following variables
2 * 2 * 2 * 2 different combinations of NASS, Double_by_Notes, Irrigated, LastSurveyYear
For now, leave out the fucking last survey year!

In the following we have abbreviated:

AF: All Fields
DP: Double Potential Fields (i.e. perennials out)
Irr: Just Irrigated Fields
BothIrr: both irrigated and non-irrigated

"""





