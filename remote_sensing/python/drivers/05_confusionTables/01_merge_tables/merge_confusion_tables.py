  
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
###                   Aeolus Directories
###
####################################################################################

data_dir = "/data/hydro/users/Hossein/remote_sensing/05_Regular_SOS_confusions/00_allYCtables_separate/"
output_dir = "/data/hydro/users/Hossein/remote_sensing/05_Regular_SOS_confusions/01_allYCtables_merged/"

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

counties = ["Grant", "Whitman", "Asotin",
            "Garfield", "Ferry", "Franklin",
            "Columbia", "Adams","Benton",
            "Chelan", "Douglas", "Kittitas", "Klickitat",
            "Lincoln", "Okanogan", "Spokane", "Stevens",
            "Yakima",'Pend_Oreille', 'Walla_Walla']

years = [2016, 2017, 2018]
indekses = ["EVI"]           # EVI      NDVI
perennials_out = ["PereOut"] # PereIn   PereOut
NASS_out = ["NASSin"]        # NASSin   NASSOut
non_Irr_out = ["JustIrr"]    # JustIrr  BothIrr 

double_by_Note = ["dblNotFiltered"] # dblNotFiltered  onlyDblByNotes

season_counts = ["morethan2seasons", "exactly2seasons"] 


list_of_files = os.listdir(data_dir)

list_2016 = [f for f in list_of_files if "2016" in f]
list_2017 = [f for f in list_of_files if "2017" in f]
list_2018 = [f for f in list_of_files if "2018" in f]

print (list_2016[0:2])
print (list_2017[0:2])
print (list_2018[0:2])


pat_1 = "EVI_PereOut_NASSin_JustIrr_dblNotFiltered_confusion_Acr_morethan2seasons_regular"
curr_output_dir = output_dir + pat_1 + "/"
os.makedirs(curr_output_dir, exist_ok=True)

list_2016_EVI_PereOut_NASSin_JustIrr_dblNotFiltered_confusion_Acr_morethan2seasons_regular = [f for f in list_2016 if pat_1 in f]
list_2017_EVI_PereOut_NASSin_JustIrr_dblNotFiltered_confusion_Acr_morethan2seasons_regular = [f for f in list_2017 if pat_1 in f]
list_2018_EVI_PereOut_NASSin_JustIrr_dblNotFiltered_confusion_Acr_morethan2seasons_regular = [f for f in list_2018 if pat_1 in f]

list_of_lists = [list_2016_EVI_PereOut_NASSin_JustIrr_dblNotFiltered_confusion_Acr_morethan2seasons_regular, 
                 list_2017_EVI_PereOut_NASSin_JustIrr_dblNotFiltered_confusion_Acr_morethan2seasons_regular,
                 list_2018_EVI_PereOut_NASSin_JustIrr_dblNotFiltered_confusion_Acr_morethan2seasons_regular]

for a_list in list_of_lists:
    output_df = pd.DataFrame()
    for a_file in a_list:
        f_name = data_dir + a_file
        curr_file = pd.read_csv(f_name, low_memory=False)
        curr_county = a_file.split("_")[0]
        print (curr_county)
        print (curr_file.shape)
        curr_file['county'] = curr_county
        print (curr_file.shape)
        output_df = pd.concat([output_df, curr_file])

    if "2016" in a_file:
        output_name = curr_output_dir + "allCounties_separate_2016_confusion.csv"
        eastern_out_name = curr_output_dir + "eastern_2016_confusion.csv"
    elif "2017" in a_file:
        output_name = curr_output_dir + "allCounties_separate_2017_confusion.csv"
        eastern_out_name = curr_output_dir + "eastern_2017_confusion.csv"
    elif "2018" in a_file:
        output_name = curr_output_dir + "allCounties_separate_2018_confusion.csv"
        eastern_out_name = curr_output_dir + "eastern_2018_confusion.csv"

    output_df.to_csv(output_name, index = False)

    eastern_confusion = output_df.groupby(['parameters']).sum()
    eastern_confusion.to_csv(eastern_out_name, index = False)









