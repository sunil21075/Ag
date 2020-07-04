####
#### May 26, 2019
####

"""
Just generate peak tables for Grant 2018 Irrigated fields 
for all cultivars; EVI and my peak finder

"""

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

# search path for modules
# look @ https://stackoverflow.com/questions/67631/how-to-import-a-module-given-the-full-path


####################################################################################
###
###                      Local
###
####################################################################################

################
###
### Core path
###

sys.path.append('/Users/hn/Documents/00_GitHub/Ag/remote_sensing/python/')

################
###
### Directories
###
data_dir = "/Users/hn/Documents/01_research_data/remote_sensing/" + \
           "01_NDVI_TS/04_Irrigated_eastern_Cloud70/Grant_2018_irrigated/"

param_dir = "/Users/hn/Documents/00_GitHub/Ag/remote_sensing/parameters/"
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

data_dir = "/data/hydro/users/Hossein/remote_sensing/01_NDVI_TS/allFs_NotCorrectYears/"
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
freedom_df = 7

Sav_win_size = 9
sav_order = 1
delt = 0.1

Sav_win_size = int(sys.argv[1])
sav_order = int(sys.argv[2])
delt = float(sys.argv[3])
indeks = sys.argv[4]

plot_sub_dir = "w_plot"
print ("delta = {fileShape}.".format(fileShape=delt))

####################################################################################
###
###                   process data
###
####################################################################################
f_name = "Grant_2018_allFs_notCorrectYrs_70cloud.csv"
a_df = pd.read_csv(data_dir + f_name, low_memory=False)


######################
                                                                        # last_part_name + \
output_dir = data_dir + "/savitzky_" + indeks + "/Grant_Irrigated_2018_" + plot_sub_dir + \
             "/delta" + str(delt) + "_Sav_win" + str(Sav_win_size) + "_Order"  + str(sav_order) + "/"

plot_dir_base = output_dir
print ("plot_dir_base is " + plot_dir_base)

os.makedirs(output_dir, exist_ok=True)
os.makedirs(plot_dir_base, exist_ok=True)

######################
a_df['year'] = 2018

# The following columns do not exist in the old data
#
if not('DataSrc' in a_df.columns):
    print ("Data source is being set to NA")
    a_df['DataSrc'] = "NA"

if not('CovrCrp' in a_df.columns):
    print ("Data source is being set to NA")
    a_df['CovrCrp'] = "NA"

if (indeks == "EVI"):
    a_df = rc.initial_clean_EVI(a_df)
else:
    a_df = rc.initial_clean_NDVI(a_df)

a_df.head(2)
an_EE_TS = a_df.copy()

### List of unique polygons
polygon_list = an_EE_TS['geo'].unique()
print(len(polygon_list))

max_output_columns = ['ID', 'Acres', 'CovrCrp', 'CropGrp', 'CropTyp',
                      'DataSrc', 'ExctAcr', 'IntlSrD', 'Irrigtn', 'LstSrvD', 'Notes',
                      'RtCrpTy', 'Shap_Ar', 'Shp_Lng', 'TRS', 'county', 'year', 'geo',
                      'max_Doy', 'max_value', 'max_count']

all_poly_and_maxs_savitzky = pd.DataFrame(data=None, 
                                          index=np.arange(3*len(an_EE_TS)), 
                                          columns=max_output_columns)


min_output_columns = ['ID', 'Acres', 'CovrCrp', 'CropGrp', 'CropTyp',
                      'DataSrc', 'ExctAcr', 'IntlSrD', 'Irrigtn', 'LstSrvD', 'Notes',
                      'RtCrpTy', 'Shap_Ar', 'Shp_Lng', 'TRS', 'county', 'year', 'geo',
                      'min_Doy', 'min_value', 'min_count']

all_poly_and_mins_savitzky = pd.DataFrame(data=None, 
                                          index=np.arange(3*len(an_EE_TS)), 
                                          columns=min_output_columns)

pointer_max_savitzky = 0
pointer_min_savitzky = 0

counter = 0

for a_poly in polygon_list:
    if (counter%1000 == 0):
        print (counter)
    counter += 1
    curr_field = an_EE_TS[an_EE_TS['geo']==a_poly].copy()
    ################################################################
    # Sort by DoY (sanitary check)
    curr_field.sort_values(by=['doy'], inplace=True)

    ################################################################

    year = int(curr_field['year'].unique())
    plant = curr_field['CropTyp'].unique()[0]

    # Take care of names, replace "/" and "," and " " by "_"
    plant = plant.replace("/", "_")
    plant = plant.replace(",", "_")
    plant = plant.replace(" ", "_")
    plant = plant.replace("__", "_")

    county = curr_field['county'].unique()[0]
    ID = curr_field['ID'].unique()[0]

    ### 
    ###  There is a chance that a polygon is repeated twice?
    ###

    X = curr_field['doy']
    y = curr_field[indeks]

    #############################################
    ###
    ###             Smoothen
    ###
    #############################################
    # differences are minor, but lets keep using Pythons function
    # my_savitzky_pred = rc.savitzky_golay(y, window_size=Sav_win_size, order=sav_order)

    savitzky_pred = scipy.signal.savgol_filter(y, window_length= Sav_win_size, polyorder=sav_order)

    #############################################
    ###
    ###             find peaks
    ###
    #############################################
    #################################################################################
    #
    #    savitzky
    #

    savitzky_max_min = rc.my_peakdetect(y_axis=savitzky_pred, x_axis=X, delta=delt);

    savitzky_max =  savitzky_max_min[0];
    savitzky_min =  savitzky_max_min[1];

    savitzky_max = rc.separate_x_and_y(m_list = savitzky_max);
    savitzky_min = rc.separate_x_and_y(m_list = savitzky_min);

    savitzky_max_DoYs_series = pd.Series(savitzky_max[0]);
    savitzky_max_series = pd.Series(savitzky_max[1]);

    savitzky_min_DoYs_series = pd.Series(savitzky_min[0]);
    savitzky_min_series = pd.Series(savitzky_min[1]);


    savitzky_max_df = pd.DataFrame({ 
                           'max_Doy': savitzky_max_DoYs_series,
                           'max_value': savitzky_max_series
                          })
    # add number of max to the data frame.
    savitzky_max_df['max_count'] = savitzky_max_df.shape[0]

    savitzky_min_df = pd.DataFrame({ 
                           'min_Doy': savitzky_min_DoYs_series,
                           'min_value': savitzky_min_series
                          })
    # add number of max to the data frame.
    savitzky_min_df['max_count'] = savitzky_min_df.shape[0]
    ########################################################################################################
    ########################################################################################################

    #############################################
    ###
    ###             plot
    ###
    #############################################
    if do_plot == True:
        sub_out = "/plant_based_plots/" + plant + "/"
        plot_path = plot_dir_base + sub_out
        plot_path = plot_path + str(savitzky_max_df.shape[0]) + "_peaks/"
        os.makedirs(plot_path, exist_ok=True)
        # print ("plot_path is " + plot_path)
        if (len(os.listdir(plot_path))<50):
            
            plot_title = county + ", " + plant + ", " + str(year) + " (" + ID + ")"
            sb.set();

            fig, ax = plt.subplots(figsize=(8,6));
            ax.scatter(X, y, label="Data", s=30);

            ax.plot(X, savitzky_pred, 'k--', label="savitzky")
            ax.scatter(savitzky_max_DoYs_series, savitzky_max_series, s=200, c='k', marker='*');

            ax.set_title(plot_title);
            ax.set(xlabel='DoY', ylabel=indeks)

            ################################################
            #
            #    bare soil indices plots
            #

            an_EE_TS_BSI = rc.initial_clean(df = curr_field, column_to_be_cleaned='BSI')
            # an_EE_TS_NDWI = rc.initial_clean(df = curr_field, column_to_be_cleaned='NDWI')
            an_EE_TS_PSRI = rc.initial_clean(df = curr_field, column_to_be_cleaned='PSRI')
            an_EE_TS_LSWI = rc.initial_clean(df = curr_field, column_to_be_cleaned='LSWI')

            ax.plot(an_EE_TS_BSI['doy'], an_EE_TS_BSI['BSI'], label="BSI")
            # ax.plot(x_NDWI, y_NDWI, label="NWDI")

            ax.plot(an_EE_TS_PSRI['doy'], an_EE_TS_PSRI['PSRI'], label="PSRI")
            ax.plot(an_EE_TS_LSWI['doy'], an_EE_TS_LSWI['LSWI'], label="LSWI")

            ax.legend(loc="best");
            fig_name = plot_path + county + "_" + plant + "_" + str(year) + "_" + str(counter) + '.png'
            plt.savefig(fname = fig_name, \
                         dpi=300,
                         bbox_inches='tight')
            plt.close()
            del(plot_path, sub_out) #  county, plant, year
 