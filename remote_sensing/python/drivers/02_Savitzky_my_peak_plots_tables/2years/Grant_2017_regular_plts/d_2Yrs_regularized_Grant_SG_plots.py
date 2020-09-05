####
#### July 2. This is a copy of the version we had from before. plotting one year.
#### Here we are extending it to 2 years. Since August of a given year to the end
#### of the next year.
####


import matplotlib.backends.backend_pdf
import csv
import numpy as np
import pandas as pd
# import geopandas as gpd
from IPython.display import Image
# from shapely.geometry import Point, Polygon
from math import factorial
import datetime
from datetime import date
import time
import scipy
import scipy.signal
import os, os.path
import matplotlib

from statsmodels.sandbox.regression.predstd import wls_prediction_std
from sklearn.linear_model import LinearRegression
from patsy import cr

# from pprint import pprint
import matplotlib.pyplot as plt
import seaborn as sb

from pandas.plotting import register_matplotlib_converters
register_matplotlib_converters()

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

####################################################################################
###
###                      Aeolus Core path
###
####################################################################################

sys.path.append('/home/hnoorazar/remote_sensing_codes/')

###
### Import remote cores
###
import remote_sensing_core as rc
import remote_sensing_plot_core as rcp

####################################################################################
###
###      Parameters                   
###
####################################################################################
eleven_colors = ["gray", "lightcoral", "red", "peru",
                 "darkorange", "gold", "olive", "green",
                 "blue", "violet", "deepskyblue"]

# indeks = "EVI"
# irrigated_only = 1
# SF_year = 2017

given_county = "Grant"

jumps = sys.argv[1]
indeks = sys.argv[2]
irrigated_only = int(sys.argv[3])
SF_year = int(sys.argv[4])
given_county = sys.argv[5]
regularized = True

sos_thresh = 0.4
eos_thresh = 0.4
minFinderDetla = 0.4
####################################################################################
###
###                   Aeolus Directories
###
####################################################################################
output_base = "/data/hydro/users/Hossein/remote_sensing/04_noJump_Regularized_plt_tbl_SOSEOS/"

if irrigated_only == True:
    output_Irr = "irrigated_only"
else:
    output_Irr = "non_irrigated_only"

regular_data_dir = "/data/hydro/users/Hossein/remote_sensing/03_Regularized_TS/70_cloud/2Yrs/"

# if jumps == "yes":
#     regular_output_dir = output_base + "/2Yrs_plots_70Cloud_Regular_wJumps/" + \
#                          str(SF_year) + "_regular_" + output_Irr + "_" + indeks + \
#                          "_SOS" + str(int(sos_thresh*10))+ "_EOS" + str(int(eos_thresh*10)) + "/"
# else:
regular_data_dir = regular_data_dir + "/noJump_Regularized/"
regular_output_dir = output_base + "/2Yrs_plots_70Cloud_Regular_noJumps/" + \
                     str(SF_year) + "_regular_" + output_Irr + "_" + indeks + \
                     "_SOS" + str(int(sos_thresh*10)) + "_EOS" + str(int(eos_thresh*10)) + "/"

f_name = "01_Regular_filledGap_" + given_county + "_SF_" + str(SF_year) + "_" + indeks + ".csv"

plot_dir_base = regular_output_dir
print ("plot_dir_base is " + plot_dir_base)
param_dir = "/home/hnoorazar/remote_sensing_codes/parameters/"

#####################################################################################
data_dir = regular_data_dir
output_dir = regular_output_dir
plot_dir_base = output_dir 
print ("plot_dir_base is " + plot_dir_base)

os.makedirs(output_dir, exist_ok=True)
os.makedirs(plot_dir_base, exist_ok=True)

print ("_________________________________________________________")
print ("data dir is:")
print (data_dir)
print ("_________________________________________________________")
print ("output_dir is:")
print (output_dir)
print ("_________________________________________________________")

####################################################################################
###
###                   Read data
###
####################################################################################

a_df = pd.read_csv(data_dir + f_name, low_memory=False)

if 'Date' in a_df.columns:
    if type(a_df.Date.iloc[0]) == str:
        a_df['Date'] = pd.to_datetime(a_df.Date.values).values

##################################################################
##################################################################
####
####  plots has to be exact. So, we need 
####  to filter out NASS, and filter by last survey date
####
##################################################################
##################################################################

a_df = a_df[a_df['county'] == given_county.replace("_", " ")] # Filter Grant
a_df = rc.filter_out_NASS(a_df) # Toss NASS
a_df = rc.filter_by_lastSurvey(a_df, year = SF_year) # filter by last survey date
a_df['SF_year'] = SF_year

if irrigated_only == True:
    a_df = rc.filter_out_nonIrrigated(a_df)
    output_Irr = "irrigated_only"
else:
    output_Irr = "non_irrigated_only"
    a_df = rc.filter_out_Irrigated(a_df)

######################

# The following columns do not exist in the old data
#
if not('DataSrc' in a_df.columns):
    print ("Data source is being set to NA")
    a_df['DataSrc'] = "NA"

if not('CovrCrp' in a_df.columns):
    print ("CovrCrp is being set to NA")
    a_df['CovrCrp'] = "NA"


a_df = rc.initial_clean(df = a_df, column_to_be_cleaned = indeks)

an_EE_TS = a_df.copy()
del(a_df)

### List of unique polygons
polygon_list = np.sort(an_EE_TS['ID'].unique())
print ("_____________________________________")
print("len(polygon_list)")
print (len(polygon_list))
print ("_____________________________________")

counter = 0

for a_poly in polygon_list:
    if (counter%10 == 0):
        print ("_____________________________________")
        print ("counter: " + str(counter))
        print (a_poly)

    curr_field = an_EE_TS[an_EE_TS['ID']==a_poly].copy()

    #
    #  filter just one year to have a clean SOS EOS stuff
    #
    curr_field = curr_field[curr_field.image_year == SF_year]
    
    ################################################################
    # Sort by DoY (sanitary check)
    curr_field.sort_values(by=['image_year', 'doy'], inplace=True)

    ################################################################
    ID = curr_field['ID'].unique()[0]
    plant = curr_field['CropTyp'].unique()[0]
    plant = plant.replace("/", "_")
    plant = plant.replace(",", "_")
    plant = plant.replace(" ", "_")
    plant = plant.replace("__", "_")

    county = given_county # curr_field['county'].unique()[0]

    sub_out = plant + "/" # "/plant_based_plots/" + plant + "/"
    plot_path = plot_dir_base + sub_out
    plot_path = plot_path   # +  str(len(SG_max_DoYs_series)) + "_peaks/"
    os.makedirs(plot_path, exist_ok=True)
    # print ("plot_path is " + plot_path)
    if (len(os.listdir(plot_path)) < 100):
        # 
        #  Set up Canvas
        #
        fig, axs = plt.subplots(2, 2, figsize=(20,12),
                        sharex='col', sharey='row',
                        gridspec_kw={'hspace': 0.1, 'wspace': .1});

        (ax1, ax2), (ax3, ax4) = axs;
        ax1.grid(True); ax2.grid(True); ax3.grid(True); ax4.grid(True);

        rcp.SG_1yr_panels_clean_sciPy_My_Peaks_SOS(dataAB = curr_field, 
                                                   idx=indeks, 
                                                   SG_params=[5, 1], 
                                                   SFYr = SF_year, ax=ax1, deltA= minFinderDetla,
                                                   onset_cut = sos_thresh, 
                                                   offset_cut = eos_thresh);

        rcp.SG_1yr_panels_clean_sciPy_My_Peaks_SOS(dataAB = curr_field, 
                                                   idx=indeks, SG_params=[5, 3], 
                                                   SFYr=SF_year, ax=ax2, deltA=minFinderDetla,
                                                   onset_cut = sos_thresh, 
                                                   offset_cut = eos_thresh); 

        rcp.SG_1yr_panels_clean_sciPy_My_Peaks_SOS(dataAB = curr_field, 
                                                   idx = indeks, SG_params=[7, 3],
                                                   SFYr = SF_year, ax=ax3, deltA=minFinderDetla,
                                                   onset_cut = sos_thresh, 
                                                   offset_cut = eos_thresh);

        rcp.SG_1yr_panels_clean_sciPy_My_Peaks_SOS(dataAB = curr_field, 
                                                   idx=indeks, SG_params=[9, 3],
                                                   SFYr=SF_year, ax=ax4, deltA=minFinderDetla,
                                                   onset_cut = sos_thresh, 
                                                   offset_cut = eos_thresh)

        fig_name = plot_path + county + "_" + plant + "_SF_year_" + str(SF_year) + "_" + ID + '.png'

        os.makedirs(plot_dir_base, exist_ok=True)

        plt.savefig(fname = fig_name, dpi=250, bbox_inches='tight')
        plt.close('all')
        counter += 1


print ("done")
end_time = time.time()
print(end_time - start_time)


