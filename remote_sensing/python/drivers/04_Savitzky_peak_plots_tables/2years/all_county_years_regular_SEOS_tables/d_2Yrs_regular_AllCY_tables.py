####
#### Aug. 31
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

# search path for modules
# look @ https://stackoverflow.com/questions/67631/how-to-import-a-module-given-the-full-path


# ####################################################################################
# ###
# ###                      Local
# ###
# ####################################################################################

# ################################################################################
# ###
# ### Core path
# ###

# sys.path.append('/Users/hn/Documents/00_GitHub/Ag/remote_sensing/python/')

# ################
# ###
# ### Directories
# ###
# data_dir = "/Users/hn/Documents/01_research_data/remote_sensing/" + \
#            "01_NDVI_TS/04_Irrigated_eastern_Cloud70/Grant_2018_irrigated/"

# param_dir = "/Users/hn/Documents/00_GitHub/Ag/remote_sensing/parameters/"
####################################################################################
###
###                      Aeolus Core path
###
####################################################################################

sys.path.append('/home/hnoorazar/remote_sensing_codes/')

import remote_sensing_core as rc
import remote_sensing_plot_core as rcp

####################################################################################
###
###      Parameters                   
###
####################################################################################
SG_win_size = 5
SG_order = 3
delt = 0.4
SF_year = 2017

given_county = sys.argv[1]
SF_year = int(sys.argv[2])
indeks = sys.argv[3]
SG_params = int(sys.argv[4])
delt = float(sys.argv[5])

SG_win_size = int(SG_params / 10) # grab the first digit as window size
SG_order = SG_params % 10 # grab the second digit as poly. order

print ("given_county is " + given_county.replace("_", " "))
print("SG_params is {}.".format(SG_params))
print("SG_win_size is {} and SG_order is {}.".format(SG_win_size, SG_order))

###
### White SOS and EOS params
###
onset_cut = 0.5
offset_cut = 0.5

regularized = True


print ("delta = {fileShape}.".format(fileShape = delt))

####################################################################################
###
###                   Aeolus Directories
###
####################################################################################
param_dir = "/home/hnoorazar/remote_sensing_codes/parameters/"
annual_crops = pd.read_csv(param_dir, "double_crop_potential_plants.csv")

regular_data_dir = "/data/hydro/users/Hossein/remote_sensing/03_Regularized_TS/70_cloud/2Yrs/noJump_Regularized/"
regular_output_dir = "/data/hydro/users/Hossein/remote_sensing/04_noJump_Regularized_plt_tbl_SOSEOS/2Yrs_tables_regular/"

f_name = "01_Regular_filledGap_" + given_county + "_SF_" + str(SF_year) + "_" + indeks + ".csv"

data_dir = regular_data_dir
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

a_df = pd.read_csv(data_dir + f_name, low_memory=False)
#
# Toss perennials and alfalfa (This step will be done in creating the confusion table.)
#
# a_df = a_df[a_df['CropTyp'].isin(annual_crops.Crop_Type)]

print ("columns of a_df right after reading is: ")
print (a_df.columns)

#####################################################################
##
## it seems Date and human_system_start_time are identical!!!
##
#####################################################################
a_df['Date'] = pd.to_datetime(a_df.Date.values).values

if "human_system_start_time" in list(a_df.columns):
    a_df['human_system_start_time'] = pd.to_datetime(a_df.human_system_start_time.values).values

####################################################################################
###
###                   process data
###
####################################################################################

"""
 Tables does not have to be exact. So, we do not need 
 to filter out NASS, and filter by last survey date
"""

a_df = a_df[a_df['county']== given_county.replace("_", " ")] # Filter the given county; given_county
a_df.reset_index(drop=True, inplace=True)

# a_df = filter_out_NASS(a_df) # Toss NASS
# a_df = filter_by_lastSurvey(a_df, year = SF_year) # filter by last survey date


# filter_NASS = False
# filter_lastSurDate = False

# if filter_NASS == True:
#     if filter_lastSurDate == True:
#         print ("1")
#         last_part_name = "NassOut_CorrectYear"
#     elif filter_lastSurDate == False:
#         print ("2")
#         last_part_name = "NassOut_NotCorrectYear"

# if filter_NASS == False:
#     if filter_lastSurDate == True:
#         print ("3")
#         last_part_name = "NassIn_CorrectYears"
#     elif filter_lastSurDate == False:
#         print ("4")
#         last_part_name = "NassIn_NotCorrectYears"

# print(last_part_name)
# print ("filter_NASS is " + str(filter_NASS))
# print ("filter_lastSurDate is " + str(filter_lastSurDate))


# if (filter_NASS == True):
#     a_df = rc.filter_by_lastSurvey(dt_df_surv = a_df, year=2018)
#     print ("After filtering by last survey date, a_df is of dimension {fileShape}.".format(fileShape=a_df.shape))

# if (filter_lastSurDate == True):
#     a_df = rc.filter_out_NASS(dt_df_NASS = a_df)
#     print ("After filtering out NASS, a_df is of dimension {fileShape}.".format(fileShape=a_df.shape))
######################


# a_df['year'] = SF_year
#
# The following columns do not exist in the old data
#
if not('DataSrc' in a_df.columns):
    print ("_________________________________________________________")
    print ("Data source is being set to NA")
    a_df['DataSrc'] = "NA"

a_df = rc.initial_clean(df = a_df, column_to_be_cleaned = indeks)
a_df = a_df.copy()

### List of unique polygons
polygon_list = a_df['ID'].unique()

print ("_________________________________________________________")
print("polygon_list is of length {}.".format(len(polygon_list)))

# 
# 25 columns
#
SEOS_output_columns = ['ID', 'Acres', 'county', 'CropGrp', 'CropTyp', 'DataSrc', 'ExctAcr',
                       'IntlSrD', 'Irrigtn', 'LstSrvD', 'Notes', 'RtCrpTy', 'Shap_Ar',
                       'Shp_Lng', 'TRS', 'image_year', 'SF_year', 'doy', 'EVI',
                       'human_system_start_time', 'Date', 
                       'EVI_ratio', 'SOS', 'EOS', 'season_count']

#
# The reason I am multiplying len(a_df) by 4 is that we can have at least two
# seasons which means 2 SOS and 2 EOS. So, at least 4 rows are needed.
#
all_poly_and_SEOS = pd.DataFrame(data = None, 
                                 index = np.arange(4*len(a_df)), 
                                 columns = SEOS_output_columns)


counter = 0
pointer_SEOS_tab = 0

###########
###########  Re-order columns of the read data table to be consistent with the output columns
###########
a_df = a_df[SEOS_output_columns[0:21]]

for a_poly in polygon_list:
    if (counter % 10 == 0):
        print ("_________________________________________________________")
        print ("counter: " + str(counter))
    curr_field = a_df[a_df['ID']==a_poly].copy()
    curr_field.reset_index(drop=True, inplace=True)
    
    if (not("human_system_start_time" in list(curr_field.columns))):
        curr_field = rc.add_human_start_time_by_YearDoY(curr_field)

    ################################################################
    # Sort by DoY (sanitary check)
    curr_field.sort_values(by=['human_system_start_time'], inplace=True)

    if len(curr_field.image_year.unique()) != 1:
        print (curr_field.image_year.unique())
        raise ValueError("image year must be unique at this point!!!")

    ### 
    ###  There is a chance that a polygon is repeated twice?
    ###

    X = curr_field['doy']
    y = curr_field[indeks]

    #######################################################################
    ###
    ###   Smoothen
    ###
    """
    Here we do the SG filtering smoothing with 1.5 years worth of data
    """
    SG_pred = scipy.signal.savgol_filter(y, window_length= SG_win_size, polyorder=SG_order)

    # SG might violate the boundaries. clip them:
    SG_pred[SG_pred > 1 ] = 1
    SG_pred[SG_pred < -1 ] = -1

    curr_field[indeks] = SG_pred

    y_orchard = curr_field[curr_field['doy'] >= 122]
    y_orchard = y_orchard[y_orchard['doy'] <= 305]
    y_orchard_range = max(y_orchard[indeks]) - min(y_orchard[indeks])

    if y_orchard_range > 0.3:
        #######################################################################
        ###
        ###             find SOS and EOS, and add them to the table
        ###
        #######################################################################
        curr_field = curr_field[curr_field['image_year'] == SF_year]


        # create the full calenadr to make better estimation of SOS and EOS.
        fine_granular_table = rc.create_calendar_table(SF_year = SF_year)
        fine_granular_table = pd.merge(fine_granular_table, curr_field, on=['Date', 'SF_year', 'doy'], how='left')

        ###### We need to fill the NAs that are created because they were not created in fine_granular_table
        fine_granular_table["image_year"] = curr_field["image_year"].unique()[0]
        fine_granular_table["ID"] = curr_field["ID"].unique()[0]
        fine_granular_table["Acres"] = curr_field["Acres"].unique()[0]
        fine_granular_table["county"] = curr_field["county"].unique()[0]

        fine_granular_table["CropGrp"] = curr_field["CropGrp"].unique()[0]
        fine_granular_table["CropTyp"] = curr_field["CropTyp"].unique()[0]
        fine_granular_table["DataSrc"] = curr_field["DataSrc"].unique()[0]
        fine_granular_table["ExctAcr"] = curr_field["ExctAcr"].unique()[0]
        
        fine_granular_table["IntlSrD"] = curr_field["IntlSrD"].unique()[0]
        fine_granular_table["Irrigtn"] = curr_field["Irrigtn"].unique()[0]

        fine_granular_table["LstSrvD"] = curr_field["LstSrvD"].unique()[0]
        fine_granular_table["Notes"] = curr_field["Notes"].unique()[0]
        fine_granular_table["RtCrpTy"] = curr_field["RtCrpTy"].unique()[0]
        fine_granular_table["Shap_Ar"] = curr_field["Shap_Ar"].unique()[0]
        fine_granular_table["Shp_Lng"] = curr_field["Shp_Lng"].unique()[0]
        fine_granular_table["TRS"] = curr_field["TRS"].unique()[0]

        fine_granular_table = rc.add_human_start_time_by_YearDoY(fine_granular_table)

        # replace NAs with -1.5. Because, that is what the function fill_theGap_linearLine()
        # uses as indicator for missing values
        fine_granular_table.fillna(value={indeks:-1.5}, inplace=True)
        fine_granular_table = rc.fill_theGap_linearLine(regular_TS = fine_granular_table, V_idx=indeks, SF_year=SF_year)

        curr_field = rc.addToDF_SOS_EOS_White(pd_TS = curr_field, 
                                              VegIdx = indeks, 
                                              onset_thresh = onset_cut, 
                                              offset_thresh = offset_cut)

        ##
        ##  Kill false detected seasons 
        ##
        curr_field = rc.Null_SOS_EOS_by_DoYDiff(pd_TS = curr_field, min_season_length=40)

        #
        # extract the SOS and EOS rows 
        #
        SEOS = curr_field[(curr_field['SOS'] != 0) | curr_field['EOS'] != 0]
        SEOS = SEOS.copy()
        # SEOS = SEOS.reset_index() # not needed really
        SOS_tb = curr_field[curr_field['SOS'] != 0]
        if len(SOS_tb) >= 2:
            SEOS["season_count"] = len(SOS_tb)
            all_poly_and_SEOS[pointer_SEOS_tab:(pointer_SEOS_tab+len(SEOS))] = SEOS.values
            pointer_SEOS_tab += len(SEOS)
        else:
            aaa = curr_field.iloc[0].values.reshape(1, len(curr_field.iloc[0]))
            aaa = np.append(aaa, [1])
            aaa = aaa.reshape(1, len(aaa))

            all_poly_and_SEOS.iloc[pointer_SEOS_tab:(pointer_SEOS_tab+1)] = aaa
            pointer_SEOS_tab += 1
    else: # here are potentially apples, cherries, etc.
        # we did not add EVI_ratio, SOS, and EOS. So, we are missing these
        # columns in the data frame. So, use 666 as proxy
        aaa = np.append(curr_field.iloc[0], [666, 666, 666, 1])
        aaa = aaa.reshape(1, len(aaa))
        all_poly_and_SEOS.iloc[pointer_SEOS_tab:(pointer_SEOS_tab+1)] = aaa
        pointer_SEOS_tab += 1

    counter += 1

####################################################################################
###
###                   Write the outputs
###
####################################################################################
given_county = given_county.replace(" ", "_")

all_poly_and_SEOS = all_poly_and_SEOS[0:(pointer_SEOS_tab)]

out_name = output_dir + given_county + "_" + str(SF_year) + "_regular_" + indeks + \
           "_SG_win" + str(SG_win_size) + "_Order"  + str(SG_order) + ".csv"

all_poly_and_SEOS.to_csv(out_name, index = False)


print ("done")
end_time = time.time()
print(end_time - start_time)


