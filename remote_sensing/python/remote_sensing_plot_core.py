# import libraries

import numpy as np
import pandas as pd
# import geopandas as gpd
from IPython.display import Image
# from shapely.geometry import Point, Polygon
from math import factorial
import datetime
import time
import scipy
from statsmodels.sandbox.regression.predstd import wls_prediction_std
from sklearn.linear_model import LinearRegression
from patsy import cr

from pprint import pprint
import matplotlib.pyplot as plt
import seaborn as sb

import os, os.path

import sys
# search path for modules
# look @ https://stackoverflow.com/questions/67631/how-to-import-a-module-given-the-full-path
sys.path.append('/Users/hn/Documents/00_GitHub/Ag/remote_sensing/python/')
import remote_sensing_core as rc

################################################################
#####
#####                   Function definitions
#####
################################################################
def savitzky_2yrs_panel(crr_fld, idx, deltA, SFYr, ax):

    if (not("human_system_start_time" in list(crr_fld.columns))):
        crr_fld = rc.add_human_start_time(crr_fld)

    eleven_colors = ["gray", "lightcoral", "red", "peru",
                     "darkorange", "gold", "olive", "green",
                     "blue", "violet", "deepskyblue"]

    plant = crr_fld['CropTyp'].unique()[0]
    # Take care of names, replace "/" and "," and " " by "_"
    plant = plant.replace("/", "_")
    plant = plant.replace(",", "_")
    plant = plant.replace(" ", "_")
    plant = plant.replace("__", "_")
    
    county = crr_fld['county'].unique()[0]
    ID = crr_fld['ID'].unique()[0]

    y = crr_fld[idx]

    #############################################
    ###
    ###             Smoothen
    ###
    #############################################
    # differences are minor, but lets keep using Pythons function
    # my_savitzky_pred = rc.savitzky_golay(y, window_size=Sav_win_size, order=sav_order)

    SG_pred_31 = scipy.signal.savgol_filter(y, window_length= 3, polyorder=1)
    SG_pred_32 = scipy.signal.savgol_filter(y, window_length= 3, polyorder=2)
    
    SG_pred_51 = scipy.signal.savgol_filter(y, window_length= 5, polyorder=1)
    SG_pred_52 = scipy.signal.savgol_filter(y, window_length= 5, polyorder=2)
    SG_pred_53 = scipy.signal.savgol_filter(y, window_length= 5, polyorder=3)
    
    SG_pred_71 = scipy.signal.savgol_filter(y, window_length= 7, polyorder=1)
    SG_pred_72 = scipy.signal.savgol_filter(y, window_length= 7, polyorder=2)
    SG_pred_73 = scipy.signal.savgol_filter(y, window_length= 7, polyorder=3)

    SG_pred_91 = scipy.signal.savgol_filter(y, window_length= 9, polyorder=1)
    SG_pred_92 = scipy.signal.savgol_filter(y, window_length= 9, polyorder=2)
    SG_pred_93 = scipy.signal.savgol_filter(y, window_length= 9, polyorder=3)
    #############################################
    ###
    ###             find peaks
    ###
    #############################################
    # X = crr_fld['doy']
    X = rc.extract_XValues_of_2Yrs_TS(crr_fld, SF_yr = SFYr)

    d = {'DoY': X, 'Date': pd.to_datetime(crr_fld.human_system_start_time.values).values}
    date_df = pd.DataFrame(data=d)
    """
    Jul 1.
    This function is written since Kirti said
    we do not need to have parts of the next year. i.e. 
    if we are looking at what is going on in a field in 2017,
    we only need data since Aug. 2016 till the end of 2017.
    We do not need anything in 2018.
    """

    SG_max_min_31 = rc.my_peakdetect(y_axis=SG_pred_31, x_axis=X, delta=deltA);
    SG_max_31 =  SG_max_min_31[0]; SG_min_31 =  SG_max_min_31[1];
    SG_max_31 = rc.separate_x_and_y(m_list = SG_max_31);
    SG_min_31 = rc.separate_x_and_y(m_list = SG_min_31);
    SG_max_DoYs_series_31 = pd.Series(SG_max_31[0]);
    SG_max_series_31 = pd.Series(SG_max_31[1]);
    SG_min_DoYs_series_31 = pd.Series(SG_min_31[0]);
    SG_min_series_31 = pd.Series(SG_min_31[1]);


    SG_max_min_32 = rc.my_peakdetect(y_axis=SG_pred_32, x_axis=X, delta=deltA);
    SG_max_32 =  SG_max_min_32[0]; SG_min_32 =  SG_max_min_32[1];
    SG_max_32 = rc.separate_x_and_y(m_list = SG_max_32);
    SG_min_32 = rc.separate_x_and_y(m_list = SG_min_32);
    SG_max_DoYs_series_32 = pd.Series(SG_max_32[0]);
    SG_max_series_32 = pd.Series(SG_max_32[1]);
    SG_min_DoYs_series_32 = pd.Series(SG_min_32[0]);
    SG_min_series_32 = pd.Series(SG_min_32[1]);

    ############
    ############ window 5
    ############

    SG_max_min_51 = rc.my_peakdetect(y_axis=SG_pred_51, x_axis=X, delta=deltA);
    SG_max_51 =  SG_max_min_51[0]; SG_min_51 =  SG_max_min_51[1];
    SG_max_51 = rc.separate_x_and_y(m_list = SG_max_51);
    SG_min_51 = rc.separate_x_and_y(m_list = SG_min_51);
    SG_max_DoYs_series_51 = pd.Series(SG_max_51[0]);
    SG_max_series_51 = pd.Series(SG_max_51[1]);
    SG_min_DoYs_series_51 = pd.Series(SG_min_51[0]);
    SG_min_series_51 = pd.Series(SG_min_51[1]);

    SG_max_min_52 = rc.my_peakdetect(y_axis=SG_pred_52, x_axis=X, delta=deltA);
    SG_max_52 =  SG_max_min_52[0]; SG_min_52 =  SG_max_min_52[1];
    SG_max_52 = rc.separate_x_and_y(m_list = SG_max_52);
    SG_min_52 = rc.separate_x_and_y(m_list = SG_min_52);
    SG_max_DoYs_series_52 = pd.Series(SG_max_52[0]);
    SG_max_series_52 = pd.Series(SG_max_52[1]);
    SG_min_DoYs_series_52 = pd.Series(SG_min_52[0]);
    SG_min_series_52 = pd.Series(SG_min_52[1]);

    SG_max_min_53 = rc.my_peakdetect(y_axis=SG_pred_53, x_axis=X, delta=deltA);
    SG_max_53 =  SG_max_min_53[0]; SG_min_53 =  SG_max_min_53[1];
    SG_max_53 = rc.separate_x_and_y(m_list = SG_max_53);
    SG_min_53 = rc.separate_x_and_y(m_list = SG_min_53);
    SG_max_DoYs_series_53 = pd.Series(SG_max_53[0]);
    SG_max_series_53 = pd.Series(SG_max_53[1]);
    SG_min_DoYs_series_53 = pd.Series(SG_min_53[0]);
    SG_min_series_53 = pd.Series(SG_min_53[1]);

    ############
    ############ window 7
    ############

    SG_max_min_71 = rc.my_peakdetect(y_axis=SG_pred_71, x_axis=X, delta=deltA);
    SG_max_71 =  SG_max_min_71[0]; SG_min_71 =  SG_max_min_71[1];
    SG_max_71 = rc.separate_x_and_y(m_list = SG_max_71);
    SG_min_71 = rc.separate_x_and_y(m_list = SG_min_71);
    SG_max_DoYs_series_71 = pd.Series(SG_max_71[0]);
    SG_max_series_71 = pd.Series(SG_max_71[1]);
    SG_min_DoYs_series_71 = pd.Series(SG_min_71[0]);
    SG_min_series_71 = pd.Series(SG_min_71[1]);

    SG_max_min_72 = rc.my_peakdetect(y_axis=SG_pred_72, x_axis=X, delta=deltA);
    SG_max_72 =  SG_max_min_72[0]; SG_min_72 =  SG_max_min_72[1];
    SG_max_72 = rc.separate_x_and_y(m_list = SG_max_72);
    SG_min_72 = rc.separate_x_and_y(m_list = SG_min_72);
    SG_max_DoYs_series_72 = pd.Series(SG_max_72[0]);
    SG_max_series_72 = pd.Series(SG_max_72[1]);
    SG_min_DoYs_series_72 = pd.Series(SG_min_72[0]);
    SG_min_series_72 = pd.Series(SG_min_72[1]);

    SG_max_min_73 = rc.my_peakdetect(y_axis=SG_pred_73, x_axis=X, delta=deltA);
    SG_max_73 =  SG_max_min_73[0]; SG_min_73 =  SG_max_min_73[1];
    SG_max_73 = rc.separate_x_and_y(m_list = SG_max_73);
    SG_min_73 = rc.separate_x_and_y(m_list = SG_min_73);
    SG_max_DoYs_series_73 = pd.Series(SG_max_73[0]);
    SG_max_series_73 = pd.Series(SG_max_73[1]);
    SG_min_DoYs_series_73 = pd.Series(SG_min_73[0]);
    SG_min_series_73 = pd.Series(SG_min_73[1]);

    ############
    ############ window 9
    ############

    SG_max_min_91 = rc.my_peakdetect(y_axis=SG_pred_91, x_axis=X, delta=deltA);
    SG_max_91 =  SG_max_min_91[0]; SG_min_91 =  SG_max_min_91[1];
    SG_max_91 = rc.separate_x_and_y(m_list = SG_max_91);
    SG_min_91 = rc.separate_x_and_y(m_list = SG_min_91);
    SG_max_DoYs_series_91 = pd.Series(SG_max_91[0]);
    SG_max_series_91 = pd.Series(SG_max_91[1]);
    SG_min_DoYs_series_91 = pd.Series(SG_min_91[0]);
    SG_min_series_91 = pd.Series(SG_min_91[1]);

    SG_max_min_92 = rc.my_peakdetect(y_axis=SG_pred_92, x_axis=X, delta=deltA);
    SG_max_92 =  SG_max_min_92[0]; SG_min_92 =  SG_max_min_92[1];
    SG_max_92 = rc.separate_x_and_y(m_list = SG_max_92);
    SG_min_92 = rc.separate_x_and_y(m_list = SG_min_92);
    SG_max_DoYs_series_92 = pd.Series(SG_max_92[0]);
    SG_max_series_92 = pd.Series(SG_max_92[1]);
    SG_min_DoYs_series_92 = pd.Series(SG_min_92[0]);
    SG_min_series_92 = pd.Series(SG_min_92[1]);

    SG_max_min_93 = rc.my_peakdetect(y_axis=SG_pred_93, x_axis=X, delta=deltA);
    SG_max_93 =  SG_max_min_93[0]; SG_min_93 =  SG_max_min_93[1];
    SG_max_93 = rc.separate_x_and_y(m_list = SG_max_93);
    SG_min_93 = rc.separate_x_and_y(m_list = SG_min_93);
    SG_max_DoYs_series_93 = pd.Series(SG_max_93[0]);
    SG_max_series_93 = pd.Series(SG_max_93[1]);
    SG_min_DoYs_series_93 = pd.Series(SG_min_93[0]);
    SG_min_series_93 = pd.Series(SG_min_93[1]);

    ########################################################################################################
    ########################################################################################################

    plotting_dic = { "SG 31" : [SG_pred_31, SG_max_DoYs_series_31, SG_max_series_31],
                     "SG 32" : [SG_pred_32, SG_max_DoYs_series_32, SG_max_series_32],

                     "SG 51" : [SG_pred_51, SG_max_DoYs_series_51, SG_max_series_51],
                     "SG 52" : [SG_pred_52, SG_max_DoYs_series_52, SG_max_series_52],
                     "SG 53" : [SG_pred_53, SG_max_DoYs_series_53, SG_max_series_53],

                     "SG 71" : [SG_pred_71, SG_max_DoYs_series_71, SG_max_series_71],
                     "SG 72" : [SG_pred_72, SG_max_DoYs_series_72, SG_max_series_72],
                     "SG 73" : [SG_pred_73, SG_max_DoYs_series_73, SG_max_series_73],

                     "SG 91" : [SG_pred_91, SG_max_DoYs_series_91, SG_max_series_91],
                     "SG 92" : [SG_pred_92, SG_max_DoYs_series_92, SG_max_series_92],
                     "SG 93" : [SG_pred_93, SG_max_DoYs_series_93, SG_max_series_93]
                     }

    #############################################
    ###
    ###             plot
    ###
    #############################################
    #
    #   We have to change this part to make a perfect plot
    #        
    plot_title = county + ", " + plant + " (" + ID + "), delta = " + str(deltA)
    ax.set_ylim([-1.7, 1.7])
    # sb.set();
    
    # fucking Aeolus throws error. TypeError: invalid type promotion
    # ax.scatter(date_df.Date, y, label="Raw data", s = 60); 
    ax.scatter(date_df.Date.values, y.values, label="Raw data", s = 60);

    for co, ite in enumerate(plotting_dic):
        lbl = ite + ", Peaks: " + str(len(plotting_dic[ite][2]))
        # ax.plot(X, plotting_dic[ite][0], label = lbl, c = eleven_colors[co])
        
        # fucking Aeolus throws error. TypeError: invalid type promotion
        # ax.plot(date_df.Date, plotting_dic[ite][0], label = lbl, c = eleven_colors[co])
        ax.plot(date_df.Date.values, plotting_dic[ite][0], label = lbl, c = eleven_colors[co])

        date_df_specific = date_df[date_df.DoY.isin(plotting_dic[ite][1])]
        
        # fucking Aeolus throws error. TypeError: invalid type promotion
                 # plotting_dic[ite][1]
        # ax.scatter(date_df_specific.Date, plotting_dic[ite][2], s=100, marker='*', c = eleven_colors[co]);
        ax.scatter(date_df_specific.Date.values, plotting_dic[ite][2], s=100, marker='*', c = eleven_colors[co]);

    ax.set_title(plot_title);
    ax.set(ylabel=idx) # xlabel='Time', 
    ax.legend(loc="best");


# def subplots_savitzky_2_yrs(crr_fld, idx, deltA, SFYr):
#     #
#     #  This function is fine. But, we repace it with savitzky_2_yrs_panel()
#     #

#     if (not("human_system_start_time" in list(crr_fld.columns))):
#         crr_fld = rc.add_human_start_time(crr_fld)

#     eleven_colors = ["gray", "lightcoral", "red", "peru",
#                      "darkorange", "gold", "olive", "green",
#                      "blue", "violet", "deepskyblue"]

#     plant = crr_fld['CropTyp'].unique()[0]
#     # Take care of names, replace "/" and "," and " " by "_"
#     plant = plant.replace("/", "_")
#     plant = plant.replace(",", "_")
#     plant = plant.replace(" ", "_")
#     plant = plant.replace("__", "_")
    
#     county = crr_fld['county'].unique()[0]
#     ID = crr_fld['ID'].unique()[0]

#     y = crr_fld[idx]

#     #############################################
#     ###
#     ###             Smoothen
#     ###
#     #############################################
#     # differences are minor, but lets keep using Pythons function
#     # my_savitzky_pred = rc.savitzky_golay(y, window_size=Sav_win_size, order=sav_order)

#     SG_pred_31 = scipy.signal.savgol_filter(y, window_length= 3, polyorder=1)
#     SG_pred_32 = scipy.signal.savgol_filter(y, window_length= 3, polyorder=2)
    
#     SG_pred_51 = scipy.signal.savgol_filter(y, window_length= 5, polyorder=1)
#     SG_pred_52 = scipy.signal.savgol_filter(y, window_length= 5, polyorder=2)
#     SG_pred_53 = scipy.signal.savgol_filter(y, window_length= 5, polyorder=3)
    
#     SG_pred_71 = scipy.signal.savgol_filter(y, window_length= 7, polyorder=1)
#     SG_pred_72 = scipy.signal.savgol_filter(y, window_length= 7, polyorder=2)
#     SG_pred_73 = scipy.signal.savgol_filter(y, window_length= 7, polyorder=3)

#     SG_pred_91 = scipy.signal.savgol_filter(y, window_length= 9, polyorder=1)
#     SG_pred_92 = scipy.signal.savgol_filter(y, window_length= 9, polyorder=2)
#     SG_pred_93 = scipy.signal.savgol_filter(y, window_length= 9, polyorder=3)

   
#     # preds_df = pd.DataFrame(data = {'SG 31':SG_pred_31, 'SG 32':SG_pred_32, 
#     #                                 'SG 51':SG_pred_51, 'SG 52':SG_pred_52, 'SG 53':SG_pred_53, 
#     #                                 'SG 71':SG_pred_71, 'SG 72':SG_pred_72, 'SG 73':SG_pred_73, 
#     #                                 'SG 91':SG_pred_91, 'SG 92':SG_pred_92, 'SG 93':SG_pred_93}, 
#     #                         index = X)
#     # df2.set_index(keys = ['a'], drop=True,  inplace=True)
#     #############################################
#     ###
#     ###             find peaks
#     ###
#     #############################################
#     # X = crr_fld['doy']
#     X = rc.extract_XValues_of_2Yrs_TS(crr_fld, SF_yr = SFYr)

#     d = {'DoY': X, 'Date': pd.to_datetime(crr_fld.human_system_start_time.values).values}
#     date_df = pd.DataFrame(data=d)
#     """
#     Jul 1.
#     This function is written since Kirti said
#     we do not need to have parts of the next year. i.e. 
#     if we are looking at what is going on in a field in 2017,
#     we only need data since Aug. 2016 till the end of 2017.
#     We do not need anything in 2018.
#     """

#     SG_max_min_31 = rc.my_peakdetect(y_axis=SG_pred_31, x_axis=X, delta=deltA);
#     SG_max_31 =  SG_max_min_31[0]; SG_min_31 =  SG_max_min_31[1];
#     SG_max_31 = rc.separate_x_and_y(m_list = SG_max_31);
#     SG_min_31 = rc.separate_x_and_y(m_list = SG_min_31);
#     SG_max_DoYs_series_31 = pd.Series(SG_max_31[0]);
#     SG_max_series_31 = pd.Series(SG_max_31[1]);
#     SG_min_DoYs_series_31 = pd.Series(SG_min_31[0]);
#     SG_min_series_31 = pd.Series(SG_min_31[1]);


#     SG_max_min_32 = rc.my_peakdetect(y_axis=SG_pred_32, x_axis=X, delta=deltA);
#     SG_max_32 =  SG_max_min_32[0]; SG_min_32 =  SG_max_min_32[1];
#     SG_max_32 = rc.separate_x_and_y(m_list = SG_max_32);
#     SG_min_32 = rc.separate_x_and_y(m_list = SG_min_32);
#     SG_max_DoYs_series_32 = pd.Series(SG_max_32[0]);
#     SG_max_series_32 = pd.Series(SG_max_32[1]);
#     SG_min_DoYs_series_32 = pd.Series(SG_min_32[0]);
#     SG_min_series_32 = pd.Series(SG_min_32[1]);

#     ############
#     ############ window 5
#     ############

#     SG_max_min_51 = rc.my_peakdetect(y_axis=SG_pred_51, x_axis=X, delta=deltA);
#     SG_max_51 =  SG_max_min_51[0]; SG_min_51 =  SG_max_min_51[1];
#     SG_max_51 = rc.separate_x_and_y(m_list = SG_max_51);
#     SG_min_51 = rc.separate_x_and_y(m_list = SG_min_51);
#     SG_max_DoYs_series_51 = pd.Series(SG_max_51[0]);
#     SG_max_series_51 = pd.Series(SG_max_51[1]);
#     SG_min_DoYs_series_51 = pd.Series(SG_min_51[0]);
#     SG_min_series_51 = pd.Series(SG_min_51[1]);

#     SG_max_min_52 = rc.my_peakdetect(y_axis=SG_pred_52, x_axis=X, delta=deltA);
#     SG_max_52 =  SG_max_min_52[0]; SG_min_52 =  SG_max_min_52[1];
#     SG_max_52 = rc.separate_x_and_y(m_list = SG_max_52);
#     SG_min_52 = rc.separate_x_and_y(m_list = SG_min_52);
#     SG_max_DoYs_series_52 = pd.Series(SG_max_52[0]);
#     SG_max_series_52 = pd.Series(SG_max_52[1]);
#     SG_min_DoYs_series_52 = pd.Series(SG_min_52[0]);
#     SG_min_series_52 = pd.Series(SG_min_52[1]);

#     SG_max_min_53 = rc.my_peakdetect(y_axis=SG_pred_53, x_axis=X, delta=deltA);
#     SG_max_53 =  SG_max_min_53[0]; SG_min_53 =  SG_max_min_53[1];
#     SG_max_53 = rc.separate_x_and_y(m_list = SG_max_53);
#     SG_min_53 = rc.separate_x_and_y(m_list = SG_min_53);
#     SG_max_DoYs_series_53 = pd.Series(SG_max_53[0]);
#     SG_max_series_53 = pd.Series(SG_max_53[1]);
#     SG_min_DoYs_series_53 = pd.Series(SG_min_53[0]);
#     SG_min_series_53 = pd.Series(SG_min_53[1]);

#     ############
#     ############ window 7
#     ############

#     SG_max_min_71 = rc.my_peakdetect(y_axis=SG_pred_71, x_axis=X, delta=deltA);
#     SG_max_71 =  SG_max_min_71[0]; SG_min_71 =  SG_max_min_71[1];
#     SG_max_71 = rc.separate_x_and_y(m_list = SG_max_71);
#     SG_min_71 = rc.separate_x_and_y(m_list = SG_min_71);
#     SG_max_DoYs_series_71 = pd.Series(SG_max_71[0]);
#     SG_max_series_71 = pd.Series(SG_max_71[1]);
#     SG_min_DoYs_series_71 = pd.Series(SG_min_71[0]);
#     SG_min_series_71 = pd.Series(SG_min_71[1]);

#     SG_max_min_72 = rc.my_peakdetect(y_axis=SG_pred_72, x_axis=X, delta=deltA);
#     SG_max_72 =  SG_max_min_72[0]; SG_min_72 =  SG_max_min_72[1];
#     SG_max_72 = rc.separate_x_and_y(m_list = SG_max_72);
#     SG_min_72 = rc.separate_x_and_y(m_list = SG_min_72);
#     SG_max_DoYs_series_72 = pd.Series(SG_max_72[0]);
#     SG_max_series_72 = pd.Series(SG_max_72[1]);
#     SG_min_DoYs_series_72 = pd.Series(SG_min_72[0]);
#     SG_min_series_72 = pd.Series(SG_min_72[1]);

#     SG_max_min_73 = rc.my_peakdetect(y_axis=SG_pred_73, x_axis=X, delta=deltA);
#     SG_max_73 =  SG_max_min_73[0]; SG_min_73 =  SG_max_min_73[1];
#     SG_max_73 = rc.separate_x_and_y(m_list = SG_max_73);
#     SG_min_73 = rc.separate_x_and_y(m_list = SG_min_73);
#     SG_max_DoYs_series_73 = pd.Series(SG_max_73[0]);
#     SG_max_series_73 = pd.Series(SG_max_73[1]);
#     SG_min_DoYs_series_73 = pd.Series(SG_min_73[0]);
#     SG_min_series_73 = pd.Series(SG_min_73[1]);

#     ############
#     ############ window 9
#     ############

#     SG_max_min_91 = rc.my_peakdetect(y_axis=SG_pred_91, x_axis=X, delta=deltA);
#     SG_max_91 =  SG_max_min_91[0]; SG_min_91 =  SG_max_min_91[1];
#     SG_max_91 = rc.separate_x_and_y(m_list = SG_max_91);
#     SG_min_91 = rc.separate_x_and_y(m_list = SG_min_91);
#     SG_max_DoYs_series_91 = pd.Series(SG_max_91[0]);
#     SG_max_series_91 = pd.Series(SG_max_91[1]);
#     SG_min_DoYs_series_91 = pd.Series(SG_min_91[0]);
#     SG_min_series_91 = pd.Series(SG_min_91[1]);

#     SG_max_min_92 = rc.my_peakdetect(y_axis=SG_pred_92, x_axis=X, delta=deltA);
#     SG_max_92 =  SG_max_min_92[0]; SG_min_92 =  SG_max_min_92[1];
#     SG_max_92 = rc.separate_x_and_y(m_list = SG_max_92);
#     SG_min_92 = rc.separate_x_and_y(m_list = SG_min_92);
#     SG_max_DoYs_series_92 = pd.Series(SG_max_92[0]);
#     SG_max_series_92 = pd.Series(SG_max_92[1]);
#     SG_min_DoYs_series_92 = pd.Series(SG_min_92[0]);
#     SG_min_series_92 = pd.Series(SG_min_92[1]);

#     SG_max_min_93 = rc.my_peakdetect(y_axis=SG_pred_93, x_axis=X, delta=deltA);
#     SG_max_93 =  SG_max_min_93[0]; SG_min_93 =  SG_max_min_93[1];
#     SG_max_93 = rc.separate_x_and_y(m_list = SG_max_93);
#     SG_min_93 = rc.separate_x_and_y(m_list = SG_min_93);
#     SG_max_DoYs_series_93 = pd.Series(SG_max_93[0]);
#     SG_max_series_93 = pd.Series(SG_max_93[1]);
#     SG_min_DoYs_series_93 = pd.Series(SG_min_93[0]);
#     SG_min_series_93 = pd.Series(SG_min_93[1]);

#     ########################################################################################################
#     ########################################################################################################

#     plotting_dic = { "SG 31" : [SG_pred_31, SG_max_DoYs_series_31, SG_max_series_31],
#                      "SG 32" : [SG_pred_32, SG_max_DoYs_series_32, SG_max_series_32],

#                      "SG 51" : [SG_pred_51, SG_max_DoYs_series_51, SG_max_series_51],
#                      "SG 52" : [SG_pred_52, SG_max_DoYs_series_52, SG_max_series_52],
#                      "SG 53" : [SG_pred_53, SG_max_DoYs_series_53, SG_max_series_53],

#                      "SG 71" : [SG_pred_71, SG_max_DoYs_series_71, SG_max_series_71],
#                      "SG 72" : [SG_pred_72, SG_max_DoYs_series_72, SG_max_series_72],
#                      "SG 73" : [SG_pred_73, SG_max_DoYs_series_73, SG_max_series_73],

#                      "SG 91" : [SG_pred_91, SG_max_DoYs_series_91, SG_max_series_91],
#                      "SG 92" : [SG_pred_92, SG_max_DoYs_series_92, SG_max_series_92],
#                      "SG 93" : [SG_pred_93, SG_max_DoYs_series_93, SG_max_series_93]
#                      }

#     #############################################
#     ###
#     ###             plot
#     ###
#     #############################################
#     #
#     #   We have to change this part to make a perfect plot
#     #        
#     plot_title = county + ", " + plant + " (" + ID + "), delta = " + str(deltA)
#     # sb.set();

#     fig, ax = plt.subplots(figsize=(8,6));
#     ax.scatter(date_df.Date, y, label="Raw data", s = 60);

#     for co, ite in enumerate(plotting_dic):
#         lbl = ite + ", Peaks: " + str(len(plotting_dic[ite][2]))
#         # ax.plot(X, plotting_dic[ite][0], label = lbl, c = eleven_colors[co])
#         ax.plot(date_df.Date, plotting_dic[ite][0], label = lbl, c = eleven_colors[co])

#         date_df_specific = date_df[date_df.DoY.isin(plotting_dic[ite][1])]
#                  # plotting_dic[ite][1]
#         ax.scatter(date_df_specific.Date, plotting_dic[ite][2], s=100, marker='*', c = eleven_colors[co]);

#     ax.set_title(plot_title);
#     ax.set(xlabel='DoY', ylabel=idx)

#     ################################################
#     #
#     #    bare soil indices plots
#     #

#     # an_EE_TS_BSI = rc.initial_clean(df = crr_fld, column_to_be_cleaned='BSI')
#     # # an_EE_TS_NDWI = rc.initial_clean(df = crr_fld, column_to_be_cleaned='NDWI')
#     # an_EE_TS_PSRI = rc.initial_clean(df = crr_fld, column_to_be_cleaned='PSRI')
#     # an_EE_TS_LSWI = rc.initial_clean(df = crr_fld, column_to_be_cleaned='LSWI')

#     # ax.plot(an_EE_TS_BSI['doy'], an_EE_TS_BSI['BSI'], label="BSI")
#     # # ax.plot(x_NDWI, y_NDWI, label="NWDI")

#     # ax.plot(an_EE_TS_PSRI['doy'], an_EE_TS_PSRI['PSRI'], label="PSRI")
#     # ax.plot(an_EE_TS_LSWI['doy'], an_EE_TS_LSWI['LSWI'], label="LSWI")

#     ax.legend(loc="best");
#     return (fig)

def plot_TS(an_EE_TS_df, xp_axis='doy', yp_axis='NDVI'):
    year = int(an_EE_TS_df['year'].unique())
    plant = an_EE_TS_df['CropTyp'].unique()[0]
    county = an_EE_TS_df['county'].unique()[0]
    TRS = an_EE_TS_df['TRS'].unique()[0]

    an_EE_TS_df = remote_core.initial_clean(an_EE_TS_df)
    #
    # plot
    #
    plot_title = county + ", " + plant + ", " + str(year) + ", (" + TRS + ") delta = " + str()
    # sb.set();
    
    fig, ax = plt.subplots(figsize=(8,6));
    ax.plot(an_EE_TS_df[xp_axis], an_EE_TS_df[yp_axis], label="NDVI data");
    ax.set_title(plot_title);
    ax.legend(loc="best");

    return(TS_plot)

def subplots_savitzky(current_field, idx, deltA):

    eleven_colors = ["gray", "lightcoral", "red", "peru",
                     "darkorange", "gold", "olive", "green",
                     "blue", "violet", "deepskyblue"]

    plant = current_field['CropTyp'].unique()[0]
    # Take care of names, replace "/" and "," and " " by "_"
    plant = plant.replace("/", "_")
    plant = plant.replace(",", "_")
    plant = plant.replace(" ", "_")
    plant = plant.replace("__", "_")
    
    county = current_field['county'].unique()[0]
    ID = current_field['ID'].unique()[0]

    X = current_field['doy']
    y = current_field[idx]

    #############################################
    ###
    ###             Smoothen
    ###
    #############################################
    # differences are minor, but lets keep using Pythons function
    # my_savitzky_pred = rc.savitzky_golay(y, window_size=Sav_win_size, order=sav_order)

    SG_pred_31 = scipy.signal.savgol_filter(y, window_length= 3, polyorder=1)
    SG_pred_32 = scipy.signal.savgol_filter(y, window_length= 3, polyorder=2)
    
    SG_pred_51 = scipy.signal.savgol_filter(y, window_length= 5, polyorder=1)
    SG_pred_52 = scipy.signal.savgol_filter(y, window_length= 5, polyorder=2)
    SG_pred_53 = scipy.signal.savgol_filter(y, window_length= 5, polyorder=3)
    
    SG_pred_71 = scipy.signal.savgol_filter(y, window_length= 7, polyorder=1)
    SG_pred_72 = scipy.signal.savgol_filter(y, window_length= 7, polyorder=2)
    SG_pred_73 = scipy.signal.savgol_filter(y, window_length= 7, polyorder=3)

    SG_pred_91 = scipy.signal.savgol_filter(y, window_length= 9, polyorder=1)
    SG_pred_92 = scipy.signal.savgol_filter(y, window_length= 9, polyorder=2)
    SG_pred_93 = scipy.signal.savgol_filter(y, window_length= 9, polyorder=3)

   
    # preds_df = pd.DataFrame(data = {'SG 31':SG_pred_31, 'SG 32':SG_pred_32, 
    #                                 'SG 51':SG_pred_51, 'SG 52':SG_pred_52, 'SG 53':SG_pred_53, 
    #                                 'SG 71':SG_pred_71, 'SG 72':SG_pred_72, 'SG 73':SG_pred_73, 
    #                                 'SG 91':SG_pred_91, 'SG 92':SG_pred_92, 'SG 93':SG_pred_93}, 
    #                         index = X)
    # df2.set_index(keys = ['a'], drop=True,  inplace=True)
    #############################################
    ###
    ###             find peaks
    ###
    #############################################

    SG_max_min_31 = rc.my_peakdetect(y_axis=SG_pred_31, x_axis=X, delta=deltA);
    SG_max_31 =  SG_max_min_31[0]; SG_min_31 =  SG_max_min_31[1];
    SG_max_31 = rc.separate_x_and_y(m_list = SG_max_31);
    SG_min_31 = rc.separate_x_and_y(m_list = SG_min_31);
    SG_max_DoYs_series_31 = pd.Series(SG_max_31[0]);
    SG_max_series_31 = pd.Series(SG_max_31[1]);
    SG_min_DoYs_series_31 = pd.Series(SG_min_31[0]);
    SG_min_series_31 = pd.Series(SG_min_31[1]);


    SG_max_min_32 = rc.my_peakdetect(y_axis=SG_pred_32, x_axis=X, delta=deltA);
    SG_max_32 =  SG_max_min_32[0]; SG_min_32 =  SG_max_min_32[1];
    SG_max_32 = rc.separate_x_and_y(m_list = SG_max_32);
    SG_min_32 = rc.separate_x_and_y(m_list = SG_min_32);
    SG_max_DoYs_series_32 = pd.Series(SG_max_32[0]);
    SG_max_series_32 = pd.Series(SG_max_32[1]);
    SG_min_DoYs_series_32 = pd.Series(SG_min_32[0]);
    SG_min_series_32 = pd.Series(SG_min_32[1]);

    ############
    ############ window 5
    ############

    SG_max_min_51 = rc.my_peakdetect(y_axis=SG_pred_51, x_axis=X, delta=deltA);
    SG_max_51 =  SG_max_min_51[0]; SG_min_51 =  SG_max_min_51[1];
    SG_max_51 = rc.separate_x_and_y(m_list = SG_max_51);
    SG_min_51 = rc.separate_x_and_y(m_list = SG_min_51);
    SG_max_DoYs_series_51 = pd.Series(SG_max_51[0]);
    SG_max_series_51 = pd.Series(SG_max_51[1]);
    SG_min_DoYs_series_51 = pd.Series(SG_min_51[0]);
    SG_min_series_51 = pd.Series(SG_min_51[1]);

    SG_max_min_52 = rc.my_peakdetect(y_axis=SG_pred_52, x_axis=X, delta=deltA);
    SG_max_52 =  SG_max_min_52[0]; SG_min_52 =  SG_max_min_52[1];
    SG_max_52 = rc.separate_x_and_y(m_list = SG_max_52);
    SG_min_52 = rc.separate_x_and_y(m_list = SG_min_52);
    SG_max_DoYs_series_52 = pd.Series(SG_max_52[0]);
    SG_max_series_52 = pd.Series(SG_max_52[1]);
    SG_min_DoYs_series_52 = pd.Series(SG_min_52[0]);
    SG_min_series_52 = pd.Series(SG_min_52[1]);

    SG_max_min_53 = rc.my_peakdetect(y_axis=SG_pred_53, x_axis=X, delta=deltA);
    SG_max_53 =  SG_max_min_53[0]; SG_min_53 =  SG_max_min_53[1];
    SG_max_53 = rc.separate_x_and_y(m_list = SG_max_53);
    SG_min_53 = rc.separate_x_and_y(m_list = SG_min_53);
    SG_max_DoYs_series_53 = pd.Series(SG_max_53[0]);
    SG_max_series_53 = pd.Series(SG_max_53[1]);
    SG_min_DoYs_series_53 = pd.Series(SG_min_53[0]);
    SG_min_series_53 = pd.Series(SG_min_53[1]);

    ############
    ############ window 7
    ############

    SG_max_min_71 = rc.my_peakdetect(y_axis=SG_pred_71, x_axis=X, delta=deltA);
    SG_max_71 =  SG_max_min_71[0]; SG_min_71 =  SG_max_min_71[1];
    SG_max_71 = rc.separate_x_and_y(m_list = SG_max_71);
    SG_min_71 = rc.separate_x_and_y(m_list = SG_min_71);
    SG_max_DoYs_series_71 = pd.Series(SG_max_71[0]);
    SG_max_series_71 = pd.Series(SG_max_71[1]);
    SG_min_DoYs_series_71 = pd.Series(SG_min_71[0]);
    SG_min_series_71 = pd.Series(SG_min_71[1]);

    SG_max_min_72 = rc.my_peakdetect(y_axis=SG_pred_72, x_axis=X, delta=deltA);
    SG_max_72 =  SG_max_min_72[0]; SG_min_72 =  SG_max_min_72[1];
    SG_max_72 = rc.separate_x_and_y(m_list = SG_max_72);
    SG_min_72 = rc.separate_x_and_y(m_list = SG_min_72);
    SG_max_DoYs_series_72 = pd.Series(SG_max_72[0]);
    SG_max_series_72 = pd.Series(SG_max_72[1]);
    SG_min_DoYs_series_72 = pd.Series(SG_min_72[0]);
    SG_min_series_72 = pd.Series(SG_min_72[1]);

    SG_max_min_73 = rc.my_peakdetect(y_axis=SG_pred_73, x_axis=X, delta=deltA);
    SG_max_73 =  SG_max_min_73[0]; SG_min_73 =  SG_max_min_73[1];
    SG_max_73 = rc.separate_x_and_y(m_list = SG_max_73);
    SG_min_73 = rc.separate_x_and_y(m_list = SG_min_73);
    SG_max_DoYs_series_73 = pd.Series(SG_max_73[0]);
    SG_max_series_73 = pd.Series(SG_max_73[1]);
    SG_min_DoYs_series_73 = pd.Series(SG_min_73[0]);
    SG_min_series_73 = pd.Series(SG_min_73[1]);

    ############
    ############ window 9
    ############

    SG_max_min_91 = rc.my_peakdetect(y_axis=SG_pred_91, x_axis=X, delta=deltA);
    SG_max_91 =  SG_max_min_91[0]; SG_min_91 =  SG_max_min_91[1];
    SG_max_91 = rc.separate_x_and_y(m_list = SG_max_91);
    SG_min_91 = rc.separate_x_and_y(m_list = SG_min_91);
    SG_max_DoYs_series_91 = pd.Series(SG_max_91[0]);
    SG_max_series_91 = pd.Series(SG_max_91[1]);
    SG_min_DoYs_series_91 = pd.Series(SG_min_91[0]);
    SG_min_series_91 = pd.Series(SG_min_91[1]);

    SG_max_min_92 = rc.my_peakdetect(y_axis=SG_pred_92, x_axis=X, delta=deltA);
    SG_max_92 =  SG_max_min_92[0]; SG_min_92 =  SG_max_min_92[1];
    SG_max_92 = rc.separate_x_and_y(m_list = SG_max_92);
    SG_min_92 = rc.separate_x_and_y(m_list = SG_min_92);
    SG_max_DoYs_series_92 = pd.Series(SG_max_92[0]);
    SG_max_series_92 = pd.Series(SG_max_92[1]);
    SG_min_DoYs_series_92 = pd.Series(SG_min_92[0]);
    SG_min_series_92 = pd.Series(SG_min_92[1]);

    SG_max_min_93 = rc.my_peakdetect(y_axis=SG_pred_93, x_axis=X, delta=deltA);
    SG_max_93 =  SG_max_min_93[0]; SG_min_93 =  SG_max_min_93[1];
    SG_max_93 = rc.separate_x_and_y(m_list = SG_max_93);
    SG_min_93 = rc.separate_x_and_y(m_list = SG_min_93);
    SG_max_DoYs_series_93 = pd.Series(SG_max_93[0]);
    SG_max_series_93 = pd.Series(SG_max_93[1]);
    SG_min_DoYs_series_93 = pd.Series(SG_min_93[0]);
    SG_min_series_93 = pd.Series(SG_min_93[1]);

    ########################################################################################################
    ########################################################################################################

    plotting_dic = { "SG 31" : [SG_pred_31, SG_max_DoYs_series_31, SG_max_series_31],
                     "SG 32" : [SG_pred_32, SG_max_DoYs_series_32, SG_max_series_32],

                     "SG 51" : [SG_pred_51, SG_max_DoYs_series_51, SG_max_series_51],
                     "SG 52" : [SG_pred_52, SG_max_DoYs_series_52, SG_max_series_52],
                     "SG 53" : [SG_pred_53, SG_max_DoYs_series_53, SG_max_series_53],

                     "SG 71" : [SG_pred_71, SG_max_DoYs_series_71, SG_max_series_71],
                     "SG 72" : [SG_pred_72, SG_max_DoYs_series_72, SG_max_series_72],
                     "SG 73" : [SG_pred_73, SG_max_DoYs_series_73, SG_max_series_73],

                     "SG 91" : [SG_pred_91, SG_max_DoYs_series_91, SG_max_series_91],
                     "SG 92" : [SG_pred_92, SG_max_DoYs_series_92, SG_max_series_92],
                     "SG 93" : [SG_pred_93, SG_max_DoYs_series_93, SG_max_series_93]
    }

    #############################################
    ###
    ###             plot
    ###
    #############################################

    # sub_out = plant + "/" # "/plant_based_plots/" + plant + "/"
    # plot_path = plot_dir_base + sub_out
    # plot_path = plot_path   # +  str(len(SG_max_DoYs_series)) + "_peaks/"
    # os.makedirs(plot_path, exist_ok=True)
    # # print ("plot_path is " + plot_path)
    # if (len(os.listdir(plot_path))<50):
        
    plot_title = county + ", " + plant + " (" + ID + "), delta = " + str(deltA)
    # sb.set();

    ##################################################################
    fig, ax = plt.subplots(figsize=(8,6));
    ax.grid(True)

    ax.scatter(X, y, label="Raw data", s=30);

    for co, ite in enumerate(plotting_dic):
        lbl = ite + ", Peaks: " + str(len(plotting_dic[ite][2]))
        ax.plot(X, plotting_dic[ite][0], label = lbl, c = eleven_colors[co])
        ax.scatter(plotting_dic[ite][1], plotting_dic[ite][2], s=100, marker='*', c = eleven_colors[co]);

    ax.set_title(plot_title);
    ax.set(xlabel='DoY', ylabel=idx)

    ################################################
    #
    #    bare soil indices plots
    #

    # an_EE_TS_BSI = rc.initial_clean(df = current_field, column_to_be_cleaned='BSI')
    # # an_EE_TS_NDWI = rc.initial_clean(df = current_field, column_to_be_cleaned='NDWI')
    # an_EE_TS_PSRI = rc.initial_clean(df = current_field, column_to_be_cleaned='PSRI')
    # an_EE_TS_LSWI = rc.initial_clean(df = current_field, column_to_be_cleaned='LSWI')

    # ax.plot(an_EE_TS_BSI['doy'], an_EE_TS_BSI['BSI'], label="BSI")
    # # ax.plot(x_NDWI, y_NDWI, label="NWDI")

    # ax.plot(an_EE_TS_PSRI['doy'], an_EE_TS_PSRI['PSRI'], label="PSRI")
    # ax.plot(an_EE_TS_LSWI['doy'], an_EE_TS_LSWI['LSWI'], label="LSWI")

    ax.legend(loc="best");
    return (fig)
