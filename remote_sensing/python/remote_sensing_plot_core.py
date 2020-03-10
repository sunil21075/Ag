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
import remote_sensing_core as remote_core

################################################################
#####
#####                   Function definitions
#####
################################################################
def plot_TS(an_EE_TS_df, xp_axis='doy', yp_axis='NDVI'):
    year = int(an_EE_TS_df['year'].unique())
    plant = an_EE_TS_df['CropTyp'].unique()[0]
    county = an_EE_TS_df['county'].unique()[0]
    TRS = an_EE_TS_df['TRS'].unique()[0]

    an_EE_TS_df = remote_core.initial_clean(an_EE_TS_df)
    #
    # plot
    #
    plot_title = county + ", " + plant + ", " + str(year) + ", (" + TRS + ")"
    sb.set();
    fig, ax = plt.subplots(figsize=(8,6));
    ax.plot(an_EE_TS_df[xp_axis], an_EE_TS_df[yp_axis], label="NDVI data");
    ax.set_title(plot_title);
    ax.legend(loc="best");

    return(TS_plot)



