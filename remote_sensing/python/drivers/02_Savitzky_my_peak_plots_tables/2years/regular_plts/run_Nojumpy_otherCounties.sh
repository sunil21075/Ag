#!/bin/bash

cd /home/hnoorazar/remote_sensing_codes/02_Savitzky_my_peak/00_peak_tables_and_plots_Aug26/01_2Yrs_regular_plots/qsubs/
for runname in {3..40}
do
qsub ./q_$runname.sh
done
