#!/bin/bash

cd /home/hnoorazar/remote_sensing_codes/02_Savitzky_my_peak/00_peak_tables_and_plots_Aug26/01_2Yrs_regular_Grant_2017/plots/qsubs/
for runname in {1..2}
do
qsub ./q_$runname.sh
done
