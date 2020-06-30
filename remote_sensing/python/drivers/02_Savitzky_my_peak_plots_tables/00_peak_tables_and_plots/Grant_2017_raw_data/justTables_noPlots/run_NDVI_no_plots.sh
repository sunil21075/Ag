#!/bin/bash

cd /home/hnoorazar/remote_sensing_codes/02_Savitzky_my_peak/00_Eastern_tables_and_plots/00_raw_Grant_2017/tables/qsubs/
for runname in {97..144}
do
qsub ./q_$runname.sh
done
