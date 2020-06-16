#!/bin/bash

cd /home/hnoorazar/remote_sensing_codes/02_Savitzky_my_peak/00_peak_tables_and_plots/00_Eastern/Grant_2017/tables/qsubs/
for runname in {1..48}
do
qsub ./q_$runname.sh
done
