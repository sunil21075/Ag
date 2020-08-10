#!/bin/bash

cd /home/hnoorazar/remote_sensing_codes/03_remove_outliers_n_jumps/2Yrs/00_remove_outliers/qsubs/
for runname in {201..380}
do
qsub ./q_$runname.sh
done
