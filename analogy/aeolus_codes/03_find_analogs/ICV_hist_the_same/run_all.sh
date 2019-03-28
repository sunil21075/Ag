#!/bin/bash

cd /home/hnoorazar/analog_codes/03_find_analogs/rcp45_qsubs/

for runname in bcc-csm1-1-m BNU-ESM CanESM2 CNRM-CM5 GFDL-ESM2G GFDL-ESM2M
do
qsub ./q_rcp45_$runname.sh
done


cd /home/hnoorazar/analog_codes/03_find_analogs/rcp85_qsubs/

for runname in bcc-csm1-1-m BNU-ESM CanESM2 CNRM-CM5 GFDL-ESM2G GFDL-ESM2M
do
qsub ./q_rcp85_$runname.sh
done
