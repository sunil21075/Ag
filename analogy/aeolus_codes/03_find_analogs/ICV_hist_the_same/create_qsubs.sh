#!/bin/bash

cd /home/hnoorazar/analog_codes/03_find_analogs/

mkdir rcp45_qsubs
for runname in bcc-csm1-1-m BNU-ESM CanESM2 CNRM-CM5 GFDL-ESM2G GFDL-ESM2M
do
cp template.sh ./rcp45_qsubs/q_rcp45_$runname.sh
sed -i s/emission_type/rcp45/g ./rcp45_qsubs/q_rcp45_$runname.sh
sed -i s/model_type/"$runname"/g ./rcp45_qsubs/q_rcp45_$runname.sh
done

mkdir rcp85_qsubs
for runname in bcc-csm1-1-m BNU-ESM CanESM2 CNRM-CM5 GFDL-ESM2G GFDL-ESM2M
do
cp template.sh ./rcp85_qsubs/q_rcp85_$runname.sh
sed -i s/emission_type/rcp85/g ./rcp85_qsubs/q_rcp85_$runname.sh
sed -i s/model_type/"$runname"/g ./rcp85_qsubs/q_rcp85_$runname.sh
done