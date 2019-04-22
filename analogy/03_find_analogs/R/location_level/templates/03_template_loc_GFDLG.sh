#!/bin/bash

#PBS -V

## Define a job name
#PBS -N precip_type_gen_3_type_GFDLG_emission_type_int_file

#PBS -l nodes=1:ppn=1,walltime=99:00:00
#PBS -l mem=30gb
#PBS -q hydro

#PBS -k o
  ##PBS -j oe
#PBS -e /home/hnoorazar/analog_codes/03_find_analogs/fine/location_level/precip_type_gen_3_type_GFDLG_emission_type_int_file.e
#PBS -o /home/hnoorazar/analog_codes/03_find_analogs/fine/location_level/precip_type_gen_3_type_GFDLG_emission_type_int_file.o
#PBS -m abe

echo
echo We are in the $PWD directory
echo

module purge

# Load R
module load udunits/2.2.20
module load libxml2/2.9.4
module load gdal/2.1.2_gcc proj/4.9.2
module load gcc/7.3.0 r/3.5.1/gcc/7.3.0

Rscript --vanilla /home/hnoorazar/analog_codes/03_find_analogs/location_level/d_analog_location.R precip_type gen_3_type GFDL-ESM2G emission_type int_file

echo
echo "----- DONE -----"
echo

exit 0


