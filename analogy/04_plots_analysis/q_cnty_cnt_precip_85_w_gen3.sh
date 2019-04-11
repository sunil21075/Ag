#!/bin/bash

## Export all environment variables in the qsub command's environment to the
## batch job.
#PBS -V

## Define a job name
#PBS -N cnty_cnt_85_precip_gen3

## Define compute options
#PBS -l nodes=1:ppn=1,walltime=99:00:00
#PBS -l mem=10gb
#PBS -q hydro

## Define path for output & error logs
#PBS -k o
  ##PBS -j oe
#PBS -e /home/hnoorazar/analog_codes/04_analysis/error/cnty_cnt_85_precip_gen3_E
#PBS -o /home/hnoorazar/analog_codes/04_analysis/error/cnty_cnt_85_precip_gen3_O

## Define path for reporting
#PBS -m abe

echo
echo We are now in $PWD.
echo

# First we ensure a clean running environment:
module purge

# Load R
module load udunits/2.2.20
module load libxml2/2.9.4
module load gdal/2.1.2_gcc proj/4.9.2
module load gcc/7.3.0 r/3.5.1/gcc/7.3.0
module load gcc/7.3.0
module load r/3.5.1/gcc/7.3.0
module load r/3.5.1

Rscript --vanilla /home/hnoorazar/analog_codes/04_analysis/d_county_count.R rcp85 precip 2 w_gen3

echo
echo "----- DONE -----"
echo

exit 0
