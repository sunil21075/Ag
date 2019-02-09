#!/bin/bash

## Export all environment variables in the qsub command's environment to the
## batch job.
#PBS -V

## Define a job name
#PBS -N chill-model_hist-observed

## Define compute options
#PBS -l nodes=1:dev:ppn=1
#PBS -l mem=2gb
#PBS -l walltime=00:45:00
#PBS -q batch

## Define path for output & error logs
#PBS -k o
  ##PBS -j oe
#PBS -e /fastscratch/mbrousil/chill-model_hist-observed.e
#PBS -o /fastscratch/mbrousil/chill-model_hist-observed.o

## Define path for reporting
#PBS -M matthew.brousil@wsu.edu
#PBS -m abe


echo
echo We are in the $PWD directory
echo

cd /data/hydro/jennylabcommon2/metdata/historical/UI_historical/VIC_Binary_CONUS_to_2016

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


Rscript --vanilla ~/scripts/chill-model_historical-observed.R


echo
echo "----- DONE -----"
echo

exit 0
