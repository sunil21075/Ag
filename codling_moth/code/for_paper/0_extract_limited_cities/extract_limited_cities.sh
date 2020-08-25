#!/bin/bash

## Export all environment variables in the qsub command's environment to the
## batch job.
#PBS -V

## Define a job name
#PBS -N extract_CTs_4Paper

## Define compute options
#PBS -l nodes=1:ppn=1
#PBS -l mem=80gb
#PBS -l walltime=10:00:00
#PBS -q hydro

## Define path for output & error logs
#PBS -k o
  ##PBS -j oe
#PBS -e /home/hnoorazar/cleaner_codes/for_paper/0_extract_limited_cities/error/extract_limited_CTs_E
#PBS -o /home/hnoorazar/cleaner_codes/for_paper/0_extract_limited_cities/error/extract_limited_CTs_O

## Define path for reporting
#PBS -m abe

echo
echo We are in the $PWD directory
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

cd /home/hnoorazar/cleaner_codes/for_paper/0_extract_limited_cities/

Rscript --vanilla extract_limited_cities.R "dynamic" chill_sea

echo
echo "----- DONE -----"
echo

exit 0
