#!/bin/bash
#v#!/usr/bin/env Rscript

#PBS -N GFDL-ESM2G
#PBS -l nodes=1:ppn=6,walltime=99:00:00
#PBS -l mem=10gb
#PBS -q hydro
#PBS -e error_GFDL-ESM2G.txt
#PBS -o output_GFDL-ESM2G.txt
#PBS -M h.noorazar@yahoo.com
#PBS -m abe
cd $PBS_O_WORKDIR

# Ensure a clean running environment:
module purge

# Load modules (if needed)
module load R/R-3.2.2_gcc

/home/hnoorazar/cleaner_codes/drivers/all_us_future_45.R GFDL-ESM2G

exit 0
