#!/bin/bash
#v#!/usr/bin/env Rscript

#PBS -N CNRM-CM5
#PBS -l nodes=1:ppn=6,walltime=99:00:00
#PBS -l mem=10gb
#PBS -q hydro
#PBS -e error_CNRM-CM5.txt
#PBS -o output_CNRM-CM5.txt
#PBS -M h.noorazar@yahoo.com
#PBS -m abe
cd $PBS_O_WORKDIR

# Ensure a clean running environment:
module purge

# Load modules (if needed)
module load R/R-3.2.2_gcc

/home/hnoorazar/cleaner_codes/drivers/CNRM-CM5_driver.R CNRM-CM5

exit 0