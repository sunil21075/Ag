#!/bin/bash
#v#!/usr/bin/env Rscript

#PBS -N bcc-45
#PBS -l nodes=1:ppn=1,walltime=30:00:00
#PBS -l mem=10gb
#PBS -q hydro
#PBS -e bcc-45-error.txt
#PBS -o bcc-45-output.txt
#PBS -M h.noorazar@yahoo.com
#PBS -m abe
cd $PBS_O_WORKDIR

# Ensure a clean running environment:
module purge

# Load modules (if needed)
module load R/R-3.2.2_gcc

/home/hnoorazar/cleaner_codes/drivers/local_future/local_future_driver_45.R bcc-csm1-1-m

exit 0
