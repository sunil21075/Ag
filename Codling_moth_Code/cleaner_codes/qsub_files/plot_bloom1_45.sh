#!/bin/bash
#v#!/usr/bin/env Rscript

#PBS -N plot_bloom1_45
#PBS -l nodes=1:ppn=1,walltime=99:00:00
#PBS -l mem=40gb
#PBS -q hydro
#PBS -e plot_bloom1_45_E.txt
#PBS -o plot_bloom1_45_O.txt
#PBS -m abe
cd $PBS_O_WORKDIR

# Ensure a clean running environment:
module purge

# Load modules (if needed)
module load R/R-3.2.2_gcc

/home/hnoorazar/cleaner_codes/drivers/plot_bloom1_driver.R rcp45

exit 0
