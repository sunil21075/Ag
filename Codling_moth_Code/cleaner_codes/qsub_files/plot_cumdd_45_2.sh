#!/bin/bash
#v#!/usr/bin/env Rscript

#PBS -N plot_cumdd_45_2
#PBS -l nodes=1:ppn=1,walltime=99:00:00
#PBS -l mem=40gb
#PBS -q hydro
#PBS -e plot_cumdd_45_2_E.txt
#PBS -o plot_cumdd_45_2_O.txt
#PBS -m abe
cd $PBS_O_WORKDIR

# Ensure a clean running environment:
module purge

# Load modules (if needed)
module load R/R-3.2.2_gcc

/home/hnoorazar/cleaner_codes/drivers/plot_cumdd_driver.R rcp45 2

exit 0
