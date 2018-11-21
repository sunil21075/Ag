#!/bin/bash
#v#!/usr/bin/env Rscript

#PBS -N vertdd_85_2015
#PBS -l nodes=1:ppn=1,walltime=70:00:00
#PBS -l mem=20gb
#PBS -q hydro
#PBS -e vertdd_85_2015_e.txt
#PBS -o vertdd_85_2015_o.txt
#PBS -m abe
cd $PBS_O_WORKDIR

# Ensure a clean running environment:
module purge

# Load modules (if needed)
module load R/R-3.2.2_gcc

/home/hnoorazar/cleaner_codes/drivers/vertdd_driver_2015.R combined_CMPOP_rcp85

exit 0
