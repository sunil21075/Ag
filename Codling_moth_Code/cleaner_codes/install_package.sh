#!/bin/bash
#v#!/usr/bin/env Rscript

#PBS -N diapause_85
#PBS -l nodes=1:ppn=1,walltime=99:00:00
#PBS -l mem=40gb
#PBS -q hydro
#PBS -e diapause_85_error.txt
#PBS -o diapause_85_output.txt
#PBS -m abe
cd $PBS_O_WORKDIR

# Ensure a clean running environment:
module purge

# Load modules (if needed)
module load R/R-3.2.2_gcc

/home/hnoorazar/cleaner_codes/install_package.R

exit 0