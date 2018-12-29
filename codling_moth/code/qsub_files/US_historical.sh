#!/bin/bash
#v#!/usr/bin/env Rscript
# job name

#PBS -N Historical
#PBS -l nodes=1:ppn=1,walltime=99:00:00
#PBS -l mem=10gb
#PBS -q hydro
#PBS -e E_historical_e.txt
#PBS -o historical_o.txt
#PBS -m abe
cd $PBS_O_WORKDIR

# First we ensure a clean running environment:
module purge

# Load modules (if needed)
module load R/R-3.2.2_gcc


/home/hnoorazar/cleaner_codes/drivers/CodlingMothGenerations_H.R historical

exit 0
