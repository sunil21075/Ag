#!/bin/bash
#v#!/usr/bin/env Rscript
# job name

#PBS -N Historical
#PBS -l nodes=1:ppn=1,walltime=99:00:00
#PBS -l mem=10gb
#PBS -q hydro
#PBS -e local_historical_error.txt
#PBS -o local_historical_output.txt
#PBS -M h.noorazar@yahoo.com
#PBS -m abe
cd $PBS_O_WORKDIR

# First we ensure a clean running environment:
module purge

# Load modules (if needed)
module load R/R-3.2.2_gcc


/home/hnoorazar/cleaner_codes/drivers/local_historical/CodlingMothGenerations_CM.R historical

exit 0
