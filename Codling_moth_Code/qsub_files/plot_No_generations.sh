#!/bin/bash
#v#!/usr/bin/env Rscript

#PBS -N plot_no_gen
#PBS -l nodes=1:ppn=1,walltime=12:00:00
#PBS -l mem=20gb
#PBS -q hydro
#PBS -e plot_no_gen_e.txt
#PBS -o plot_no_gen_o.txt
#PBS -m abe
cd $PBS_O_WORKDIR

# Ensure a clean running environment:
module purge

# Load modules (if needed)
module load R/R-3.4.3_gcc

/home/hnoorazar/cleaner_codes/drivers/plot_generations.R

exit 0
