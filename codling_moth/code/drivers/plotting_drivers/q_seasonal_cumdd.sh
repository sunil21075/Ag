#!/bin/bash
#v#!/usr/bin/env Rscript

#PBS -N seasonal_45
#PBS -l nodes=1:ppn=1,walltime=99:00:00
#PBS -l mem=40gb
#PBS -q hydro
#PBS -e seasonal_45_E.txt
#PBS -o seasonal_45_o.txt
#PBS -m abe
cd $PBS_O_WORKDIR

# Ensure a clean running environment:
module purge

# Load modules (if needed)
module load R/R-3.2.2_gcc

./d_seasonal_cumdd.R rcp45

exit 0
