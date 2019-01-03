#!/bin/bash
#v#!/usr/bin/env Rscript

#PBS -N LA
#PBS -l nodes=1:ppn=1,walltime=99:00:00
#PBS -l mem=20gb
#PBS -q hydro
#PBS -e E_LA.txt
#PBS -o O_LA.txt
#PBS -m abe
cd $PBS_O_WORKDIR
# Ensure a clean running environment:
module purge
# Load modules (if needed)
module load R/R-3.2.2_gcc 0.25

./d_filter_LA.R

exit 0
