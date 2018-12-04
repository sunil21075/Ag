#!/bin/bash
#v#!/usr/bin/env Rscript
#PBS -N diap_45_shift_5
#PBS -l nodes=1:ppn=1,walltime=70:00:00
#PBS -l mem=30gb
#PBS -q hydro
#PBS -e diap_45_shift_5_e.txt
#PBS -o diap_45_shift_5_o.txt
#PBS -m abe
cd $PBS_O_WORKDIR
# Ensure a clean running environment:
module purge
# Load modules (if needed)
module load R/R-3.2.2_gcc

./diapause_shift_flat.R rcp45 5

exit 0
