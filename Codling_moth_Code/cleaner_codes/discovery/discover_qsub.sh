#!/bin/bash
#v#!/usr/bin/env Rscript
#PBS -N discover
#PBS -l nodes=1:ppn=1,walltime=11:59:00
#PBS -l mem=10gb
#PBS -q hydro
#PBS -e discover_error.txt
#PBS -o discover_output.txt
#PBS -M h.noorazar@yahoo.com
#PBS -m abe
cd $PBS_O_WORKDIR

# Ensure a clean running environment:
module purge

# Load modules (if needed)
module load R/R-3.2.2_gcc

/data/hydro/users/Hossein/codling_moth/local/processed/discovery/samples/take_samples.R

exit 0
