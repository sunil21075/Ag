#!/bin/bash
#v#!/usr/bin/env Rscript
# job name
#PBS -N sens_20_historical
#PBS -l nodes=1:ppn=1,walltime=70:00:00
#PBS -l mem=5gb
#PBS -q hydro
#PBS -e sens_20_historical_e.txt
#PBS -o sens_20_historical_o.txt
#PBS -M h.noorazar@yahoo.com
#PBS -m abe
cd $PBS_O_WORKDIR
# First we ensure a clean running environment:
module purge
# Load modules (if needed)
module load R/R-3.2.2_gcc
./scale_sensitivity.R GFDL-ESM2M 20
exit 0

### categories = c("bcc-csm1-1-m", "BNU-ESM", "CanESM2", "CNRM-CM5", "GFDL-ESM2G", "GFDL-ESM2M", "historical")