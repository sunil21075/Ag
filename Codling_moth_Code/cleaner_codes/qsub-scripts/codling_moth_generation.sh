#!/bin/bash
#v#!/usr/bin/env Rscript
# job name
PBS -N RCodMoth-CM_POP_hist
# set the requested resources
PBS -l mem=20480mb
PBS -l nodes=1
PBS -l walltime=72:00:00
# Request email on (a)bort, (b)eginning, and (e)nd.
#PBS -m abe

# email when finished
PBS -M h.noorazar@yahoo.com

cd $PBS_O_WORKDIR

# First we ensure a clean running environment:
module purge

# Load modules (if needed)
module load R/R-3.2.2_gcc

#./CodlingMoth.R  /home/kiran/histmetdata/vic_inputdata0625_pnw_combined_05142008/ /data/kiran/CodlingMoth/hist/
#./testR.R
/home/hnoorazar/cleaner_codes/drivers/CodlingMothGenerations_H.R

exit 0

# 
# this is first .sh script written for testing and running CodlingMothGenerations_H.R and see how it goes
#