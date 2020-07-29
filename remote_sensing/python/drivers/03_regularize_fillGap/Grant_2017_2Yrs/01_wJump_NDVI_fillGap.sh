#!/bin/bash

# ----------------------------------------------------------------
# Configure PBS options
# ----------------------------------------------------------------
## Define a job name
#PBS -N NDVI_Gap_filling_2Yrs_w_jump

## Define compute options
#PBS -l nodes=1:ppn=1
#PBS -l mem=60gb
#PBS -l walltime=06:00:00
#PBS -q batch

## Define path for output & error logs
#PBS -k o

#PBS -e /home/hnoorazar/remote_sensing_codes/03_regularize_fillGap/01_regularize_2Yrs/error/NDVI_GapFill_w_jump_E
#PBS -o /home/hnoorazar/remote_sensing_codes/03_regularize_fillGap/01_regularize_2Yrs/error/NDVI_GapFill_w_jump_O

## Define path for reporting
##PBS -M h.noorazar@yahoo.com
#PBS -m abe

# ----------------------------------------------------------------
# Start the script itself
# ----------------------------------------------------------------
module purge
module load gcc/7.3.0
module load python/3.7.1/gcc/7.3.0

cd /home/hnoorazar/remote_sensing_codes/03_regularize_fillGap/01_regularize_2Yrs

# ----------------------------------------------------------------
# Gathering useful information
# ----------------------------------------------------------------
echo "--------- environment ---------"
env | grep PBS

echo "--------- where am i  ---------"
echo WORKDIR: ${PBS_O_WORKDIR}
echo HOMEDIR: ${PBS_O_HOME}

echo Running time on host `hostname`
echo Time is `date`
echo Directory is `pwd`

echo "--------- continue on ---------"

# ----------------------------------------------------------------
# Run python code for matrix
# ----------------------------------------------------------------

python3 ./01_2Yrs_Grant_2017_fillGaps.py NDVI w_jump






