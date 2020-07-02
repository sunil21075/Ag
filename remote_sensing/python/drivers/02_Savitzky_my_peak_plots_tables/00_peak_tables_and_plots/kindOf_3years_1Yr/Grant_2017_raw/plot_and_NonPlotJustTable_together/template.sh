#!/bin/bash

# ----------------------------------------------------------------
# Configure PBS options
# ----------------------------------------------------------------
## Define a job name
#PBS -N outer

## Define compute options
#PBS -l nodes=1:ppn=1
#PBS -l mem=30gb
#PBS -l walltime=06:00:00
#PBS -q batch

## Define path for output & error logs
#PBS -k o

#PBS -e /home/hnoorazar/remote_sensing_codes/02_Savitzky_my_peak/00_peak_tables_and_plots/Grant_BothIrr_2018_Savitzky/error/outer_E
#PBS -o /home/hnoorazar/remote_sensing_codes/02_Savitzky_my_peak/00_peak_tables_and_plots/Grant_BothIrr_2018_Savitzky/error/outer_O

## Define path for reporting
##PBS -M h.noorazar@yahoo.com
#PBS -m abe

# ----------------------------------------------------------------
# Start the script itself
# ----------------------------------------------------------------
module purge
module load gcc/7.3.0
module load python/3.7.1/gcc/7.3.0

cd /home/hnoorazar/remote_sensing_codes/02_Savitzky_my_peak/00_peak_tables_and_plots/Grant_BothIrr_2018_Savitzky

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

python3 ./d_Grant_2018_Savitzky_bothIrr.py Sav_win_size sav_order delt do_plot indeks






