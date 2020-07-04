#!/bin/bash

# ----------------------------------------------------------------
# Configure PBS options
# ----------------------------------------------------------------
## Define a job name
<<<<<<< HEAD
#PBS -N outer
=======
#PBS -N regularized_2Yrs_plot_outer
>>>>>>> 86825e55a353b137541d6f22b7ef1b6e1e47e0d2

## Define compute options
#PBS -l nodes=1:ppn=1
#PBS -l mem=60gb
#PBS -l walltime=06:00:00
#PBS -q batch

## Define path for output & error logs
#PBS -k o

<<<<<<< HEAD
#PBS -e /home/hnoorazar/remote_sensing_codes/02_Savitzky_my_peak/00_Eastern_tables_and_plots/00_1Yr_raw_Grant_2017/plots/error/outer_E
#PBS -o /home/hnoorazar/remote_sensing_codes/02_Savitzky_my_peak/00_Eastern_tables_and_plots/00_1Yr_raw_Grant_2017/plots/error/outer_O
=======
<<<<<<<< HEAD:remote_sensing/python/drivers/02_Savitzky_my_peak_plots_tables/00_peak_tables_and_plots/2years/Grant_2017_regularized_plots/template.sh
#PBS -e /home/hnoorazar/remote_sensing_codes/02_Savitzky_my_peak/00_Eastern_tables_and_plots/01_2Yrs_regular_Grant_2017/plots/error/outer_E
#PBS -o /home/hnoorazar/remote_sensing_codes/02_Savitzky_my_peak/00_Eastern_tables_and_plots/01_2Yrs_regular_Grant_2017/plots/error/outer_O
========
#PBS -e /home/hnoorazar/remote_sensing_codes/02_Savitzky_my_peak/00_Eastern_tables_and_plots/00_1Yr_raw_Grant_2017/plots/error/outer_E
#PBS -o /home/hnoorazar/remote_sensing_codes/02_Savitzky_my_peak/00_Eastern_tables_and_plots/00_1Yr_raw_Grant_2017/plots/error/outer_O
>>>>>>>> 86825e55a353b137541d6f22b7ef1b6e1e47e0d2:remote_sensing/python/drivers/02_Savitzky_my_peak_plots_tables/00_peak_tables_and_plots/ZZ_Not_Needed_kindOf_3years_1Yr/Grant_2017_raw/justPlots/template.sh
>>>>>>> 86825e55a353b137541d6f22b7ef1b6e1e47e0d2

## Define path for reporting
##PBS -M h.noorazar@yahoo.com
#PBS -m abe

# ----------------------------------------------------------------
# Start the script itself
# ----------------------------------------------------------------
module purge
module load gcc/7.3.0
module load python/3.7.1/gcc/7.3.0

<<<<<<< HEAD
cd /home/hnoorazar/remote_sensing_codes/02_Savitzky_my_peak/00_Eastern_tables_and_plots/00_1Yr_raw_Grant_2017/plots/
=======
<<<<<<<< HEAD:remote_sensing/python/drivers/02_Savitzky_my_peak_plots_tables/00_peak_tables_and_plots/2years/Grant_2017_regularized_plots/template.sh
cd /home/hnoorazar/remote_sensing_codes/02_Savitzky_my_peak/00_Eastern_tables_and_plots/01_2Yrs_regular_Grant_2017/plots
========
cd /home/hnoorazar/remote_sensing_codes/02_Savitzky_my_peak/00_Eastern_tables_and_plots/00_1Yr_raw_Grant_2017/plots/
>>>>>>>> 86825e55a353b137541d6f22b7ef1b6e1e47e0d2:remote_sensing/python/drivers/02_Savitzky_my_peak_plots_tables/00_peak_tables_and_plots/ZZ_Not_Needed_kindOf_3years_1Yr/Grant_2017_raw/justPlots/template.sh
>>>>>>> 86825e55a353b137541d6f22b7ef1b6e1e47e0d2
   

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

<<<<<<< HEAD
python3 ./d_1Yr_raw_Grant_2017_SG_pots.py indeks irrigated_only SF_year
=======
<<<<<<<< HEAD:remote_sensing/python/drivers/02_Savitzky_my_peak_plots_tables/00_peak_tables_and_plots/2years/Grant_2017_regularized_plots/template.sh
python3 ./d_2Yrs_regularized_Grant_SG_plots.py indeks irrigated_only SF_year
========
python3 ./d_1Yr_raw_Grant_2017_SG_pots.py indeks irrigated_only SF_year
>>>>>>>> 86825e55a353b137541d6f22b7ef1b6e1e47e0d2:remote_sensing/python/drivers/02_Savitzky_my_peak_plots_tables/00_peak_tables_and_plots/ZZ_Not_Needed_kindOf_3years_1Yr/Grant_2017_raw/justPlots/template.sh
>>>>>>> 86825e55a353b137541d6f22b7ef1b6e1e47e0d2






