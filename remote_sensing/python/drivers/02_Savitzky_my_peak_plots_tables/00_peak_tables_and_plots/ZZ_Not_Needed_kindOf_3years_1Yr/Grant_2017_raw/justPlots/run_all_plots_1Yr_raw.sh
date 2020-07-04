#!/bin/bash
<<<<<<< HEAD
   
cd /home/hnoorazar/remote_sensing_codes/02_Savitzky_my_peak/00_Eastern_tables_and_plots/00_1Yr_raw_Grant_2017/plots/qsubs/
=======
<<<<<<<< HEAD:remote_sensing/python/drivers/02_Savitzky_my_peak_plots_tables/00_peak_tables_and_plots/2years/Grant_2017_regularized_plots/run_all_plots.sh

cd /home/hnoorazar/remote_sensing_codes/02_Savitzky_my_peak/00_Eastern_tables_and_plots/01_2Yrs_regular_Grant_2017/plots/qsubs/
========
   
cd /home/hnoorazar/remote_sensing_codes/02_Savitzky_my_peak/00_Eastern_tables_and_plots/00_1Yr_raw_Grant_2017/plots/qsubs/
>>>>>>>> 86825e55a353b137541d6f22b7ef1b6e1e47e0d2:remote_sensing/python/drivers/02_Savitzky_my_peak_plots_tables/00_peak_tables_and_plots/ZZ_Not_Needed_kindOf_3years_1Yr/Grant_2017_raw/justPlots/run_all_plots_1Yr_raw.sh
>>>>>>> 86825e55a353b137541d6f22b7ef1b6e1e47e0d2
for runname in {1..4}
do
qsub ./q_$runname.sh
done
