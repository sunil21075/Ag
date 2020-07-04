#!/bin/bash
<<<<<<< HEAD
cd /home/hnoorazar/remote_sensing_codes/02_Savitzky_my_peak/00_Eastern_tables_and_plots/01_1Yr_regular_Grant_2017/plots/
=======
<<<<<<<< HEAD:remote_sensing/python/drivers/02_Savitzky_my_peak_plots_tables/00_peak_tables_and_plots/2years/Grant_2017_regularized_plots/create_qsubs.sh
cd /home/hnoorazar/remote_sensing_codes/02_Savitzky_my_peak/00_Eastern_tables_and_plots/01_2Yrs_regular_Grant_2017/plots
========
cd /home/hnoorazar/remote_sensing_codes/02_Savitzky_my_peak/00_Eastern_tables_and_plots/01_1Yr_regular_Grant_2017/plots/
>>>>>>>> 86825e55a353b137541d6f22b7ef1b6e1e47e0d2:remote_sensing/python/drivers/02_Savitzky_my_peak_plots_tables/00_peak_tables_and_plots/ZZ_Not_Needed_kindOf_3years_1Yr/Grant_2017_regularized/Plots/create_qsubs.sh
>>>>>>> 86825e55a353b137541d6f22b7ef1b6e1e47e0d2

outer=1
for indeks in EVI NDVI
do
  for irrigated_only in 0 1
  do
    for SF_year in 2017
    do
      cp template.sh ./qsubs/q_$outer.sh
      sed -i s/outer/"$outer"/g          ./qsubs/q_$outer.sh
      sed -i s/indeks/"$indeks"/g          ./qsubs/q_$outer.sh
      sed -i s/irrigated_only/"$irrigated_only"/g   ./qsubs/q_$outer.sh
      sed -i s/SF_year/"$SF_year"/g  ./qsubs/q_$outer.sh
      let "outer+=1" 
    done
  done  
done