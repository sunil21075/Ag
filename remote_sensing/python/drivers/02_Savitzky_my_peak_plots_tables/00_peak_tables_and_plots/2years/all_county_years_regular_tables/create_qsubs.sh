#!/bin/bash
cd /home/hnoorazar/remote_sensing_codes/02_Savitzky_my_peak/00_Eastern_tables_and_plots/01_2Yrs_regular_Grant_2017/tables

outer=1

for county in for county in Grant Whitman Asotin Garfield Ferry Franklin Columbia Adams Benton Chelan Douglas Kittitas Klickitat Lincoln Okanogan Spokane Stevens Yakima 'Pend_Oreille' 'Walla_Walla'
do
  for SF_year in 2016 2017 2018
  do
    for indeks in EVI
    do
      for SG_params in 51 53 73 93
      do  
        for delt in .4
        do
          cp template.sh ./qsubs/q_$outer.sh
          sed -i s/outer/"$outer"/g      ./qsubs/q_$outer.sh
          sed -i s/county/"$county"/g    ./qsubs/q_$outer.sh
          sed -i s/SF_year/"$SF_year"/g  ./qsubs/q_$outer.sh
          sed -i s/indeks/"$indeks"/g    ./qsubs/q_$outer.sh
          sed -i s/SG_params/"$SG_params"/g  ./qsubs/q_$outer.sh
          sed -i s/delt/"$delt"/g      ./qsubs/q_$outer.sh
          let "outer+=1" 
        done
      done  
    done
  done
done