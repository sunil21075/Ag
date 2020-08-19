#!/bin/bash
cd /home/hnoorazar/remote_sensing_codes/04_regularize_fillGap/01_regularize_2Yrs/

outer=1

for jumps in noJumps
do
  for cloud_type in 70_cloud # 30_cloud 30_cloud_max 10_cloud 
  do
    for SF_year in 2016 2017 2018
    do
      for indeks in EVI NDVI
      do
        for county in Grant Whitman Asotin Garfield Ferry Franklin Columbia Adams Benton Chelan Douglas Kittitas Klickitat Lincoln Okanogan Spokane Stevens Yakima 'Pend_Oreille' 'Walla_Walla'
        do
          cp 00_regular_template.sh           ./qsubs/q_$outer.sh
          sed -i s/outer/"$outer"/g           ./qsubs/q_$outer.sh
          sed -i s/cloud_type/"$cloud_type"/g ./qsubs/q_$outer.sh
          sed -i s/indeks/"$indeks"/g         ./qsubs/q_$outer.sh
          sed -i s/jumps/"$jumps"/g           ./qsubs/q_$outer.sh
          sed -i s/SF_year/"$SF_year"/g       ./qsubs/q_$outer.sh
          sed -i s/county/"$county"/g         ./qsubs/q_$outer.sh
          let "outer+=1" 
        done
      done  
    done
  done
done


