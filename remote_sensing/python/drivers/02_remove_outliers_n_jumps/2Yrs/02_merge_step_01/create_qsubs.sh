#!/bin/bash

cd /home/hnoorazar/remote_sensing_codes/02_remove_outliers_n_jumps/2Yrs/02_merge_step_01

outer=1

for cloud_type in 70_cloud # 30_cloud 30_cloud_max 10_cloud 
do
	for SF_year in 2016 2017 2018
	do
	  for indeks in EVI NDVI
	  do
	    cp template.sh ./qsubs/q_$outer.sh
	    sed -i s/outer/"$outer"/g           ./qsubs/q_$outer.sh
	    sed -i s/cloud_type/"$cloud_type"/g ./qsubs/q_$outer.sh
	    sed -i s/indeks/"$indeks"/g         ./qsubs/q_$outer.sh
	    sed -i s/SF_year/"$SF_year"/g       ./qsubs/q_$outer.sh
	    let "outer+=1" 
	  done  
	done
done