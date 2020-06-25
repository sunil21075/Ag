#!/bin/bash
cd /home/hnoorazar/remote_sensing_codes/03_regularize_fillGap/01_regularize/

outer=1
for indeks in EVI NDVI
do
  cp template.sh ./qsubs/q_$outer.sh
  sed -i s/outer/"$outer"/g    ./qsubs/q_$outer.sh
  sed -i s/indeks/"$indeks"/g  ./qsubs/q_$outer.sh
  let "outer+=1"
done