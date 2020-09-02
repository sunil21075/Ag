#!/bin/bash

cd /home/hnoorazar/remote_sensing_codes/05_confusionTables/00_create_tables/qsubs/
for runname in {1..60}
do
qsub ./q_$runname.sh
done
