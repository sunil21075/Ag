#!/bin/bash

cd /home/hnoorazar/remote_sensing_codes/03_regularize_fillGap/01_regularize/qsubs/
for runname in {97..144}
do
qsub ./q_$runname.sh
done
