#!/bin/bash

for runname in bcc BNU Can CNRM GFDLG GFDLM
cd /home/hnoorazar/analog_codes/03_find_analogs/fine/rcp85_qsubs/$runname
do
qsub ./q_rcp85_w_precip_w_gen3_$runname.sh
qsub ./q_rcp85_no_precip_w_gen3_$runname.sh
#qsub ./q_rcp85_no_precip_no_gen3_$runname.sh
#qsub ./q_rcp85_w_precip_no_gen3_$runname.sh
done

for runname in bcc BNU Can CNRM GFDLG GFDLM
cd /home/hnoorazar/analog_codes/03_find_analogs/rcp45_qsubs/$runname
do
qsub ./q_rcp45_w_precip_w_gen3_$runname.sh
qsub ./q_rcp45_no_precip_w_gen3_$runname.sh
#qsub ./q_rcp45_no_precip_no_gen3_$runname.sh
#qsub ./q_rcp45_w_precip_no_gen3_$runname.sh
done


for runname in bcc BNU Can CNRM GFDLG GFDLM
cd /home/hnoorazar/analog_codes/03_find_analogs/rcp85_qsubs/$runname
do
# qsub ./q_rcp85_w_precip_w_gen3_$runname.sh
# qsub ./q_rcp85_no_precip_w_gen3_$runname.sh
qsub ./q_rcp85_no_precip_no_gen3_$runname.sh
qsub ./q_rcp85_w_precip_no_gen3_$runname.sh
done

for runname in bcc BNU Can CNRM GFDLG GFDLM
cd /home/hnoorazar/analog_codes/03_find_analogs/rcp45_qsubs/$runname
do
# qsub ./q_rcp45_w_precip_w_gen3_$runname.sh
# qsub ./q_rcp45_no_precip_w_gen3_$runname.sh
qsub ./q_rcp45_no_precip_no_gen3_$runname.sh
qsub ./q_rcp45_w_precip_no_gen3_$runname.sh
done

