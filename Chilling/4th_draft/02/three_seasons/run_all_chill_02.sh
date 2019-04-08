#!/bin/bash

cd /home/hnoorazar/chilling_codes/current_draft/02/three_seasons/qsubs/
for runname in q_model_dyn_mid_nov q_model_dyn_mid_oct q_model_dyn_mid_sept q_model_dyn_nov q_model_dyn_oct q_model_dyn_sept q_obs_dyn_mid_nov q_obs_dyn_mid_oct q_obs_dyn_mid_sept q_obs_dyn_nov q_obs_dyn_oct q_obs_dyn_sept
do
qsub ./$runname.sh
done







