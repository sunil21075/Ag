#!/bin/bash

cd /home/hnoorazar/chilling_codes/current_draft/02/three_seasons/
for runname in q_model_mid_nov_dyn_nonover q_model_mid_oct_dyn_nonover q_model_mid_sept_dyn_nonover q_model_nov_dyn_nonover q_model_oct_dyn_nonover q_obs_mid_nov_dynamic q_obs_mid_oct_dynamic q_obs_mid_sept_dynamic  q_obs_nov_dyn q_obs_oct_dynamic
do
qsub ./$runname.sh
done
