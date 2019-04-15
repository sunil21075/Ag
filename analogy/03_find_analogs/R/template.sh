#!/bin/bash

<<<<<<< HEAD
#PBS -V

## Define a job name
#PBS -N precip_type_gen_3_type_model_type_emission_type

#PBS -l nodes=1:ppn=1,walltime=02:00:00
#PBS -l mem=8gb
#PBS -q hydro

#PBS -k o
  ##PBS -j oe
#PBS -e /home/hnoorazar/analog_codes/03_find_analogs/error/precip_type_gen_3_type_model_type_emission_type.e
#PBS -o /home/hnoorazar/analog_codes/03_find_analogs/error/precip_type_gen_3_type_model_type_emission_type.o
s
=======
## Export all environment variables in the qsub command's environment to the
## batch job.
#PBS -V

## Define a job name
#PBS -N model_type_emission_type

## Define compute options
#PBS -l nodes=1:dev:ppn=1
#PBS -l mem=8gb
#PBS -l walltime=00:45:00
#PBS -q hydro

## Define path for output & error logs
#PBS -k o
  ##PBS -j oe
#PBS -e /home/hnoorazar/analog_codes/03_find_analogs/error/model_type_emission_type.e
#PBS -o /home/hnoorazar/analog_codes/03_find_analogs/error/model_type_emission_type.o

## Define path for reporting
>>>>>>> a255bd425a6f23bebc1f80714626251bfb7c2646
#PBS -m abe

echo
echo We are in the $PWD directory
echo

# First we ensure a clean running environment:
module purge

# Load R
module load udunits/2.2.20
module load libxml2/2.9.4
module load gdal/2.1.2_gcc proj/4.9.2
module load gcc/7.3.0 r/3.5.1/gcc/7.3.0

<<<<<<< HEAD
Rscript --vanilla /home/hnoorazar/analog_codes/03_find_analogs/d_analog.R precip_type gen_3_type model_type emission_type
=======
Rscript --vanilla /home/hnoorazar/analog_codes/03_find_analogs/d_analog.R model_type emission_type
>>>>>>> a255bd425a6f23bebc1f80714626251bfb7c2646

echo
echo "----- DONE -----"
echo

exit 0
