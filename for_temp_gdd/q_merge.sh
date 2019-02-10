#!/bin/bash
#v#!/usr/bin/env Rscript

#PBS -N merge
#PBS -l nodes=1:ppn=1,walltime=70:00:00
#PBS -l mem=20gb
#PBS -q hydro
#PBS -e /data/hydro/users/Hossein/temp_gdd/error/merge_E.txt
#PBS -o /data/hydro/users/Hossein/temp_gdd/error/merge_O.txt
#PBS -m abe
cd $PBS_O_WORKDIR

# Ensure a clean running environment:
module purge

# Load R
module load udunits/2.2.20
module load libxml2/2.9.4
module load gdal/2.1.2_gcc proj/4.9.2
module load gcc/7.3.0 r/3.5.1/gcc/7.3.0

/data/hydro/users/Hossein/temp_gdd/d_merge.R

exit 0
