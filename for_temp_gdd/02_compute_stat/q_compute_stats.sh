#!/bin/bash

## Export all environment variables in the qsub command's environment to the
## batch job.
#PBS -V

## Define a job name
#PBS -N compute_stats_H

## Define compute options
#PBS -l nodes=1:dev:ppn=1
#PBS -l mem=10gb
#PBS -l walltime=20:00:00
#PBS -q hydro

#PBS -k o
  ##PBS -j oe
#PBS -e /data/hydro/users/Hossein/temp_gdd/error/compute_stats_H_E.txt
#PBS -o /data/hydro/users/Hossein/temp_gdd/error/compute_stats_H_O.txt

#PBS -m abe
cd $PBS_O_WORKDIR

echo
echo We are now in $PWD.
echo

module purge

### ulimit -s 16384 # enlarge stack limit to 16 megs
### R --slave -e 'Cstack_info()["size"]'

module load udunits/2.2.20
module load libxml2/2.9.4
module load gdal/2.1.2_gcc proj/4.9.2
module load gcc/7.3.0 r/3.5.1/gcc/7.3.0

# Rscript --vanilla /data/hydro/users/Hossein/temp_gdd/d_merge_separately.R historical
Rscript --vanilla ./d_compute_stats.R rcp45
echo
echo "----- DONE -----"
echo

exit 0
