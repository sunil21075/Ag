Namak Script

#PBS -N varTheta3
#PBS -l nodes=1:bigmem:ppn=6,walltime=99:00:00
#PBS -l mem=32gb
#PBS -e error1.txt
#PBS -o output1.txt
#PBS -M mhn.namak@gmail.com
#PBS -m abe

module load jdk/1.8.0_51
cd /data/wudb/users/mhn/gqrbe/jars/varyingTheta/dbpFun/p3/

Rscript. R