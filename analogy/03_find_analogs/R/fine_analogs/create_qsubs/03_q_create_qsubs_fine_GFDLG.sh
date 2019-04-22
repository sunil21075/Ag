#!/bin/bash

#PBS -l nodes=1:ppn=1,walltime=10:00:00
#PBS -l mem=2gb
#PBS -q hydro

cd /home/hnoorazar/analog_codes/03_find_analogs/fine
###########
########### RCP 45
###########
########### w_precip, w_gen3

cat /home/hnoorazar/analog_codes/parameters/file_names | while read LINE ; do
cp 03_template_fine_GFDLG.sh ./rcp45_qsubs/GFDLG/q_rcp45_w_precip_w_gen3_$LINE.sh
sed -i s/precip_type/w_precip/g ./rcp45_qsubs/GFDLG/q_rcp45_w_precip_w_gen3_$LINE.sh
sed -i s/gen_3_type/w_gen3/g ./rcp45_qsubs/GFDLG/q_rcp45_w_precip_w_gen3_$LINE.sh
sed -i s/emission_type/rcp45/g ./rcp45_qsubs/GFDLG/q_rcp45_w_precip_w_gen3_$LINE.sh
sed -i s/int_file/"$LINE"/g ./rcp45_qsubs/GFDLG/q_rcp45_w_precip_w_gen3_$LINE.sh
done

########### no_precip, w_gen3

cat /home/hnoorazar/analog_codes/parameters/file_names | while read LINE ; do
cp 03_template_fine_GFDLG.sh ./rcp45_qsubs/GFDLG/q_rcp45_no_precip_w_gen3_$LINE.sh
sed -i s/precip_type/no_precip/g ./rcp45_qsubs/GFDLG/q_rcp45_no_precip_w_gen3_$LINE.sh
sed -i s/gen_3_type/w_gen3/g ./rcp45_qsubs/GFDLG/q_rcp45_no_precip_w_gen3_$LINE.sh
sed -i s/emission_type/rcp45/g ./rcp45_qsubs/GFDLG/q_rcp45_no_precip_w_gen3_$LINE.sh
sed -i s/int_file/"$LINE"/g ./rcp45_qsubs/GFDLG/q_rcp45_no_precip_w_gen3_$LINE.sh
done

########### w_precip, no_gen3

cat /home/hnoorazar/analog_codes/parameters/file_names | while read LINE ; do
cp 03_template_fine_GFDLG.sh ./rcp45_qsubs/GFDLG/q_rcp45_w_precip_no_gen3_$LINE.sh
sed -i s/precip_type/w_precip/g ./rcp45_qsubs/GFDLG/q_rcp45_w_precip_no_gen3_$LINE.sh
sed -i s/gen_3_type/no_gen3/g ./rcp45_qsubs/GFDLG/q_rcp45_w_precip_no_gen3_$LINE.sh
sed -i s/emission_type/rcp45/g ./rcp45_qsubs/GFDLG/q_rcp45_w_precip_no_gen3_$LINE.sh
sed -i s/int_file/"$LINE"/g ./rcp45_qsubs/GFDLG/q_rcp45_w_precip_no_gen3_$LINE.sh
done

########### no_precip, no_gen3

cat /home/hnoorazar/analog_codes/parameters/file_names | while read LINE ; do
cp 03_template_fine_GFDLG.sh ./rcp45_qsubs/GFDLG/q_rcp45_no_precip_no_gen3_$LINE.sh
sed -i s/precip_type/no_precip/g ./rcp45_qsubs/GFDLG/q_rcp45_no_precip_no_gen3_$LINE.sh
sed -i s/gen_3_type/no_gen3/g ./rcp45_qsubs/GFDLG/q_rcp45_no_precip_no_gen3_$LINE.sh
sed -i s/emission_type/rcp45/g ./rcp45_qsubs/GFDLG/q_rcp45_no_precip_no_gen3_$LINE.sh
sed -i s/int_file/"$LINE"/g ./rcp45_qsubs/GFDLG/q_rcp45_no_precip_no_gen3_$LINE.sh
done

###########
########### RCP 85
########### 
########### w_precip, w_gen3

cat /home/hnoorazar/analog_codes/parameters/file_names | while read LINE ; do
cp 03_template_fine_GFDLG.sh ./rcp85_qsubs/GFDLG/q_rcp85_w_precip_w_gen3_$LINE.sh
sed -i s/precip_type/w_precip/g ./rcp85_qsubs/GFDLG/q_rcp85_w_precip_w_gen3_$LINE.sh
sed -i s/gen_3_type/w_gen3/g ./rcp85_qsubs/GFDLG/q_rcp85_w_precip_w_gen3_$LINE.sh
sed -i s/emission_type/rcp85/g ./rcp85_qsubs/GFDLG/q_rcp85_w_precip_w_gen3_$LINE.sh
sed -i s/int_file/"$LINE"/g ./rcp85_qsubs/GFDLG/q_rcp85_w_precip_w_gen3_$LINE.sh
done

########### no_precip, w_gen3

cat /home/hnoorazar/analog_codes/parameters/file_names | while read LINE ; do
cp 03_template_fine_GFDLG.sh ./rcp85_qsubs/GFDLG/q_rcp85_no_precip_w_gen3_$LINE.sh
sed -i s/precip_type/no_precip/g ./rcp85_qsubs/GFDLG/q_rcp85_no_precip_w_gen3_$LINE.sh
sed -i s/gen_3_type/w_gen3/g ./rcp85_qsubs/GFDLG/q_rcp85_no_precip_w_gen3_$LINE.sh
sed -i s/emission_type/rcp85/g ./rcp85_qsubs/GFDLG/q_rcp85_no_precip_w_gen3_$LINE.sh
sed -i s/int_file/"$LINE"/g ./rcp85_qsubs/GFDLG/q_rcp85_no_precip_w_gen3_$LINE.sh
done

########### w_precip, no_gen3

cat /home/hnoorazar/analog_codes/parameters/file_names | while read LINE ; do
cp 03_template_fine_GFDLG.sh ./rcp85_qsubs/GFDLG/q_rcp85_w_precip_no_gen3_$LINE.sh
sed -i s/precip_type/w_precip/g ./rcp85_qsubs/GFDLG/q_rcp85_w_precip_no_gen3_$LINE.sh
sed -i s/gen_3_type/no_gen3/g ./rcp85_qsubs/GFDLG/q_rcp85_w_precip_no_gen3_$LINE.sh
sed -i s/emission_type/rcp85/g ./rcp85_qsubs/GFDLG/q_rcp85_w_precip_no_gen3_$LINE.sh
sed -i s/int_file/"$LINE"/g ./rcp85_qsubs/GFDLG/q_rcp85_w_precip_no_gen3_$LINE.sh
done

########### no_precip, no_gen3

cat /home/hnoorazar/analog_codes/parameters/file_names | while read LINE ; do
cp 03_template_fine_GFDLG.sh ./rcp85_qsubs/GFDLG/q_rcp85_no_precip_no_gen3_$LINE.sh
sed -i s/precip_type/no_precip/g ./rcp85_qsubs/GFDLG/q_rcp85_no_precip_no_gen3_$LINE.sh
sed -i s/gen_3_type/no_gen3/g ./rcp85_qsubs/GFDLG/q_rcp85_no_precip_no_gen3_$LINE.sh
sed -i s/emission_type/rcp85/g ./rcp85_qsubs/GFDLG/q_rcp85_no_precip_no_gen3_$LINE.sh
sed -i s/int_file/"$LINE"/g ./rcp85_qsubs/GFDLG/q_rcp85_no_precip_no_gen3_$LINE.sh
done



