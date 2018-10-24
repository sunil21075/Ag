#!/share/apps/R-3.2.2_gcc/bin/Rscript
library(data.table)
library(reshape2)
library(dplyr)
library(foreach)
library(iterators)
library(chron)

input_dir = "/data/hydro/users/Hossein/codling_moth/local/processed/discovery/Girids/"
write_dir = "/data/hydro/users/Hossein/codling_moth/local/processed/discovery/samples/"

files = list.files(input_dir)

for (file in files){
	file_name = paste0(input_dir, file)
	data <- data.table(readRDS(file_name))
	dimension = dim(data)
	column_names = colnames(data)
	write.table(data.frame(dimension, column_names), paste0(write_dir, file, "metadata" , ".txt"), sep="\t")
}