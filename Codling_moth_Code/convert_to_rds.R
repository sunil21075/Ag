convert_to_rds(file_dir, file_name, write_dir){
	file_name = paste0(file_dir, file_name)
	data <- read.table(filename, header = TRUE, sep = ",")
	saveRDS(data, paste0(write_path, file_name, ".rds"))
}