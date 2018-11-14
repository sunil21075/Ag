

compare <- function(input_1_dir, input_1_name, input_2_dir, input_2_name, output_dir){
	f1 = readRDS(paste0(input_1_dir, input_1_name))
	f2 = readRDS(paste0(input_2_dir, input_2_name))
	x = grep(x = f1 == f2, pattern = "FALSE")
	write.table(data.frame(x), paste0(output_dir, input_1_name, "_", input_2_name, "comparison" , ".txt"), sep="\t")
}