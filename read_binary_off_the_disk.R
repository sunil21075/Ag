
source_path = "/Users/hn/Documents/GitHub/Kirti/read_binary_core/read_binary_core.R"
source(source_path)

main_in <- "/Users/hn/Desktop/Desktop/Kirti/check_point/binary/"
file_name <- "data_43.84375_-113.78125"

observed <- read_binary(paste0(main_in, "observed/file_name"), 
                        hist=T, no_vars=8)

modeled_hist <- read_binary(paste0(main_in, "modeled/historical/", file_name),
                            hist=T, no_vars=4)

modeled_85 <- read_binary(paste0(main_in, "modeled/rcp85/", file_name), 
                          hist=F, no_vars=4)





##########################################################
#############################
#############################            TEST Lagoon codes
#############################
observed <- subset(observed, select=c(year, month, day, precip))
modeled_hist <- subset(modeled_hist, select=c(year, month, day, precip))
modeled_85 <- subset(modeled_85, select=c(year, month, day, precip))









