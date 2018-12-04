merge_data <- function(input_dir, param_dir, locations_file_name, file_prefix="CMPOP", version="rcp45"){
  data = data.table()
  conn = file(paste0(param_dir, locations_file_name), open = "r")
  locations = readLines(conn)
  close(conn)

  for( category in categories) {
      for( location in locations) {
        if(category != "historical") {
          filename <- paste0(read_data_dir, "future_", file_prefix, "/", category, "/", version, "/", file_prefix, "_", location)
        }
        else {
          filename <- paste0(read_data_dir, "historical_", file_prefix, "/", file_prefix, "_" ,location)
            }
    data <- rbind(data, read.table(filename, header = TRUE, sep = ","))
      }
  }
  return(data)
}
