
options(digits=9)
options(digit=9)
####################################################################
##                                                                ##
##                                                                ##
##                            Analysis                            ##
##                                                                ##
##                                                                ##
####################################################################
no_locs_in_a_county_and_our_f_model <- function(fips_dt, target_fip){
  # input: fips_dt: a data table containing county of a given location
  #                 which includes columns: fips, location, lat, long
  #                 We have to use a file that on;y contains the locations
  #                 used in our data.
  #        target_fips: a given county fips
  # output: number of locations/grids in a given county
  counts <- fips_dt %>% filter(fips == target_fip) %>% summarise(count = n_distinct(location))
  return(counts[1, 1])
}


no_locs_in_a_county_and_our_hist_model <- function(fips_dt, target_fip){
  # input: fips_dt: a data table containing county of a given location
  #                 which includes columns: fips, location, lat, long
  #                 We have to use a file that on;y contains the locations
  #                 used in our data.
  #        target_fips: a given county fips
  # output: number of locations/grids in a given county
  counts <- fips_dt %>% filter(fips == target_fip) %>% summarise(count = n_distinct(location))
  return(counts[1, 1])
}
