####################################################################
##                                                                ##
##            Read the data off Jenny's data                      ##
##            and get the percipitation.                          ##
##                                                                ##
####################################################################
##    The modeled historical is in 
##               /data/hydro/jennylabcommon2/metdata/maca_v2_vic_binary/
##    modeled historical is equivalent to 
##    having 4 variables, and years 1950-2005
##
##    The observed historical is in 
##    /data/hydro/jennylabcommon2/metdata/historical/UI_historical/VIC_Binary_CONUS_to_2016
##    observed historical is equivalent to 
##    having 8 variables, and years 1979-2016
##
options(digits=9)
options(digit=9)
####################################################################
##                                                                ##
##                                                                ##
##                            Analysis                            ##
##                                                                ##
##                                                                ##
####################################################################
filter_pure_analogs <- function(analog_dt){
  #     SANITY Check
  # find most similar analog county
  # filter the data that are analog 
  # i.e. (exclude the row with "no_analog" in analog_NNs_county column)

  analog_dt$analog_NNs_county[is.na(analog_dt$analog_NNs_county)] <- "no_analog"
  analog_dt <- analog_dt %>% filter(analog_NNs_county != "no_analog")
  analog_dt$analog_NNs_county <- as.integer(analog_dt$analog_NNs_county)
  return(analog_dt)
}

standardize_by_all_pairs <- function(analog_dt, 
                                     f_fips_dt, h_fips_dt, 
                                     f_years, h_years){
  # inputs: analog_dt. It contains 4 columns: 
  #                    "query_county", "analog_NNs_county", "analog_freq", "model"
  #                     a) query_county: will be future_target_fips input of 
  #                                   all_inter_cnty_pair_count(.)
  #                     b) analog_NNs_county: will be the column we iterate through
  #                        to count number of grids in each fips.
  #                     c) analog_freq: the column to standerdize.
  #         f_fips_dt: data table containing locations, fips, st_county
  #         h_fips_dt: data table containing locations, fips, st_county
  #
  analog_dt <- filter_pure_analogs(analog_dt)
  f_tgt_fips <- unique(analog_dt$query_county)
  h_fips_vec <- analog_dt$analog_NNs_county

  for (h_tgt_fip in h_fips_vec){
    denom <- all_inter_cnty_pair_count(future_fips_dt = f_fips_dt, 
                                       hist_fips_dt = h_fips_dt, 
                                       future_target_fips = f_tgt_fips, 
                                       hist_target_fips = h_tgt_fip,
                                       f_yrs=f_years, h_yrs=h_years)

    numer <- analog_dt$analog_freq[analog_dt$analog_NNs_county == h_tgt_fip]
    analog_dt$analog_freq[analog_dt$analog_NNs_county == h_tgt_fip] = numer/denom
  }
  hist_target_row <- analog_dt[which.max(analog_dt$analog_freq),]
  hist_target_fip <- hist_target_row$analog_NNs_county
  analog_dt <- data.table(analog_dt)
  analog_dt <- analog_dt[order(-analog_freq), ]
  return(list(data.table(analog_dt), hist_target_fip))
}

produce_dt_for_map <- function(b_dt){
  data(county.fips)        # Load the county.fips dataset for plotting
  ct <- map_data("county") # Load the county data from the maps package
  cnty3 <- ct %>%
           mutate(polyname = paste(region, subregion, sep=",")) %>%
           left_join(county.fips, by="polyname")

 DT <- left_join(b_dt, cnty3, by=c("analog_NNs_county" = "fips"))
 return(DT)
}

produce_dt_for_pie_Q4 <- function(analog_dt, tgt_fip, f_fips, h_fips, f_years, h_years=37){


  analog_dt <- analog_dt %>% filter(query_county == tgt_fip)

  analog_dt$analog_NNs_county[is.na(analog_dt$analog_NNs_county)] <- "no_analog"
  
  # find most similar analog county
  # filter the data that are analog 
  # i.e. (exclude the row with "no_analog" in analog_NNs_county column)
  just_analogs <- analog_dt %>% filter(analog_NNs_county != "no_analog")
  hist_target_row <- just_analogs[which.max(just_analogs$analog_freq),]
  hist_target_fip <- hist_target_row$analog_NNs_county
  inter_county_analog_count <- hist_target_row$analog_freq
  
  # count number of inter-county (grind, year) pairs ((grid_f_i, f_y_1), (grid_h_i, f_h_1))
  all_possible_analog_cnt <- all_inter_cnty_pair_count(future_fips_dt = f_fips, 
                                                       hist_fips_dt = h_fips, 
                                                       future_target_fip = tgt_fip,
                                                       hist_target_fips = hist_target_fip, 
                                                       f_yrs = f_years, h_yrs = h_years)

  analog_dt <- data.table(analog_dt)
  inter_county_analog_count_complement <- all_possible_analog_cnt - inter_county_analog_count

  vvv <- c("inter county analog count", "inter county pairwise count")
  DT = data.table(category = vvv,
                  counts = c(inter_county_analog_count, inter_county_analog_count_complement),
                  fraction= c((inter_county_analog_count/all_possible_analog_cnt), 
                              (inter_county_analog_count_complement/all_possible_analog_cnt)))

  DT = DT[order(DT$fraction), ]
 
  DT$category <- factor(DT$category, order=T, levels=vvv)

  DT$ymax = cumsum(DT$fraction)
  DT$ymin = c(0, head(DT$ymax, n=-1))

  return (DT)
}

produce_dt_for_pie_Q3 <- function(analog_dt, novel_dt, tgt_fip){
  analog_dt <- analog_dt %>% filter(query_county == tgt_fip)
  novel_dt <- novel_dt %>% filter(query_county == tgt_fip)

  analog_dt$analog_NNs_county[is.na(analog_dt$analog_NNs_county)] <- "no_analog"
  novel_dt$novel_NNs_county[is.na(novel_dt$novel_NNs_county)] <- "not_novel"

  analog_dt <- data.table(analog_dt)
  novel_dt <- data.table(novel_dt)

  novel_dt <- novel_dt[novel_dt$novel_NNs_county != "not_novel"]
  novel_cnt <- sum(novel_dt$novel_freq)

  if (tgt_fip %in% analog_dt$analog_NNs_county){
    self_similarity_count <- analog_dt[analog_dt$analog_NNs_county==tgt_fip, 'analog_freq']$analog_freq
    } else {
     self_similarity_count <- 0
  }
  no_analog_cnt <- analog_dt[analog_dt$analog_NNs_county=="no_analog", 'analog_freq' ]$analog_freq
  non_self_simil_cnt <- sum(analog_dt$analog_freq) - no_analog_cnt - self_similarity_count

  denom <- self_similarity_count + non_self_simil_cnt
  vvv <- c("self-similarity", "non self-similarity")
  DT = data.table(category = vvv,
                  counts = c(self_similarity_count, non_self_simil_cnt),
                  fraction= c((self_similarity_count/denom), (non_self_simil_cnt/denom)))

  DT = DT[order(DT$fraction), ]
  DT$category <- factor(DT$category, order=T, levels=vvv)
  DT$ymax = cumsum(DT$fraction)
  DT$ymin = c(0, head(DT$ymax, n=-1))

  return (DT)
}

produce_dt_for_pie_Q2 <- function(analog_dt, novel_dt, tgt_fip, f_fips, h_fips){
  analog_dt <- analog_dt %>% filter(query_county == tgt_fip)
  novel_dt <- novel_dt %>% filter(query_county == tgt_fip)

  analog_dt$analog_NNs_county[is.na(analog_dt$analog_NNs_county)] <- "no_analog"
  novel_dt$novel_NNs_county[is.na(novel_dt$novel_NNs_county)] <- "not_novel"

  analog_dt <- data.table(analog_dt)
  novel_dt <- data.table(novel_dt)

  novel_dt <- novel_dt[novel_dt$novel_NNs_county != "not_novel"]
  novel_cnt <- sum(novel_dt$novel_freq)

  if (tgt_fip %in% analog_dt$analog_NNs_county){
    self_similarity_count <- analog_dt[analog_dt$analog_NNs_county==tgt_fip, 'analog_freq']$analog_freq
    } else {
     self_similarity_count <- 0
  }

  no_analog_cnt <- analog_dt[analog_dt$analog_NNs_county=="no_analog", 'analog_freq' ]$analog_freq
  non_self_simil_cnt <- sum(analog_dt$analog_freq) - no_analog_cnt - self_similarity_count

  almost_novel_cnt <-  no_analog_cnt - novel_cnt

  analog_count <- self_similarity_count + non_self_simil_cnt
  total_comparisons <- no_analog_cnt + analog_count

  vvv <- c("analog count", "no-analog count")
  DT = data.table(category = vvv,
                  counts = c(analog_count, no_analog_cnt),
                  fraction= c((analog_count/total_comparisons), (no_analog_cnt/total_comparisons)))

  DT = DT[order(DT$fraction), ]
 
  DT$category <- factor(DT$category, order=T, levels=vvv)

  DT$ymax = cumsum(DT$fraction)
  DT$ymin = c(0, head(DT$ymax, n=-1))

  return (DT)
}

produce_dt_for_pie_all_possible <- function(analog_dt, novel_dt, tgt_fip, f_fips, h_fips, f_years, h_years=37){

  all_possible_ss_cnt <- all_possible_ss(f_fips, h_fips, tgt_fip, f_yrs=f_years, h_yrs=h_years)

  analog_dt <- analog_dt %>% filter(query_county == tgt_fip)
  novel_dt <- novel_dt %>% filter(query_county == tgt_fip)

  analog_dt$analog_NNs_county[is.na(analog_dt$analog_NNs_county)] <- "no_analog"
  novel_dt$novel_NNs_county[is.na(novel_dt$novel_NNs_county)] <- "not_novel"

  analog_dt <- data.table(analog_dt)
  novel_dt <- data.table(novel_dt)

  novel_dt <- novel_dt[novel_dt$novel_NNs_county != "not_novel"]
  novel_cnt <- sum(novel_dt$novel_freq)

  if (tgt_fip %in% analog_dt$analog_NNs_county){
    self_similarity_count <- analog_dt[analog_dt$analog_NNs_county==tgt_fip, 'analog_freq']$analog_freq
    } else {
     self_similarity_count <- 0
  }
  not_ss <- all_possible_ss_cnt - self_similarity_count

  vvv <- c("self similarity", "all_ss - self similarity")
  DT = data.table(category = vvv,
                  counts = c(self_similarity_count, not_ss),
                  fraction= c((self_similarity_count/all_possible_ss_cnt), (not_ss/all_possible_ss_cnt)))

  DT = DT[order(DT$fraction), ]
 
  DT$category <- factor(DT$category, order=T, levels=vvv)

  DT$ymax = cumsum(DT$fraction)
  DT$ymin = c(0, head(DT$ymax, n=-1))

  return (DT)
}

all_inter_cnty_pair_count <- function(future_fips_dt, hist_fips_dt, 
                                      future_target_fips, hist_target_fips,
                                      f_yrs, h_yrs){
  
  f_locs <- no_locs_in_a_county(future_fips_dt, future_target_fips)
  h_locs <- no_locs_in_a_county(hist_fips_dt, hist_target_fips)
  return(f_locs * h_locs * f_yrs * h_yrs)
}

all_possible_ss <- function(future_fips_dt, hist_fips_dt, target_fips, f_yrs, h_yrs){
  f_locs <- no_locs_in_a_county(future_fips_dt, target_fips)
  h_locs <- no_locs_in_a_county(hist_fips_dt, target_fips)
  return(f_locs * h_locs * f_yrs * h_yrs)
}

no_locs_in_a_county <- function(fips_dt, target_fip){
  # input: fips_dt: a data table containing county of a given location
  #                 which includes columns: fips, location, lat, long
  #                 We have to use a file that only contains the locations
  #                 used in our data.
  #        target_fips: a given county fips
  # output: number of locations/grids in a given county
  counts <- fips_dt %>% 
            filter(fips == target_fip) %>% 
            summarise(count = n_distinct(location))
  return(counts[1, 1])
}

count_novel_quick <- function(NNs, sigmas, county_list, novel_bd=4){
  # NNs: data table of all nearest neighbors of all locations for all years in a given model
  # county_list: data table of counties' fips and locations (lat_long)
  # sigma_bd:    cut off point of analogs, real number

  # In this function  we attemp to avoid for-loops
  # NNs (sigmas) will be data tables which contain
  # all nearest neighbors of all locations for all years in a given model.
  # So, it will have 286 locations, each for 20 (or whatever) years in future
  # We replace the locations whose dissimilarities with a given query is more than
  # 2-simga with NA in the NNs data table. 
  # (for this matter the historical years in NNs are not imoportant, hence will be droped.)
  #

  # Drop the historical year columns
  NNs <- as.data.frame(NNs) # we need to do this shit! to be able to do the next line!
  NNs <- NNs[, c(1, 2, seq(4, ncol(NNs), 2))]
  # NNs <- data.table(NNs)

  # convert non analog locations to NA. For the following command to
  # work, data has to be in data frame class.
  # these data will be in the size of about 3 gigs, shall we keep a copy untouched?

  # Make a copy, we shall need original data later
  # NNs_cp <- NNs; sigmas_cp <- sigmas; dists_cp <- dists
  
  # set the nearest neighbors whose dissimilarity is less than novel_bd to NA
  # NNs <- as.data.frame(NNs); 
  sigmas <- as.data.frame(sigmas)
  NNs[, -c(1:2)][sigmas[, -c(1:2)] < novel_bd] <- NA

  # replace the fips for coordinates of the nearest neighbors
  NNs[2:ncol(NNs)] <- lapply(NNs[2:ncol(NNs)], function(x) county_list$fips[match(x, county_list$location)])
  # NNs <- as.data.frame(NNs)
  NNs <- within(NNs, remove(year))

  novel_counts <- NNs %>% 
                   gather("key", "NNs", 2:ncol(.)) %>% 
                   group_by(location, NNs) %>% 
                   summarize(novel_freq = n()) %>% 
                   arrange(desc(location), desc(NNs)) %>%
                   data.table()

 setnames(novel_counts, new=c("query_county", "novel_NNs_county"), old=c("location", "NNs"))
 novel_counts$novel_NNs_county[is.na(novel_counts$novel_NNs_county)] <- "not_novel"

 return(novel_counts)
}

count_analogs_counties_quick <- function(NNs, sigmas, county_list, sigma_bd=2){
  # NNs: data table of all nearest neighbors of all locations for all years in a given model
  # county_list: data table of counties' fips and locations (lat_long)
  # sigma_bd:    cut off point of analogs, real number

  # In this function  we attemp to avoid for-loops
  # NNs (sigmas and dists) will be data tables which contain
  # all nearest neighbors of all locations for all years in a given model.
  # So, it will have 286 locations, each for 20 (or whatever) years in future
  # We replace the locations whose dissimilarities with a given query is more than
  # 2-simga with NA in the NNs data table. 
  # (for this matter the historical years in NNs are not imoportant, hence will be droped.)
  #

  # Drop the historical year columns
  NNs <- as.data.frame(NNs) # we need to do this shit! to be able to do the next line!
  NNs <- NNs[, c(1, 2, seq(4, ncol(NNs), 2))]
  print ("line 131 of core")
  # convert non analog locations to NA. For the following command to
  # work, data has to be in data frame class.
  # these data will be in the size of about 3 gigs, shall we keep a copy untouched?

  # Make a copy, we shall need original data later
  # NNs_cp <- NNs; sigmas_cp <- sigmas; dists_cp <- dists
  
  # set the nearest neighbors whose dissimilarity is more than sigma_bd to NA
  # NNs <- as.data.frame(NNs); 
  
  sigmas <- as.data.frame(sigmas); # dists <- as.data.frame(dists)
  NNs[, -c(1:2)][sigmas[, -c(1:2)] > sigma_bd] <- NA
  print ("line 144 of core")
  # replace the fips for coordinates of the nearest neighbors
  NNs[2:ncol(NNs)] <- lapply(NNs[2:ncol(NNs)], function(x) county_list$fips[match(x, county_list$location)])
  # NNs <- as.data.frame(NNs)
  NNs <- within(NNs, remove(year))
  print ("line 149 of core")
  analog_counts <- NNs %>% 
                   gather("key", "NNs", 2:ncol(.)) %>% 
                   group_by(location, NNs) %>% 
                   summarize(analog_freq = n()) %>% 
                   arrange(desc(location), desc(NNs)) %>%
                   data.table()
  print ("line 156 of core")
  setnames(analog_counts, new=c("query_county", "analog_NNs_county"), old=c("location", "NNs"))
  analog_counts$analog_NNs_county[is.na(analog_counts$analog_NNs_county)] <- "no_analog"

 return(analog_counts)
}

count_NNs_per_counties_all_locs <- function(NNs, dists, sigmas, county_list, sigma_bd=2, novel_thresh=4){
  # For a given location, i.e. a vector,
  # find the number of analog (historical)counties 
  # corresponding to a given target county in the future.
  #
  # input: NNs: data frame of nearest neighbors (This is a data table containing all local locations
  #      dists: distances to the locations of interest
  #     sigmas: sigma_dissimilarity between the location of interest and other locations
  #   sigma_bd: the cut-off point to use for NNs. like neighbros with distance less that sigma_bd
  # 
  # output: data tables of county counts
  #

  # set up empty tables to append results to.

  c <- c("query_loc", "query_year", "analog_fips", "Freq", "analog", "st_county", "distance", "sigma")
  all_close_analogs <- setNames(data.table(matrix(nrow = 0, ncol = 8)), c)

  c <- c("query_loc", "query_year", "analog_fips", "Freq")
  all_close_analogs_unique <- setNames(data.table(matrix(nrow = 0, ncol = 4)), c)

  local_sites <- unique(NNs$location)
  future_years <- unique(NNs$year)

  for (site_of_int in local_sites){
    for (year_of_int in future_years){
      NNs_int_yr <- NNs %>% filter(location == site_of_int & year == year_of_int)
      dist_int_yr <- dists %>% filter(location == site_of_int & year == year_of_int)
      sigma_int_yr <- sigmas %>% filter(location == site_of_int & year == year_of_int)
      
      if (check_novelty(sigma_int_yr[,-(1:2)], novel_thresh)) {
        f = county_list %>% filter(location == NNs_int_yr[1, 4])

        v <- data.frame(query_loc = site_of_int, 
                        query_year = year_of_int, 
                        analog_fips=f$fips, Freq = NA, 
                        analog="Novel", st_county = f$st_county, 
                        distance = min(dist_int_yr[,-(1:2)]), 
                        sigma = min(sigma_int_yr[,-(1:2)]))

        all_close_analogs <- rbind(all_close_analogs, v)

        v <- data.frame(query_loc = site_of_int, 
                        query_year = year_of_int, 
                        analog_fips=f$fips, Freq = NA)
        all_close_analogs_unique <- rbind(all_close_analogs_unique, v)
        rm(f)
       } else if (check_almost_novelty(sigma_int_yr[,-(1:2)], sigma_bd, novel_thresh)){
          f = county_list %>% filter(location == NNs_int_yr[1, 4])
          v <- data.frame(query_loc = site_of_int, 
                          query_year = year_of_int, 
                          analog_fips=f$fips, Freq = NA, 
                          analog="Almost_Novel", st_county = f$st_county, 
                          distance = min(dist_int_yr[,-(1:2)]), 
                          sigma=min(sigma_int_yr[,-(1:2)]))

          all_close_analogs <- rbind(all_close_analogs, v)

          v <- data.frame(query_loc = site_of_int, 
                          query_year = year_of_int, 
                          analog_fips=f$fips, Freq = NA)
          all_close_analogs_unique <- rbind(all_close_analogs_unique, v)
          rm(f)
       } else {
        output <- count_NNs_1_per_counties_1_loc(NNs_1 = NNs_int_yr, dists_1 = dist_int_yr, 
                                                 sigmas_1 = sigma_int_yr, county_list, sigma_bd)
        
        all_close_analogs <- rbind(all_close_analogs, output[[1]])
        all_close_analogs_unique <- rbind(all_close_analogs_unique, output[[2]])
      }
    }
  }
  return(list(all_close_analogs, all_close_analogs_unique))
}

count_NNs_1_per_counties_1_loc <- function(NNs_1, dists_1, sigmas_1, county_list, sigma_bd){
  # For a given location, i.e. a vector,
  # find the number of analog (historical)counties 
  # corresponding to a given target county in the future.
  #
  # input: NNs_1: data frame of nearest neighbors (This is a vector in data frame format. it is in  R^N)
  #      dists_1: distances to the location of interest
  #     sigmas_1: sigma_dissimilarity between the location of interest and other locations
  #   sigma_bd: the cut-off point to use for NNs_1. like neighbros with distance less that sigma_bd
  # 
  # output: data frame of county counts
  #

  county_list <- unique(county_list) # sanity check
  year_of_int <- NNs_1$year
  location_of_int <- NNs_1$location

  if (length(year_of_int) > 1){
    print ("You need to check the input of the function --count_NNs_1_per_counties_1_loc--")
    stop ("There are more than 1 year in this data set.")
  }

  if (length(location_of_int) > 1){
    print ("You need to check the input of the function --count_NNs_1_per_counties_1_loc--")
    stop ("There are more than 1 location in this data set.")
  }
  
  # lat_long_of_int <- c(unlist(strsplit(location_of_int, "_"))[1], 
  #                      unlist(strsplit(location_of_int, "_"))[2]) %>% 
  #                      as.numeric()
  
  analogs <- NNs_1[, seq(2, ncol(NNs_1), 2)] %>% data.table()
  
  # remove the location of interest for whom we have found analogs
  # so, the location below, is "location" of one site
  analogs <- within(analogs, remove(location))
  dists_1 <- within(dists_1, remove(year, location))
  sigmas_1 <- within(sigmas_1, remove(year, location))

  analogs <- as.list(as.data.table(t(analogs)))[[1]] # convert to matrix, table, list
  
  # initiale a data frame to use for listing close analogs
  no_of_NNs_1_found <- length(sigmas_1)
  close_analogs <- setNames(data.table(matrix(nrow = no_of_NNs_1_found, ncol = 3)), 
                                              c("location", "distance", "sigma"))
  
  close_analogs$location = analogs
  close_analogs$distance = as.numeric(dists_1)
  close_analogs$sigma = as.numeric(sigmas_1)
  
  ########### add county names to list of NNs_1.
  close_analogs <- merge(x=close_analogs, y=county_list, all.x=T)
  
  ########### Pick up locations that are similar enough to the query
  close_analogs <- close_analogs %>% filter(sigma <= sigma_bd)

  # kill the non-sxisting counties that are present in data as factor!
  close_analogs$st_county <- factor(close_analogs$st_county)

  ########### count number of counties showing up
  NNs_1_county_count <- as.data.frame(table(close_analogs$fips))
  setnames(NNs_1_county_count, old=c("Var1"), new=c("fips"))

  close_analogs <- merge(close_analogs, NNs_1_county_count, all.x=T) %>% data.table()

  ## add the year and location of interest to the data table
  setnames(close_analogs, old=c("fips", "location"), new=c("analog_fips", "analog"))
  close_analogs$query_loc <- location_of_int
  close_analogs$query_year <- year_of_int

  # just grab unique counties and their frequency
  close_analogs_unique <- subset(close_analogs, select=c(analog_fips, Freq, query_loc, query_year)) %>% 
                          data.table()
  setkey(close_analogs_unique, analog_fips)
  close_analogs_unique <- unique(close_analogs_unique)

  # reorder columns of outputs:
  c <- c("query_loc", "query_year", "analog_fips", "Freq", "analog", "st_county", "distance", "sigma")
  setcolorder(close_analogs, c)

  c <- c("query_loc", "query_year", "analog_fips", "Freq")
  setcolorder(close_analogs_unique, c)

  return (list(close_analogs, close_analogs_unique))
}

check_almost_novelty <- function(sigma_df, sigma_bd, novelty_threshold){
  if ((min(sigma_df) < novelty_threshold) & (min(sigma_df) > sigma_bd)) {
    return (as.character(T))
  } else {
    return (as.character(F))
  }
}

check_novelty <- function(sigma_df, novelty_threshold){
  if (min(sigma_df) > novelty_threshold) {
    return (as.character(T))
   } else{
    return (as.character(F))
  }
}

filter_needed_geo_info <- function(data_locs, all_locs){
  ## Lets say we ran the data on some locations
  ## of interest accross US, the data is given in data_locs,
  ## with a column named location (= lat_long)
  ## and all_locs has information about latitude, longitude, VICID.
  ## here we pick up information of all_locs that we need.
  needed_location_info <- all_locs %>% subset(location %in%  data_locs$location) %>% data.table()
  return(needed_location_info)
}

####################################################################################
##                                                                                ##
##                                                                                ##
##                                  find analogs                                  ##
##                                                                                ##
##                                                                                ##
####################################################################################

####################################################################################
#
#                                 Mahony Style
# 
####################################################################################
find_NN_info_biofix <- function(ICV, historical_dt, future_dt, n_neighbors, 
                                numeric_cols, non_numeric_cols){

  historical_dt <- subset(historical_dt, select=c(non_numeric_cols, numeric_cols))
  future_dt <- subset(future_dt, select=c(non_numeric_cols, numeric_cols))
  ICV <- subset(ICV, select=c(non_numeric_cols, numeric_cols))

  all_us_locs <- unique(historical_dt$location)
    
  # 9 local locations are not in the all_us data!!!
  local_locations <- unique(future_dt$location)
  local_locations <- local_locations[which(local_locations %in% all_us_locs)] 
  future_dt <- future_dt %>% filter(location %in% local_locations)
  future_years <- unique(future_dt$year)
  
  trunc.SDs <- 0.1 # Principal component truncation rule

  # initiate empty outputs (to be generated!)
  NN_dist_tb <- data.table()
  NNs_loc_year_tb <- data.table()
  NN_sigma_tb <- data.table()

  A <- as.data.frame(historical_dt)
  B <- as.data.frame(future_dt)
  C <- as.data.frame(ICV)

  rm (historical_dt, future_dt, ICV)

  for (loc in local_locations){
    # loc = local_locations[1]
    Bj <- B %>% filter(location==loc)
    Cj <- C %>% filter(location==loc)

    # standard deviation of 1951-1990 interannual variability in each climate 
    # variable, ignoring missing years
    print ("line 600 of core")
    print (dim(Cj))
    Cj.sd <- apply(Cj[, numeric_cols, drop=F], MARGIN=2, FUN = sd, na.rm = T)
    Cj.sd[Cj.sd<(10^-10)] = 1

    A_prime <- A
    print ("line 606")
    print (dim(A_prime))
    print (class(Cj.sd))
    print (length(Cj.sd))
    A_prime[, numeric_cols] <- sweep(A_prime[, numeric_cols], MARGIN=2, STATS=Cj.sd, FUN = `/`) 
    print ("line 607 of core")
    # standardize the analog pool
    Bj_prime <- Bj
    Bj_prime[, numeric_cols] <-sweep(Bj_prime[, numeric_cols], MARGIN=2, STATS=Cj.sd, FUN = `/`)
    print ("line 611 of core")
    # standardize the reference ICV
    Cj_prime <- Cj
    Cj_prime[, numeric_cols] <-sweep(Cj_prime[, numeric_cols], MARGIN=2, STATS=Cj.sd, FUN = `/`)
    print ("line 615 of core")
    ## Step 2: Extract the principal components (PCs) of 
    ##         the reference period ICV and project all data onto these PCs

    # Principal components analysis. The !is.na(apply(...)) 
    # term is there simply to select all years with complete observations in all variables. 
    #  ZZ[!is.na(apply(ZZ, 1, mean)) ,] selects the rows whose mean is not NaN
    PCA <- prcomp(Cj_prime[, numeric_cols][!is.na(apply(Cj_prime[, numeric_cols], 1, mean)) ,])
    print ("line 623 of core")
    # find the number of PCs to retain using the PC truncation
    # rule of eigenvector stdev > the truncation threshold
    PCs <- max(which(unlist(summary(PCA)[1])>trunc.SDs))
   
    # project the reference ICV onto the PCs which is the same as analog pool:
    X <- as.data.frame(predict(PCA, A_prime))
    X <- cbind(A_prime[, non_numeric_cols], X)

    # project the projected future conditions onto the PCs
    Yj <- as.data.frame(predict(PCA, Bj_prime[, numeric_cols]))
    Yj <- cbind(Bj_prime[, non_numeric_cols], Yj)

    Zj <- as.data.frame(predict(PCA, Cj_prime[, numeric_cols]))
    Zj <- cbind(Cj_prime[, non_numeric_cols], Zj)

    ## Step 3a: express PC scores as standardized
    #           anomalies of reference interannual variability
      
    # standard deviation of 1951-1990 interannual 
    # variability in each principal component, ignoring missing years
    Zj_sd <- apply(Zj[, 4:(PCs + 3), drop=F], MARGIN = 2, FUN = sd, na.rm=T)
    print ("line 645 of core")
    # standardize the analog pool   
    X_prime <- sweep(X[, 4:(PCs + 3)], MARGIN=2, Zj_sd, FUN = `/`)
    X_prime <- cbind(X[, non_numeric_cols], X_prime)
    print ("line 649 of core")
    # standardize the projected conditions
    Yj_prime <- sweep(Yj[, 4:(PCs + 3)], MARGIN=2, Zj_sd, FUN = `/`)
    Yj_prime <- cbind(Yj[, non_numeric_cols], Yj_prime)
    
    NN_list <- get.knnx(data = X_prime[, 4:(PCs+3)], 
                        query= Yj_prime[, 4:(PCs+3)], 
                        k=n_neighbors, algorithm="brute")
    #
    # Step 3: find sigma dissimilarities
    ############################################################
    #
    # This is for one location, and different years. 
    # So, the location column and ClimateScenario can be dropped.
    # We already know what they are from loc and the input file.
    #
    ############################################################
    # percentile of the nearest neighbour distance on the chi distribution with
    # degrees of freedom equaling the dimensionality of the distance measurement (PCs)
    NN_chi <- EnvStats::pchi(as.vector(NN_list$nn.dist), PCs)
    
    # values of the chi percentiles on a standard half-normal distribution
    # (chi distribution with one degree of freedom)
    # NN.sigma[which(proxy==j)] <- qchi(NN.chi, 1)

    NN_sigma <- EnvStats::qchi(NN_chi, 1)

    ########################################################################
    #
    # extract the rows corresponding to nearest neighbors indices
    # 
    ########################################################################
    NN_idx <- NN_list$nn.index
    NN_dist <- NN_list$nn.dist %>% data.table()
    
    NNs_loc_year <- X_prime[as.vector(NN_idx), c('year', 'location')]
    #
    # reshape the long list to wide data table
    # currenty the data are in vector form, by reshaping
    # we will have one row of NNs per sample point
    #
    NNs_loc_year <- Reduce(cbind, 
                           split(NNs_loc_year, 
                                 rep(1:n_neighbors, each=(nrow(NNs_loc_year)/n_neighbors)))) %>% 
                    data.table()

    NN_sigma <- Reduce(cbind, 
                       split(NN_sigma, 
                             rep(1:n_neighbors, each=(length(NN_sigma)/n_neighbors)))) %>% 
                data.table()
    # rename columns
    names(NN_dist) <- paste0("NN_", c(1:n_neighbors))
    names(NNs_loc_year) <- paste0(names(NNs_loc_year), paste0("_NN_", rep(1:n_neighbors, each=2)))
    names(NN_sigma) <- paste0("sigma_NN_", c(1:n_neighbors))

    NN_dist <- cbind(Yj[, c("year", "location")], NN_dist)
    NNs_loc_year <- cbind(Yj[, c("year", "location")], NNs_loc_year)
    NN_sigma <- cbind(Yj[, c("year", "location")], NN_sigma)
    
    NN_dist_tb <- rbind(NN_dist_tb, NN_dist)
    NN_sigma_tb <- rbind(NN_sigma_tb, NN_sigma)
    NNs_loc_year_tb <- rbind(NNs_loc_year_tb, NNs_loc_year)
    
    rm(NN_dist, NNs_loc_year, NN_sigma)
  }
  ########################################################################
  # For some bizzare reason, order of columns are random
  # when I run it on Aeolus. We fix the order of columns here
  ########################################################################
  distance_col_names <- c("year", "location", paste0("NN_", c(1:n_neighbors)))
  LL = (ncol(NNs_loc_year_tb)-2)/2
  v = rep(c("year_NN_", "location_NN_"), LL); w = rep(1:LL, each = 2);
  loc_year_col_names <- c("year", "location", paste0(v, w))
  
  sigma_df_col_names <- c("year", "location", paste0("sigma_NN_", c(1:n_neighbors)))
  
  setcolorder(NN_dist_tb, distance_col_names)
  setcolorder(NNs_loc_year_tb, loc_year_col_names)
  setcolorder(NN_sigma_tb, sigma_df_col_names)
  #############################################
  return(list(NN_dist_tb, NNs_loc_year_tb, NN_sigma_tb, 
              colnames(NN_dist_tb), colnames(NNs_loc_year_tb), colnames(NN_sigma_tb)))
}

find_NN_info_W4G_ICV <- function(ICV, historical_dt, future_dt, n_neighbors, precipitation=TRUE, gen3=TRUE){
  # This is modification of find_NN_info_W4G
  # where we add ICV matrix which in our case is 
  # the same as historical_dt, however, when we do averages,
  # then SD's will be NA for just one input!
  
  # remove extra columns
  if ("treatment" %in% colnames(historical_dt)) {historical_dt <- within(historical_dt, remove(treatment))}
  if ("treatment" %in% colnames(future_dt)) {future_dt <- within(future_dt, remove(treatment))}
  if ("treatment" %in% colnames(ICV)) {ICV <- within(ICV, remove(treatment))}

  if ("mean_escaped_Gen4" %in% colnames(historical_dt)) {historical_dt <- within(historical_dt, remove(mean_escaped_Gen4))}
  if ("mean_escaped_Gen4" %in% colnames(future_dt)) {future_dt <- within(future_dt, remove(mean_escaped_Gen4))}
  if ("mean_escaped_Gen4" %in% colnames(ICV)) {ICV <- within(ICV, remove(mean_escaped_Gen4))}

  # sort the columns of data tables so they both have the same order, if they do not.
  columns_ord <- c("year", "location", "ClimateScenario",
                   "medianDoY", "NumLarvaGens_Aug", 
                   "mean_escaped_Gen1", "mean_escaped_Gen2", "mean_escaped_Gen3",
                   "mean_gdd", "mean_precip")
  
  setcolorder(historical_dt, columns_ord)
  setcolorder(future_dt, columns_ord)
  setcolorder(ICV, columns_ord)

  all_us_locs <- unique(historical_dt$location)
    
  # 9 local locations are not in the all_us data!!!
  local_locations <- unique(future_dt$location)
  local_locations <- local_locations[which(local_locations %in% all_us_locs)] 
  future_dt <- future_dt %>% filter(location %in% local_locations)
  future_years <- unique(future_dt$year)
  
  trunc.SDs <- 0.1 # Principal component truncation rule

  non_numeric_cols <- c("year", "location", "ClimateScenario")

  # initiate empty outputs (to be generated!)
  NN_dist_tb <- data.table()
  NNs_loc_year_tb <- data.table()
  NN_sigma_tb <- data.table()

  if (precipitation==TRUE){
    if (gen3==TRUE){
      numeric_cols <- c("medianDoY", "NumLarvaGens_Aug", 
                        "mean_escaped_Gen1", "mean_escaped_Gen2", "mean_escaped_Gen3",
                        "mean_gdd", "mean_precip")
      } else {
        historical_dt <- within(historical_dt, remove(mean_escaped_Gen1, mean_escaped_Gen2, mean_escaped_Gen3))
        future_dt <- within(future_dt, remove(mean_escaped_Gen1, mean_escaped_Gen2, mean_escaped_Gen3))
        ICV <- within(ICV, remove(mean_escaped_Gen1, mean_escaped_Gen2, mean_escaped_Gen3))
        numeric_cols <- c("medianDoY", "NumLarvaGens_Aug", 
                          "mean_gdd", "mean_precip") 
    }
    
   } else if (precipitation==FALSE){
    historical_dt <- within(historical_dt, remove(mean_precip))
    future_dt <- within(future_dt, remove(mean_precip))
    ICV <- within(ICV, remove(mean_precip))
    
    if (gen3==TRUE){
      numeric_cols <- c("medianDoY", "NumLarvaGens_Aug", 
                        "mean_escaped_Gen1", "mean_escaped_Gen2", "mean_escaped_Gen3",
                        "mean_gdd")
      } else {
        historical_dt <- within(historical_dt, remove(mean_escaped_Gen1, mean_escaped_Gen2, mean_escaped_Gen3))
        future_dt <- within(future_dt, remove(mean_escaped_Gen1, mean_escaped_Gen2, mean_escaped_Gen3))
        ICV <- within(ICV, remove(mean_escaped_Gen1, mean_escaped_Gen2, mean_escaped_Gen3))
        numeric_cols <- c("medianDoY", "NumLarvaGens_Aug",
                          "mean_gdd") 
    }
  }
  A <- as.data.frame(historical_dt)
  B <- as.data.frame(future_dt)
  C <- as.data.frame(ICV)

  rm (historical_dt, future_dt, ICV)

  for (loc in local_locations){
    # loc = local_locations[1]
    Bj <- B %>% filter(location==loc)
    Cj <- C %>% filter(location==loc)

    # standard deviation of 1951-1990 interannual variability in each climate 
    # variable, ignoring missing years
    Cj.sd <- apply(Cj[, numeric_cols], MARGIN=2, FUN = sd, na.rm = T)
    Cj.sd[Cj.sd<(10^-10)] = 1

    A_prime <- A
    A_prime[, numeric_cols] <- sweep(A_prime[, numeric_cols], MARGIN=2, STATS=Cj.sd, FUN = `/`) 
    
    # standardize the analog pool
    Bj_prime <- Bj
    Bj_prime[, numeric_cols] <-sweep(Bj_prime[, numeric_cols], MARGIN=2, STATS=Cj.sd, FUN = `/`)

    # standardize the reference ICV
    Cj_prime <- Cj
    Cj_prime[, numeric_cols] <-sweep(Cj_prime[, numeric_cols], MARGIN=2, STATS=Cj.sd, FUN = `/`)

    ## Step 2: Extract the principal components (PCs) of 
    ##         the reference period ICV and project all data onto these PCs

    # Principal components analysis. The !is.na(apply(...)) 
    # term is there simply to select all years with complete observations in all variables. 
    #  ZZ[!is.na(apply(ZZ, 1, mean)) ,] selects the rows whose mean is not NaN
    PCA <- prcomp(Cj_prime[, numeric_cols][!is.na(apply(Cj_prime[, numeric_cols], 1, mean)) ,])

    # find the number of PCs to retain using the PC truncation
    # rule of eigenvector stdev > the truncation threshold
    PCs <- max(which(unlist(summary(PCA)[1])>trunc.SDs))

    # project the reference ICV onto the PCs which is the same as analog pool:
    X <- as.data.frame(predict(PCA, A_prime))
    X <- cbind(A_prime[, non_numeric_cols], X)

    # project the projected future conditions onto the PCs
    Yj <- as.data.frame(predict(PCA, Bj_prime[, numeric_cols]))
    Yj <- cbind(Bj_prime[, non_numeric_cols], Yj)

    Zj <- as.data.frame(predict(PCA, Cj_prime[, numeric_cols]))
    Zj <- cbind(Cj_prime[, non_numeric_cols], Zj)

    ## Step 3a: express PC scores as standardized
    #           anomalies of reference interannual variability
      
    # standard deviation of 1951-1990 interannual 
    # variability in each principal component, ignoring missing years
    Zj_sd <- apply(Zj[, 4:(PCs + 3)], MARGIN = 2, FUN = sd, na.rm=T)

    # standardize the analog pool   
    X_prime <- sweep(X[, 4:(PCs + 3)], MARGIN=2, Zj_sd, FUN = `/`)
    X_prime <- cbind(X[, non_numeric_cols], X_prime)

    # standardize the projected conditions
    Yj_prime <- sweep(Yj[, 4:(PCs + 3)], MARGIN=2, Zj_sd, FUN = `/`)
    Yj_prime <- cbind(Yj[, non_numeric_cols], Yj_prime)
    
    NN_list <- get.knnx(data = X_prime[, 4:(PCs+3)], 
                        query= Yj_prime[, 4:(PCs+3)], 
                        k=n_neighbors, algorithm="brute")
    #
    # Step 3: find sigma dissimilarities
    ############################################################
    #
    # This is for one location, and different years. 
    # So, the location column and ClimateScenario can be dropped.
    # We already know what they are from loc and the input file.
    #
    ############################################################
    # percentile of the nearest neighbour distance on the chi distribution with
    # degrees of freedom equaling the dimensionality of the distance measurement (PCs)
    NN_chi <- EnvStats::pchi(as.vector(NN_list$nn.dist), PCs)
    
    # values of the chi percentiles on a standard half-normal distribution
    # (chi distribution with one degree of freedom)
    # NN.sigma[which(proxy==j)] <- qchi(NN.chi, 1)

    NN_sigma <- EnvStats::qchi(NN_chi, 1)

    ########################################################################
    #
    # extract the rows corresponding to nearest neighbors indices
    # 
    ########################################################################
    NN_idx <- NN_list$nn.index
    NN_dist <- NN_list$nn.dist %>% data.table()
    
    NNs_loc_year <- X_prime[as.vector(NN_idx), c('year', 'location')]
    #
    # reshape the long list to wide data table
    # currenty the data are in vector form, by reshaping
    # we will have one row of NNs per sample point
    #
    NNs_loc_year <- Reduce(cbind, 
                           split(NNs_loc_year, 
                                 rep(1:n_neighbors, each=(nrow(NNs_loc_year)/n_neighbors)))) %>% 
                    data.table()

    NN_sigma <- Reduce(cbind, 
                       split(NN_sigma, 
                             rep(1:n_neighbors, each=(length(NN_sigma)/n_neighbors)))) %>% data.table()
    # rename columns
    names(NN_dist) <- paste0("NN_", c(1:n_neighbors))
    names(NNs_loc_year) <- paste0(names(NNs_loc_year), paste0("_NN_", rep(1:n_neighbors, each=2)))
    names(NN_sigma) <- paste0("sigma_NN_", c(1:n_neighbors))

    NN_dist <- cbind(Yj[, c("year", "location")], NN_dist)
    NNs_loc_year <- cbind(Yj[, c("year", "location")], NNs_loc_year)
    NN_sigma <- cbind(Yj[, c("year", "location")], NN_sigma)
    
    NN_dist_tb <- rbind(NN_dist_tb, NN_dist)
    NN_sigma_tb <- rbind(NN_sigma_tb, NN_sigma)
    NNs_loc_year_tb <- rbind(NNs_loc_year_tb, NNs_loc_year)
    
    rm(NN_dist, NNs_loc_year, NN_sigma)
  }
  ########################################################################
  # For some bizzare reason, order of columns are random
  # when I run it on Aeolus. We fix the order of columns here
  ########################################################################
  distance_col_names <- c("year", "location", paste0("NN_", c(1:n_neighbors)))
  LL = (ncol(NNs_loc_year_tb)-2)/2
  v = rep(c("year_NN_", "location_NN_"), LL); w = rep(1:LL, each = 2);
  loc_year_col_names <- c("year", "location", paste0(v, w))
  
  sigma_df_col_names <- c("year", "location", paste0("sigma_NN_", c(1:n_neighbors)))
  
  setcolorder(NN_dist_tb, distance_col_names)
  setcolorder(NNs_loc_year_tb, loc_year_col_names)
  setcolorder(NN_sigma_tb, sigma_df_col_names)
  #############################################
  return(list(NN_dist_tb, NNs_loc_year_tb, NN_sigma_tb, 
              colnames(NN_dist_tb), colnames(NNs_loc_year_tb), colnames(NN_sigma_tb)))
}
####################################################################################

####################################################################################
####################################################################################
####################################################################################
####################################################################################

########################################################################
#
#                                 MatchIt
# 
########################################################################
sort_matchit_out <- function(base, usa, m_method, m_distance, m_ratio, precip){
  if (dim(base)[2] != dim(usa)[2]){
    print ("The number of columns do not agree!")
    stopifnot(dim(base)[2] == dim(usa)[2])
   } else {
    binded_dt <- rbind(usa, base)
  }
  # add row numbers to data, so we can check
  # the order of output we get is correct
  # binded_dt$row_ID = seq(1, dim(binded_dt)[1], 1)

  m_out <- list_by_dist_1_to_all(binded_dt, m_method, m_distance, m_ratio, precip)
  
  if (m_method=="nearest"){
    matched_rows <- as.integer(m_out$match.matrix)
    matched_frame <- binded_dt[matched_rows, ]
    matched_frame <- rbind(matched_frame, base)
  }
  return(matched_frame)
}

list_by_dist_1_to_all <- function(binded, m_method, m_distance = "default", m_ratio = 500, precip = FALSE){
  # List the historical locations
  # according to increasing distance 
  # from a given locaion data in a certain year: base
  #
  # inputs: 
  #          base: a row vector of size 1-by-n_vars of future data, one location, one year
  #          usa: matrix of all observed data across USA.
  #          m_method: Matching method: could be exact, subclass, nearest ~= optimal, full
  #                    suggestion: NN, full, optimal
  #          m_option: When you choose nearest then option can be set to subclass.
  #          m_distance: distance metric:logit
  #          precipitation: TRUE or FALSE (include it or exclude it)
  #
  # output:  a matchit object
  if (m_method == "nearest"){
    if (precip == TRUE){
      if (m_distance != "default"){
        match_out <- matchit(formula = treatment ~ medianDoY + NumLarvaGens_Aug + 
                                                   mean_escaped_Gen1 + mean_escaped_Gen2 + 
                                                   mean_escaped_Gen3 + mean_escaped_Gen4 +
                                                   mean_gdd + mean_precip,
                              method=m_method, 
                              ratio = m_ratio, 
                              data = binded, 
                              distance = m_distance)
        
        } else {
          match_out <- matchit(formula = treatment ~ medianDoY + NumLarvaGens_Aug + 
                                                     mean_escaped_Gen1 + mean_escaped_Gen2 + 
                                                     mean_escaped_Gen3 + mean_escaped_Gen4 +
                                                     mean_gdd + mean_precip,
                              method=m_method, 
                              ratio = m_ratio, 
                              data = binded)
      }
     } else {
      if (m_distance != "default"){
        match_out <- matchit(formula = treatment ~ medianDoY + NumLarvaGens_Aug + 
                                                   mean_escaped_Gen1 + mean_escaped_Gen2 + 
                                                   mean_escaped_Gen3 + mean_escaped_Gen4 +
                                                   mean_gdd,
                              method=m_method, 
                              ratio = m_ratio, 
                              data = binded,
                              distance = m_distance)
        
        } else {
          match_out <- matchit(formula = treatment ~ medianDoY + NumLarvaGens_Aug + 
                                                     mean_escaped_Gen1 + mean_escaped_Gen2 + 
                                                     mean_escaped_Gen3 + mean_escaped_Gen4 +
                                                     mean_gdd,
                              method=m_method, 
                              ratio = m_ratio, 
                              data = binded)
      }
    }
  }

  if (m_method == "full"){
    if (precip == TRUE){
      if (m_distance != "default"){
        match_out <- matchit(formula = treatment ~ medianDoY + NumLarvaGens_Aug + 
                                                   mean_escaped_Gen1 + mean_escaped_Gen2 + 
                                                   mean_escaped_Gen3 + mean_escaped_Gen4 +
                                                   mean_gdd + mean_precip,
                              method=m_method, 
                              min.controls = m_ratio, 
                              data = binded, 
                              distance = m_distance)
        
        } else {
          match_out <- matchit(formula = treatment ~ medianDoY + NumLarvaGens_Aug + 
                                                     mean_escaped_Gen1 + mean_escaped_Gen2 + 
                                                     mean_escaped_Gen3 + mean_escaped_Gen4 +
                                                     mean_gdd + mean_precip,
                              method=m_method, 
                              min.controls = m_ratio, 
                              data = binded)
      }
     } else {
      if (m_distance != "default"){
        match_out <- matchit(formula = treatment ~ medianDoY + NumLarvaGens_Aug + 
                                                   mean_escaped_Gen1 + mean_escaped_Gen2 + 
                                                   mean_escaped_Gen3 + mean_escaped_Gen4 +
                                                   mean_gdd,
                              method=m_method, 
                              min.controls = m_ratio, 
                              data = binded,
                              distance = m_distance)
        
        } else {
          match_out <- matchit(formula = treatment ~ medianDoY + NumLarvaGens_Aug + 
                                                     mean_escaped_Gen1 + mean_escaped_Gen2 + 
                                                     mean_escaped_Gen3 + mean_escaped_Gen4 +
                                                     mean_gdd,
                              method=m_method, 
                              min.controls = m_ratio, 
                              data = binded)
      }
    }
  }
  return(match_out)
}

####################################################################
##                                                                ##
##            Read the data off the laptop                        ##
##            and clean them so they are ready                    ##
##            to be used to produce features.                     ##
##                                                                ##
####################################################################

# create number of generations by Aug 23, and Nov 5th
# the data is on the computer for this.
generate_no_generations <- function(input_dir, file_name, stage="Larva", dead_line="Aug", version){
  file_name <- paste0(input_dir, file_name)
  data <- data.table(readRDS(file_name))
  if (stage == "Larva"){
      var = "NumLarvaGens"
     } else if (stage == "Adult"){
      var = "NumAdultGens"
  }
  ############################################
  ##
  ## Clean data, we just need future data 
  ## for local stuff.
  ##
  ############################################
  # some how there is no 2025 in the data!
  data <- clean_gens_files(data, var)
  colnames(data)[colnames(data) == var] <- paste0(var, "_", dead_line)
  return(data)
}

clean_gens_files <- function(data, var){
  # remove historical data
  data <- data %>% filter(year >= 2025 | year <= 2015)
  
  # remove historical level stuck in ClimateScenario
  data$ClimateScenario = factor(data$ClimateScenario)
    
  data$location = paste0(data$latitude, "_", data$longitude)
    
  data <- subset(data, select = c("year", "location", var, "ClimateScenario"))
  data = unique(data)
  return(data)
}
######################################################################
##                                                                    ##
##                 Generate the following features:                   ##
##                     1. Median DoY (First Flight)                   ##
##                     2. Pest risk for Gen 3/4, for 25/75 %          ##
##                                  (not any more)                    ##
##                     3. Fraction of escaped diapause for each Gen.  ##
##                     4. No. Generations.                            ##
##                     5. GDD accumulation.                           ##
##                     6. Precipitation.                              ##
######################################################################

#######
####### First Flight
#######
generate_mDoY_FF <- function(dt, meann = TRUE){  
  mDoY_FF <- clean_4_FF(dt)
  # mDoY_FF <- within(mDoY_FF, remove(ClimateScenario))
  if (meann ==TRUE) {
      mDoY_FF = mDoY_FF[, .(medianDoY = as.integer(median(emergence))),
                          by = c("year", "location")]
   } else {
       mDoY_FF = mDoY_FF[, .(medianDoY = as.integer(median(emergence))),
                        by = c("year", "location", "ClimateScenario")]
  }
  return(mDoY_FF)
}

clean_4_FF <- function(dt){
  if ("location" %in% colnames(dt)){
    print ("Hello there! from line 131 core of analog")
    } else {
      dt$location <- paste0(dt$latitude, "_", dt$longitude)
  }

  need_cols <- c("year", "location", "ClimateScenario", "Emergence")
  sub_Emerg <- subset(dt, !is.na("Emergence"), select = need_cols)
  sub_Emerg$ClimateScenario <- factor(sub_Emerg$ClimateScenario)
  colnames(sub_Emerg)[colnames(sub_Emerg) == "Emergence"] <- paste0("emergence")
  return(sub_Emerg)
}

###############################################################
######                                                   ######
######      Generate data for escaped diapause stuff     ######
######                                                   ######
###############################################################
gen_diap_map1_4_analog_Rel <- function(sub1, param_dir, time_type, CodMothParams_name){
  CodMothParams <- read.table(paste0(param_dir, CodMothParams_name), header=TRUE, sep=",", as.is=T)
  group_vec = c("latitude", "longitude", "ClimateScenario", "year")

  sub2 = sub1[, .(RelPctDiap=(auc(CumulativeDDF, RelDiap)/auc(CumulativeDDF,RelLarvaPop))*100, 
                  RelPctNonDiap = (auc(CumulativeDDF, RelNonDiap)/auc(CumulativeDDF, RelLarvaPop))*100), 
                  by=group_vec]

  sub2=merge(sub2, sub1[CumulativeDDF>=CodMothParams[5,5]&CumulativeDDF<CodMothParams[5,6],.(RelPctDiapGen1=(auc(CumulativeDDF,RelDiap)/auc(CumulativeDDF,RelLarvaPop))*100),by=group_vec],by=group_vec,all.x=T)
  sub2=merge(sub2, sub1[CumulativeDDF>=CodMothParams[6,5]&CumulativeDDF<CodMothParams[6,6],.(RelPctDiapGen2=(auc(CumulativeDDF,RelDiap)/auc(CumulativeDDF,RelLarvaPop))*100),by=group_vec],by=group_vec,all.x=T)
  sub2=merge(sub2, sub1[CumulativeDDF>=CodMothParams[7,5]&CumulativeDDF<CodMothParams[7,6],.(RelPctDiapGen3=(auc(CumulativeDDF,RelDiap)/auc(CumulativeDDF,RelLarvaPop))*100),by=group_vec],by=group_vec,all.x=T)
  sub2=merge(sub2, sub1[CumulativeDDF>=CodMothParams[8,5]&CumulativeDDF<CodMothParams[8,6],.(RelPctDiapGen4=(auc(CumulativeDDF,RelDiap)/auc(CumulativeDDF,RelLarvaPop))*100),by=group_vec],by=group_vec,all.x=T)
  sub2=merge(sub2, sub1[CumulativeDDF>=CodMothParams[5,5]&CumulativeDDF<CodMothParams[5,6],.(RelPctNonDiapGen1=(auc(CumulativeDDF,RelNonDiap)/auc(CumulativeDDF,RelLarvaPop))*100),by=group_vec],by=group_vec,all.x=T)
  sub2=merge(sub2, sub1[CumulativeDDF>=CodMothParams[6,5]&CumulativeDDF<CodMothParams[6,6],.(RelPctNonDiapGen2=(auc(CumulativeDDF,RelNonDiap)/auc(CumulativeDDF,RelLarvaPop))*100),by=group_vec],by=group_vec,all.x=T)
  sub2=merge(sub2, sub1[CumulativeDDF>=CodMothParams[7,5]&CumulativeDDF<CodMothParams[7,6],.(RelPctNonDiapGen3=(auc(CumulativeDDF,RelNonDiap)/auc(CumulativeDDF,RelLarvaPop))*100),by=group_vec],by=group_vec,all.x=T)
  sub2=merge(sub2, sub1[CumulativeDDF>=CodMothParams[8,5]&CumulativeDDF<CodMothParams[8,6],.(RelPctNonDiapGen4=(auc(CumulativeDDF,RelNonDiap)/auc(CumulativeDDF,RelLarvaPop))*100),by=group_vec],by=group_vec,all.x=T)
  
  # double check, but with high certaintly we do not need the first four lines above!
  sub2 <- within(sub2, remove("RelPctDiapGen1", "RelPctDiapGen2", "RelPctDiapGen3", "RelPctDiapGen4"))
  sub2$location <- paste0(sub2$latitude, "_", sub2$longitude)
  sub2 <- within(sub2, remove("latitude", "longitude"))

  sub2 <- subset(sub2, select=c("location", "year", 
                                "RelPctNonDiapGen1", "RelPctNonDiapGen2",
                                "RelPctNonDiapGen3", "RelPctNonDiapGen4"))
  return (sub2)
}

diap_map1_prep_4_analog_Rel <- function(input_dir, file_name, param_dir,  time_type){
  file_N = paste0(input_dir, file_name, ".rds")
  data <- data.table(readRDS(file_N))
  print (paste0("line 624"))
  theta = 0.2163108 + (2 * atan(0.9671396 * tan(0.00860 * (data$dayofyear - 186))))
  phi = asin(0.39795 * cos(theta))
  D = 24 - ((24/pi) * acos((sin(6 * pi / 180) + 
                    (sin(data$latitude * pi / 180) * sin(phi)))/(cos(data$latitude * pi / 180) * cos(phi))))
  data$daylength = D

  data$diapause = 102.6077 * exp(-exp(-(-1.306483) * (data$daylength - 16.95815)))
  data$diapause1 = data$diapause
  data[diapause1 > 100, diapause1 := 100]
  data$enterDiap = (data$diapause1/100) * data$SumLarva
  data$escapeDiap = data$SumLarva - data$enterDiap

  sub = data
  rm(data)
  startingpopulationfortheyear <- 1000

  # Gen 1
  sub[, LarvaGen1RelFraction := LarvaGen1/sum(LarvaGen1), 
        by =list(year, ClimateScenario, latitude, longitude) ]
 
  # Gen 2
  sub[, LarvaGen2RelFraction := LarvaGen2/sum(LarvaGen2), 
        by = list(year, ClimateScenario, latitude, longitude)]

  # Gen 3
  sub[, LarvaGen3RelFraction := LarvaGen3/sum(LarvaGen3), 
        by =list(year,ClimateScenario, latitude, longitude)]

  # Gen 4
  sub[, LarvaGen4RelFraction := LarvaGen4/sum(LarvaGen4), 
        by =list(year, ClimateScenario, latitude, longitude)]

  sub = subset(sub, select = c("latitude", "longitude", 
                               "ClimateScenario",
                               "year", "dayofyear", "CumDDinF", 
                               "SumLarva", "enterDiap", 
                               "escapeDiap"))

  sub = sub[, .(RelLarvaPop = mean(SumLarva), 
                  RelDiap = mean(enterDiap), 
                RelNonDiap = mean(escapeDiap), 
                CumulativeDDF = mean(CumDDinF)), 
             by = c("ClimateScenario",
                    "latitude", "longitude", 
                    "dayofyear", "year")]

  return (sub)
}
################################################################################
extract_gdd <- function(in_dir, file_name){
  dt <- data.table(readRDS(paste0(in_dir, file_name)))
  print ("line 247 of core, in extract_gdd functions")
  print (colnames(dt))
  if ("location" %in% colnames(dt)){
    print ("Hello there")
    } else {
    dt$location = paste0(dt$latitude, "_", dt$longitude)
  }

  # extract locations and years and the last day of the year
  # which has the last GDD
  dt <- dt %>% 
        group_by(location, year, ClimateScenario) %>%
        filter(month==12 & day==31) %>%
        data.table()
  
  dt <- subset(dt, select=c("year", "location", "CumDDinF", "ClimateScenario"))
  dt$ClimateScenario <- factor(dt$ClimateScenario) # get rid of historical level!
  return(dt)
}

###############################################################
###############################################################
######                                                   ######
######               combine precip data                 ######
######                                                   ######
###############################################################
merge_precip <- function(main_in_dir, location_type){
  if (location_type == "local"){
    models = c("bcc-csm1-1-m", "BNU-ESM", "CanESM2", "CNRM-CM5", "GFDL-ESM2G", "GFDL-ESM2M")
    carbon_types = c("rcp45", "rcp85")
    all_data_45 <- data.table()
    all_data_85 <- data.table()
    for (carbon in carbon_types){
      all_data <- data.table()
      for (model in models){
        curr_path = paste0(main_in_dir, model, "/", carbon, "/")
        # list of files in current folder
        files = list.files(path = curr_path, pattern = "data_")
        for (file in files){
          dt <- data.table(readRDS(paste0(curr_path, file)))
          dt <- dt %>% filter(year >= 2026 & year <= 2095)
          location = gsub("data_", "", file)
          location = gsub(".rds", "", location)
          dt$location <- location
          dt$ClimateScenario <- model
          all_data = rbind(all_data, dt)
        }
      }
    if (carbon == "rcp45"){ all_data_45 <- all_data } else if (carbon == "rcp85") { all_data_85 <- all_data}
    }
    return(list(all_data_45, all_data_85))
   } else if (location_type == "usa"){
    
    all_data = data.table()
    # list of files in current folder
    files = list.files(path = main_in_dir, pattern = "data_")
    
    for (file in files){
      dt <- data.table(readRDS(paste0(main_in_dir, file)))
      location = gsub("data_", "", file)
      location = gsub(".rds", "", location)
      dt$location <- location
      all_data = rbind(all_data, dt)
    }
    all_data$ClimateScenario = "observed"
    return (all_data)
  }
}

###############################################################
######                                                   ######
######                                                   ######
######                                                   ######
###############################################################

make_unique <- function(input_dir, param_dir, location_group_name){
    # This fukcing function is created because neither unique, nor !duplicate,
    # could work! So, we first separate the fucking data
    # then bind it together with another function.
    # then compute diapause stuff
    #
    # loc_grp = data.table(read.csv(paste0(param_dir, location_group_name)))
    # loc_grp$location = paste0(loc_grp$latitude, "_", loc_grp$longitude)
    # loc_grp = within(loc_grp, remove(latitude, longitude))
    #
    # 
    # As of April 4th, the data in /data/hydro/users/Hossein/codling_moth_new/local/processed are
    # already unique and non overlapping. The overlapping data are in 
    # /data/hydro/users/Hossein/codling_moth_new/local/processed/overlaping/
    # So, ... This fucking function would not work on those
    # because time periods/ClimateGroup is changed to 2025-2050. 
    
    ########## RCP45 - 2040

    file_n = "combined_CMPOP_rcp45.rds"
    file_name = paste0(input_dir, file_n)
    data <- data.table(readRDS(file_name))
    data = data %>% filter(ClimateGroup == "2040's")
    data <- within(data, remove(CountyGroup, DailyDD,
                                AdultGen1, AdultGen2, AdultGen3, AdultGen4,
                                PercAdult, PercAdultGen1, PercAdultGen2,
                                PercAdultGen3, PercAdultGen4, PercEgg,
                                PercLarva, PercLarvaGen1, PercLarvaGen2, 
                                PercLarvaGen3, PercLarvaGen4, PercPupa,
                                SumAdult, SumEgg, SumPupa, CumDDinC
                                ))
    print (sort(colnames(data)))
    data$location = paste0(data$latitude, "_", data$longitude)
    # data <- data %>% filter(location %in% loc_grp$location)
    data$latitude = as.numeric(data$latitude)
    data$longitude = as.numeric(data$longitude)
    data = within(data, remove(ClimateGroup))
    out_dir = "/data/hydro/users/Hossein/analog/local/data_bases/"
    saveRDS(data, paste0(out_dir, "CMPOP_2040_rcp45.rds"))

    ########## RCP45 - 2060
    file_n = "combined_CMPOP_rcp45.rds"
    file_name = paste0(input_dir, file_n)
    data <- data.table(readRDS(file_name))
    data <- data %>% filter(ClimateGroup == "2060's")
    data <- data %>% filter(year >= 2056 & year <= 2065)

    data <- within(data, remove(CountyGroup, DailyDD, 
                                AdultGen1, AdultGen2, AdultGen3, AdultGen4,
                                PercAdult, PercAdultGen1, PercAdultGen2,
                                PercAdultGen3, PercAdultGen4, PercEgg,
                                PercLarva, PercLarvaGen1, PercLarvaGen2, 
                                PercLarvaGen3, PercLarvaGen4, PercPupa,
                                SumAdult, SumEgg, SumPupa, CumDDinC
                                ))

    data$location = paste0(data$latitude, "_", data$longitude)
    # data <- data %>% filter(location %in% loc_grp$location)
    data$latitude = as.numeric(data$latitude)
    data$longitude = as.numeric(data$longitude)

    data = within(data, remove(ClimateGroup))
    out_dir = "/data/hydro/users/Hossein/analog/local/data_bases/"
    saveRDS(data, paste0(out_dir, "CMPOP_2060_rcp45.rds"))

    ########## RCP45 - 2080
    file_n = "combined_CMPOP_rcp45.rds"
    file_name = paste0(input_dir, file_n)
    data <- data.table(readRDS(file_name))
    data <- data %>% filter(ClimateGroup == "2080's")
    data <- within(data, remove(CountyGroup, DailyDD, 
                                AdultGen1, AdultGen2, AdultGen3, AdultGen4,
                                PercAdult, PercAdultGen1, PercAdultGen2,
                                PercAdultGen3, PercAdultGen4, PercEgg,
                                PercLarva, PercLarvaGen1, PercLarvaGen2, 
                                PercLarvaGen3, PercLarvaGen4, PercPupa,
                                SumAdult, SumEgg, SumPupa, CumDDinC
                                ))
    data = within(data, remove(ClimateGroup))

    data$location = paste0(data$latitude, "_", data$longitude)
    # data <- data %>% filter(location %in% loc_grp$location)
    data$latitude = as.numeric(data$latitude)
    data$longitude = as.numeric(data$longitude)
    out_dir = "/data/hydro/users/Hossein/analog/local/data_bases/"
    saveRDS(data, paste0(out_dir, "CMPOP_2080_rcp45.rds"))
    
    ##########
    ########## rcp85
    ##########

    ########## rcp85 - 2040

    file_n = "combined_CMPOP_rcp85.rds"
    file_name = paste0(input_dir, file_n)
    data <- data.table(readRDS(file_name))
    data = data %>% filter(ClimateGroup == "2040's")
    data <- within(data, remove(CountyGroup, DailyDD, 
                                AdultGen1, AdultGen2, AdultGen3, AdultGen4,
                                PercAdult, PercAdultGen1, PercAdultGen2,
                                PercAdultGen3, PercAdultGen4, PercEgg,
                                PercLarva, PercLarvaGen1, PercLarvaGen2, 
                                PercLarvaGen3, PercLarvaGen4, PercPupa,
                                SumAdult, SumEgg, SumPupa, CumDDinC
                                ))

    data$location = paste0(data$latitude, "_", data$longitude)
    # data <- data %>% filter(location %in% loc_grp$location)
    data$latitude = as.numeric(data$latitude)
    data$longitude = as.numeric(data$longitude)
    data = within(data, remove(ClimateGroup))
    out_dir = "/data/hydro/users/Hossein/analog/local/data_bases/"
    saveRDS(data, paste0(out_dir, "CMPOP_2040_rcp85.rds"))

    ########## rcp85 - 2060
    print (paste0("line 125 of core ", dim(data)))
    file_n = "combined_CMPOP_rcp85.rds"
    file_name = paste0(input_dir, file_n)
    data <- data.table(readRDS(file_name))
    data <- data %>% filter(ClimateGroup == "2060's")
    data <- data %>% filter(year >= 2056 & year <= 2065)

    data <- within(data, remove(CountyGroup, DailyDD,
                                AdultGen1, AdultGen2, AdultGen3, AdultGen4,
                                PercAdult, PercAdultGen1, PercAdultGen2,
                                PercAdultGen3, PercAdultGen4, PercEgg,
                                PercLarva, PercLarvaGen1, PercLarvaGen2, 
                                PercLarvaGen3, PercLarvaGen4, PercPupa,
                                SumAdult, SumEgg, SumPupa, CumDDinC
                                ))

    data$location = paste0(data$latitude, "_", data$longitude)
    # data <- data %>% filter(location %in% loc_grp$location)
    data$latitude = as.numeric(data$latitude)
    data$longitude = as.numeric(data$longitude)
    
    data = within(data, remove(ClimateGroup))
    out_dir = "/data/hydro/users/Hossein/analog/local/data_bases/"
    saveRDS(data, paste0(out_dir, "CMPOP_2060_rcp85.rds"))

    ########## rcp85 - 2080
    file_n = "combined_CMPOP_rcp85.rds"
    file_name = paste0(input_dir, file_n)
    data <- data.table(readRDS(file_name))
    data <- data %>% filter(ClimateGroup == "2080's")
    data <- within(data, remove(CountyGroup, DailyDD, 
                                AdultGen1, AdultGen2, AdultGen3, AdultGen4,
                                PercAdult, PercAdultGen1, PercAdultGen2,
                                PercAdultGen3, PercAdultGen4, PercEgg,
                                PercLarva, PercLarvaGen1, PercLarvaGen2, 
                                PercLarvaGen3, PercLarvaGen4, PercPupa,
                                SumAdult, SumEgg, SumPupa, CumDDinC
                                ))
    data = within(data, remove(ClimateGroup))

    data$location = paste0(data$latitude, "_", data$longitude)
    # data <- data %>% filter(location %in% loc_grp$location)
    data$latitude = as.numeric(data$latitude)
    data$longitude = as.numeric(data$longitude)
    # data$latitude = as.numeric(substr(x = data$location, start = 1, stop = 8))
    # data$latitude = as.numeric(sapply(strsplit(data$location, "_"), function(x) x[1]))
    # data$longitude= as.numeric(sapply(strsplit(data$location, "_"), function(x) x[2]))
    
    out_dir = "/data/hydro/users/Hossein/analog/local/data_bases/"
    saveRDS(data, paste0(out_dir, "CMPOP_2080_rcp85.rds"))
}

generate_short_CM_files <- function(in_dir, out_dir){
  ## 
  ## This function just sebsets the useful columns.
  ##
  # in_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/my_aeolus_2015/all_local/analog/"
  # out_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/my_aeolus_2015/all_local/analog/"
  file_names = c("combined_CM_rcp45.rds", "combined_CM_rcp85.rds")
  for (file in file_names){
      data <- data.table(readRDS(paste0(in_dir, file)))
      data <- data.table(readRDS(paste0(in_dir, file)))
      data <- subset(data, select = c("year", "location", "ClimateScenario",
                                      "Emergence",
                                      "LGen1_0.25", "LGen1_0.5", "LGen1_0.75",
                                      "LGen2_0.25", "LGen2_0.5", "LGen2_0.75",
                                      "LGen3_0.25", "LGen3_0.5", "LGen3_0.75",
                                      "LGen4_0.25", "LGen4_0.5", "LGen4_0.75",
                                      "AGen1_0.25", "AGen1_0.5", "AGen1_0.75",
                                      "AGen2_0.25", "AGen2_0.5", "AGen2_0.75",
                                      "AGen3_0.25", "AGen3_0.5", "AGen3_0.75",
                                      "AGen4_0.25", "AGen4_0.5", "AGen4_0.75"))
      
      saveRDS(data, paste0(out_dir, "short_", file))
      rm(data)
  }
}

generate_CM_files <- function(in_dir, out_dir){
  # in_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/my_aeolus_2015/all_local/"
  # out_dir = "/Users/hn/Desktop/Desktop/Kirti/check_point/my_aeolus_2015/all_local/analog/"
  file_names = c("combined_CM_rcp45.rds", "combined_CM_rcp85.rds")
  for (file in file_names){
    data <- data.table(readRDS(paste0(in_dir, file)))
    data <- data %>% filter(year >= 2025)
    data$location = paste0(data$latitude, "_", data$longitude)
    data <- within(data, remove(longitude, latitude))
    data <- within(data, remove(CountyGroup, ClimateGroup))
    data = unique(data)
    saveRDS(data, paste0(out_dir, file))
    rm(data)
  }
}
###############################################################
######                                                   ######
######               Read the binary data                ######
######                                                   ######
###############################################################

read_binary <- function(file_path, hist, no_vars){
  if (hist) {
    if (no_vars==4){
      start_year <- 1950
      end_year <- 2005
      } else {
        start_year <- 1979
        end_year <- 2015
      }
  } else{
    start_year <- 2006
    end_year <- 2099
  }
  ymd_file <- create_ymdvalues(start_year, end_year)
  data <- read_binary_addmdy(file_path, ymd_file, no_vars)
  return(data)
}

read_binary_addmdy <- function(filename, ymd, no_vars){
  if (no_vars==4){
      return(read_binary_addmdy_4var(filename, ymd))
  } else {return(read_binary_addmdy_8var(filename, ymd))}
}

read_binary_addmdy_8var <- function(filename, ymd){
  Nofvariables <- 8 # number of variables or column in the forcing data file
  Nrecords <- nrow(ymd)
  ind <- seq(1, Nrecords * Nofvariables, Nofvariables)
  fileCon  <-  file(filename, "rb")
  temp <- readBin(fileCon, integer(), size = 2, n = Nrecords * Nofvariables,
                  endian = "little")
  dataM <- matrix(0, Nrecords, 8)
  k <- 1
  dataM[1:Nrecords, 1] <- temp[ind] / 40.00         # precip data
  dataM[1:Nrecords, 2] <- temp[ind + 1] / 100.00    # Max temperature data
  dataM[1:Nrecords, 3] <- temp[ind + 2] / 100.00    # Min temperature data
  dataM[1:Nrecords, 4] <- temp[ind + 3] / 100.00    # Wind speed data
  dataM[1:Nrecords, 5] <- temp[ind + 4] / 10000.00  # SPH
  dataM[1:Nrecords, 6] <- temp[ind + 5] / 40.00     # SRAD
  dataM[1:Nrecords, 7] <- temp[ind + 6] / 100.00    # Rmax
  dataM[1:Nrecords, 8] <- temp[ind + 7] / 100.00    # RMin
  AllData <- cbind(ymd, dataM)
  # calculate daily GDD  ...what? There doesn't appear to be any GDD work?
  colnames(AllData) <- c("year", "month", "day", "precip", "tmax", "tmin",
                         "windspeed", "SPH", "SRAD", "Rmax", "Rmin")
  close(fileCon)
  return(AllData)
}

read_binary_addmdy_4var <- function(filename, ymd) {
  Nofvariables <- 4 # number of variables or column in the forcing data file
  Nrecords <- nrow(ymd)
  ind <- seq(1, Nrecords * Nofvariables, Nofvariables)
  fileCon <-  file(filename, "rb")
  temp <- readBin(fileCon, integer(), size = 2, n = Nrecords * Nofvariables,
                  endian="little")
  dataM <- matrix(0, Nrecords, 4)
  k <- 1
  dataM[1:Nrecords, 1] <- temp[ind] / 40.00       # precip data
  dataM[1:Nrecords, 2] <- temp[ind + 1] / 100.00  # Max temperature data
  dataM[1:Nrecords, 3] <- temp[ind + 2] / 100.00  # Min temperature data
  dataM[1:Nrecords, 4] <- temp[ind + 3] / 100.00  # Wind speed data

  AllData <- cbind(ymd, dataM)
  # calculate daily GDD  ...what? There doesn't appear to be any GDD work?
  colnames(AllData) <- c("year", "month", "day", "precip", "tmax", "tmin",
                         "windspeed")
  close(fileCon)
  return(AllData)
}

create_ymdvalues <- function(data_start_year, data_end_year){
  Years <- seq(data_start_year, data_end_year)
  nYears <- length(Years)
  daycount_in_year <- 0
  moncount_in_year <- 0
  yearrep_in_year <- 0
    
  for (i in 1:nYears){
    ly <- leap_year(Years[i])
    if (ly == TRUE){
        days_in_mon <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
       } else {
        days_in_mon <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    }

    for (j in 1:12){
      daycount_in_year <- c(daycount_in_year, seq(1, days_in_mon[j]))
      moncount_in_year <- c(moncount_in_year, rep(j, days_in_mon[j]))
      yearrep_in_year <- c(yearrep_in_year, rep(Years[i], days_in_mon[j]))
    }
  }

  daycount_in_year <- daycount_in_year[-1] #delete the leading 0
  moncount_in_year <- moncount_in_year[-1]
  yearrep_in_year <- yearrep_in_year[-1]
  ymd <- cbind(yearrep_in_year, moncount_in_year, daycount_in_year)
  colnames(ymd) <- c("year", "month", "day")
  return(ymd)
}

get_county <- function(input_fill_add){
  dt <- data.table(readRDS(input_fill_add))
  dt <- subset(dt, select = c("latitude", "longitude", "County"))
  return(dt)
}




